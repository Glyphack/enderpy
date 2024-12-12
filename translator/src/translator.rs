use std::sync::Arc;
use std::collections::HashMap;

use enderpy_python_parser::ast::{self, *};
use enderpy_python_parser::parser::parser::intern_lookup;

use enderpy_python_type_checker::{types, ast_visitor::TraversalVisitor, file::EnderpyFile, checker::TypeChecker, types::PythonType};
use enderpy_python_type_checker::{get_module_name, symbol_table};

#[derive(Clone, Debug)]
pub struct CppTranslator<'a> {
    pub output: String,
    indent_level: usize,
    checker: Arc<TypeChecker<'a>>,
    file: &'a EnderpyFile,
    current_scope: u32,
    prev_scope: u32,
    // Member variables of the current class, map from name to type
    class_members: HashMap<String, String>,
    // Whether we are currently inside a __init__ method
    // and therefore need to record member variables
    in_constructor: bool,
}

impl<'a> CppTranslator<'a> {
    pub fn new(checker: Arc<TypeChecker<'a>>, file: &'a EnderpyFile) -> Self {
        CppTranslator {
            output: "".to_string(),
            indent_level: 0,
            checker: checker,
            file: file,
            current_scope: 0,
            prev_scope: 0,
            class_members: HashMap::new(),
            in_constructor: false,
        }
    }

    pub fn translate(&mut self) {
        for stmt in self.file.tree.body.iter() {
            self.visit_stmt(stmt);
        }
    }

    fn emit<S: AsRef<str>>(&mut self, s: S) {
        self.output += s.as_ref();
    }

    fn emit_indent(&mut self) {
        self.emit("  ".repeat(self.indent_level));
    }

    fn emit_type(&mut self, node: &ast::Node) {
        self.emit(self.get_cpp_type(node));
    }

    fn get_cpp_type(&self, node: &ast::Node) -> String {
        let typ = self.checker.get_type(node);
        return self.python_type_to_cpp(&typ);
    }

    fn check_type(&self, node: &Node, typ: &PythonType) {
        assert!(
            self.checker.get_type(node) == *typ,
            "type error at {}, expected {} got {}",
            self.file.get_position(node.start, node.end),
            typ, self.checker.get_type(node)
        );
    }

    fn enter_scope(&mut self, pos: u32) {
        let symbol_table = self.checker.get_symbol_table(None);
        self.prev_scope = self.current_scope;
        self.current_scope = symbol_table.get_scope(pos);
    }

    fn leave_scope(&mut self) {
        self.current_scope = self.prev_scope;
    }

    fn python_type_to_cpp(&self, python_type: &PythonType) -> String {
        let details;
        match python_type {
            PythonType::Class(c) => {
                details = &c.details;
            },
            PythonType::Instance(i) => {
                details = &i.class_type.details;
            },
            _ => {
                return String::from(format!("<unknown type {}>", python_type));
            }
        };
        // If the current symbol table already contains details.name,
        // we do not need to qualify it, otherwise qualify it with the module name,
        // unless it is a builtin type in which case we do not qualify it.
        let symbol_table = self.checker.get_symbol_table(None);
        match symbol_table.lookup_in_scope(&details.name, self.current_scope) {
            Some(_) => details.name.to_string(),
            None => {
                let symbol_table = self.checker.get_symbol_table(Some(details.declaration_path.symbol_table_id));
                if symbol_table.file_path.as_path().ends_with("builtins.pyi") {
                    details.name.to_string()
                } else {
                    format!("{}::{}", get_module_name(symbol_table.file_path.as_path()), &details.name)
                }
            }
        }
    }
}

impl<'a> TraversalVisitor for CppTranslator<'a> {
    fn visit_stmt(&mut self, s: &ast::Statement) {
        self.emit_indent();
        match s {
            Statement::ExpressionStatement(e) => self.visit_expr(e),
            Statement::Import(i) => self.visit_import(i),
            Statement::ImportFrom(i) => self.visit_import_from(i),
            Statement::AssignStatement(a) => {
                self.visit_assign(a);
                self.emit(";\n");
            },
            Statement::AnnAssignStatement(a) => self.visit_ann_assign(a),
            Statement::AugAssignStatement(a) => self.visit_aug_assign(a),
            Statement::Assert(a) => self.visit_assert(a),
            Statement::Pass(p) => self.visit_pass(p),
            Statement::Delete(d) => self.visit_delete(d),
            Statement::ReturnStmt(r) => {
                self.visit_return(r);
                self.emit(";\n");
            },
            Statement::Raise(r) => self.visit_raise(r),
            Statement::BreakStmt(b) => self.visit_break(b),
            Statement::ContinueStmt(c) => self.visit_continue(c),
            Statement::Global(g) => self.visit_global(g),
            Statement::Nonlocal(n) => self.visit_nonlocal(n),
            Statement::IfStatement(i) => self.visit_if(i),
            Statement::WhileStatement(w) => self.visit_while(w),
            Statement::ForStatement(f) => self.visit_for(f),
            Statement::WithStatement(w) => self.visit_with(w),
            Statement::TryStatement(t) => self.visit_try(t),
            Statement::TryStarStatement(t) => self.visit_try_star(t),
            Statement::FunctionDef(f) => self.visit_function_def(f),
            Statement::ClassDef(c) => self.visit_class_def(c),
            Statement::MatchStmt(m) => self.visit_match(m),
            Statement::AsyncForStatement(f) => self.visit_async_for(f),
            Statement::AsyncWithStatement(w) => self.visit_async_with(w),
            Statement::AsyncFunctionDef(f) => self.visit_async_function_def(f),
            Statement::TypeAlias(a) => self.visit_type_alias(a),
        }
    }

    fn visit_expr(&mut self, e: &Expression) {
        match e {
            Expression::Constant(c) => self.visit_constant(c),
            Expression::List(l) => self.visit_list(l),
            Expression::Tuple(t) => self.visit_tuple(t),
            Expression::Dict(d) => self.visit_dict(d),
            Expression::Set(s) => self.visit_set(s),
            Expression::Name(n) => self.visit_name(n),
            Expression::BoolOp(b) => self.visit_bool_op(b),
            Expression::UnaryOp(u) => self.visit_unary_op(u),
            Expression::BinOp(b) => self.visit_bin_op(b),
            Expression::NamedExpr(n) => self.visit_named_expr(n),
            Expression::Yield(y) => self.visit_yield(y),
            Expression::YieldFrom(y) => self.visit_yield_from(y),
            Expression::Starred(s) => self.visit_starred(s),
            Expression::Generator(g) => self.visit_generator(g),
            Expression::ListComp(l) => self.visit_list_comp(l),
            Expression::SetComp(s) => self.visit_set_comp(s),
            Expression::DictComp(d) => self.visit_dict_comp(d),
            Expression::Attribute(a) => self.visit_attribute(a),
            Expression::Subscript(s) => self.visit_subscript(s),
            Expression::Slice(s) => self.visit_slice(s),
            Expression::Call(c) => self.visit_call(c),
            Expression::Await(a) => self.visit_await(a),
            Expression::Compare(c) => self.visit_compare(c),
            Expression::Lambda(l) => self.visit_lambda(l),
            Expression::IfExp(i) => self.visit_if_exp(i),
            Expression::JoinedStr(j) => self.visit_joined_str(j),
            Expression::FormattedValue(f) => self.visit_formatted_value(f),
        }
    }

    fn visit_constant(&mut self, constant: &Constant) {
        match constant.value {
            ConstantValue::None => self.emit("None"),
            ConstantValue::Ellipsis => self.emit("..."),
            ConstantValue::Bool(_) => self.emit("bool"),
            ConstantValue::Str(_) => self.emit(constant.get_value(&self.file.source)),
            ConstantValue::Bytes => self.emit("bytes"),
            ConstantValue::Tuple => self.emit("tuple"),
            ConstantValue::Int => self.emit(constant.get_value(&self.file.source)),
            ConstantValue::Float => self.emit(constant.get_value(&self.file.source)),
            ConstantValue::Complex => self.emit("complex"),
            /*
            Constant::Tuple(elements) => {
                let tuple_elements: Vec<String> = elements
                    .iter()
                    .map(|elem| self.translate_constant(elem))
                    .collect::<Result<Vec<String>, _>>()?;
                Ok(format!("({})", tuple_elements.join(", ")))
            },
            */
        };
    }

    fn visit_import(&mut self, import: &Import) {
        for name in import.names.iter() {
            if name.name == "torch" {
                self.emit("#include <torch/torch.h>\n");
            }
        }
    }

    fn visit_assign(&mut self, a: &Assign) {
        let symbol_table = self.checker.get_symbol_table(None);
        for target in &a.targets {
            match target {
                Expression::Name(n) => {
                    let node = symbol_table.lookup_in_scope(&n.id, self.current_scope);
                    match node {
                        Some(node) => {
                            let path = node.declarations[0].declaration_path();
                            // If this is the place where the name was defined, also emit its type
                            if path.node == n.node {
                                self.emit_type(&n.node);
                            }
                        },
                        None => {},
                    };
                    self.visit_name(n);
                },
                Expression::Attribute(attr) => {
                    if let Expression::Name(n) = &attr.value {
                        if n.id == "self" {
                            let cpp_type = self.get_cpp_type(&a.value.get_node());
                            self.class_members.insert(attr.attr.clone(), cpp_type);
                        }
                    }
                    self.visit_expr(target);
                }
                _ => {
                    self.visit_expr(target);
                }
            }
        }
        self.emit(" = ");
        self.visit_expr(&a.value);
    }

    fn visit_name(&mut self, name: &Name) {
        self.emit(name.id.clone());
    }

    fn visit_bin_op(&mut self, b: &BinOp) {
        self.visit_expr(&b.left);
        self.emit(b.op.to_string());
        self.visit_expr(&b.right);
    }

    fn visit_call(&mut self, c: &Call) {
        let mut typ = self.checker.get_type(&c.func.get_node());
        self.visit_expr(&c.func);
        self.emit("(");
        // In case c.func is a class instance, we need to use the __call__ method
        // of that instance instead -- we fix this here.
        if let PythonType::Instance(i) = &typ {
            let symbol_table = self.checker.get_symbol_table(None);
            typ = self.checker.type_evaluator.lookup_on_class(&symbol_table, &i.class_type, "__call__").expect("instance type not callable").clone();
            let PythonType::Callable(old_callable) = typ else {
                panic!("XXX");
            };
            let callable_type = types::CallableType::new(
                old_callable.name,
                old_callable.signature[1..].to_vec(),
                old_callable.return_type,
                old_callable.is_async,
            );
            typ = PythonType::Callable(Box::new(callable_type));
        }
        // In case c.func is a class, we need to use the type signature of the
        // __init__ method.
        if let PythonType::Class(c) = &typ {
            let symbol_table = self.checker.get_symbol_table(None);
            typ = self.checker.type_evaluator.lookup_on_class(&symbol_table, &c, "__init__").expect("class currently needs an __init__ method").clone();
            let PythonType::Callable(old_callable) = typ else {
                panic!("XXX");
            };
            let callable_type = types::CallableType::new(
                old_callable.name,
                old_callable.signature[1..].to_vec(),
                old_callable.return_type,
                old_callable.is_async,
            );
            typ = PythonType::Callable(Box::new(callable_type));
        }
        match typ {
            PythonType::Callable(callable) => {
                let mut num_pos_args = 0;
                // First check all the positional args
                for (i, arg) in callable.signature.iter().enumerate() {
                    match arg {
                        types::CallableArgs::Positional(t) => {
                            self.check_type(&c.args[i].get_node(), t);
                            if i != 0 {
                                self.emit(", ");
                            }
                            self.visit_expr(&c.args[i]);
                            num_pos_args = num_pos_args + 1;
                        },
                        _ => {
                            break;
                        }
                    }
                }
                // Then check all the star args if there are any
                if c.args.len() > num_pos_args {
                    self.emit("{");
                    for (i, arg) in c.args[num_pos_args..].iter().enumerate() {
                        self.check_type(&arg.get_node(), callable.signature[num_pos_args].get_type());
                        if i != 0 {
                            self.emit(", ");
                        }
                        self.visit_expr(arg);
                    }
                    self.emit("}");
                }
            },
            _ => {
                println!("Shouldn't hit this code path");
            }
        }
        // for keyword in &c.keywords {
        //     self.visit_expr(&keyword.value);
        // }
        self.emit(")");
    }

    fn visit_attribute(&mut self, attribute: &Attribute) {
        self.visit_expr(&attribute.value);
        match &attribute.value {
            Expression::Name(n) =>  {
                let symbol_table = self.checker.get_symbol_table(None);
                match symbol_table.lookup_in_scope(&n.id, self.current_scope) {
                    Some(entry) => {
                        match entry.last_declaration() {
                            symbol_table::Declaration::Alias(_a) => {
                                self.emit(format!("::{}", &attribute.attr));
                                return
                            },
                            _ => {}
                        }
                    },
                    None => {},
                }
            },
            _ => {}
        }
        self.emit(format!(".{}", attribute.attr));
    }

    fn visit_return(&mut self, r: &Return) {
        self.emit("return ");
        if let Some(value) = &r.value {
            self.visit_expr(value);
        }
    }

    fn visit_function_def(&mut self, f: &Arc<FunctionDef>) {
        self.enter_scope(f.node.start);
        let mut name = intern_lookup(f.name).to_string();
        if name == "__init__" {
            // In this case, the function is a constructor and in
            // C++ needs to be named the same as the class. We achieve
            // this by naming it after the type of the "self" argument
            // of __init__.
            name = self.get_cpp_type(&f.args.args[0].node);
            self.class_members = HashMap::new();
            self.in_constructor = true;
        }
        if let Some(ret) = &f.returns {
            let return_type = self.get_cpp_type(&ret.get_node());
            self.emit(format!("{} {}(", return_type, name));
        } else {
            if self.in_constructor {
                self.emit(format!("{}(", name));
            } else {
                self.emit(format!("void {}(", name));
            }
        }
        // Filter out "self" arg (first arg of a Python method),
        // since in C++ the "this" arg is implicit.
        // TODO: This will also filter out random args called "self" --
        // instead we should check if we are in a class definition and then
        // only filter the first argument called "self".
        let args = f.args.args.iter().filter(|arg| arg.arg != "self");
        for (i, arg) in args.enumerate() {
            if i != 0 {
                self.emit(", ");
            }
            self.emit_type(&arg.node);
            self.emit(format!(" {}", arg.arg));
        }
        self.emit(") {\n");
        self.indent_level += 1;
        // If this is an instance method, introduce "self"
        self.emit_indent();
        self.emit("auto& self = *this;\n");
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
        self.indent_level -= 1;
        self.emit_indent();
        self.emit("}\n");
        self.in_constructor = false;
        self.leave_scope();
    }

    fn visit_class_def(&mut self, c: &Arc<ClassDef>) {
        let name = intern_lookup(c.name);
        self.emit(format!("class {} {{\n", name));
        self.emit_indent();
        self.emit("public:\n");
        self.enter_scope(c.node.start);
        self.indent_level += 1;
        for stmt in &c.body {
            self.visit_stmt(stmt);
        }
        self.indent_level -= 1;
        // print class member variables
        self.emit_indent();
        self.emit("private:\n");
        // TODO: Want to move this out, not clone it
        for (key, value) in self.class_members.clone() {
            self.emit_indent();
            self.emit(format!("  {} {};\n", value, key));
        }
        self.class_members = HashMap::new();
        self.emit_indent();
        self.emit("};\n");
        self.leave_scope();
    }

    fn visit_for(&mut self, f: &For) {
        let mut bound = None;
        match &f.iter {
            Expression::Call(c) => {
                match &c.func {
                    Expression::Name(n) => {
                        if n.id == "range" {
                            bound = Some(c.args[0].clone());
                        }
                    }
                    _ => {}
                }
            },
            _ => {}
        }
        self.emit("for(int ");
        self.visit_expr(&f.target);
        self.emit(" = 0; ");
        self.visit_expr(&f.target);
        self.emit(" < ");
        self.visit_expr(&bound.unwrap());
        self.emit("; ++");
        self.visit_expr(&f.target);
        self.emit(") {\n");
        self.indent_level += 1;
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
        self.indent_level -= 1;
        self.emit_indent();
        self.emit("}\n");
    }
}

