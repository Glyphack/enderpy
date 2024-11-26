use std::sync::Arc;
use enderpy_python_parser::ast::{self, *};
use enderpy_python_parser::parser::parser::intern_lookup;
use std::error::Error;
use std::fmt::Write;
use log::warn;

use enderpy_python_type_checker::{types, ast_visitor::TraversalVisitor, file::EnderpyFile, checker::TypeChecker, types::PythonType};

#[derive(Clone, Debug)]
pub struct CppTranslator<'a> {
    pub output: String,
    indent_level: usize,
    checker: Arc<TypeChecker<'a>>,
    file: &'a EnderpyFile,
}

impl<'a> CppTranslator<'a> {
    pub fn new(checker: Arc<TypeChecker<'a>>, file: &'a EnderpyFile) -> Self {
        CppTranslator {
            output: "".to_string(),
            indent_level: 0,
            checker: checker,
            file: file,
        }
    }

    pub fn translate(&mut self) {
        for stmt in self.file.tree.body.iter() {
            self.visit_stmt(stmt);
        }
    }

    pub fn write_indent(&mut self) {
        write!(self.output, "{}", "  ".repeat(self.indent_level));
    }

    fn check_type(&self, node: &Node, typ: &PythonType) {
        assert!(
            self.checker.get_type(node) == *typ,
            "type error at {}, expected {} got {}",
            self.file.get_position(node.start, node.end),
            typ, self.checker.get_type(node)
        );
    }
}

impl<'a> TraversalVisitor for CppTranslator<'a> {
    fn visit_stmt(&mut self, s: &ast::Statement) {
        self.write_indent();
        match s {
            Statement::ExpressionStatement(e) => self.visit_expr(e),
            Statement::Import(i) => self.visit_import(i),
            Statement::ImportFrom(i) => self.visit_import_from(i),
            Statement::AssignStatement(a) => {
                self.visit_assign(a);
                writeln!(self.output, ";");
            },
            Statement::AnnAssignStatement(a) => self.visit_ann_assign(a),
            Statement::AugAssignStatement(a) => self.visit_aug_assign(a),
            Statement::Assert(a) => self.visit_assert(a),
            Statement::Pass(p) => self.visit_pass(p),
            Statement::Delete(d) => self.visit_delete(d),
            Statement::ReturnStmt(r) => self.visit_return(r),
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
            ConstantValue::None => write!(self.output, "None"),
            ConstantValue::Ellipsis => write!(self.output, "..."),
            ConstantValue::Bool(_) => write!(self.output, "bool"),
            ConstantValue::Str(_) => write!(self.output, "\"{}\"", constant.get_value(&self.file.source).to_string()),
            ConstantValue::Bytes => write!(self.output, "bytes"),
            ConstantValue::Tuple => write!(self.output, "tuple"),
            ConstantValue::Int => write!(self.output, "{}", constant.get_value(&self.file.source).to_string()),
            ConstantValue::Float => write!(self.output, "float"),
            ConstantValue::Complex => write!(self.output, "complex"),
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
                writeln!(self.output, "#include <torch/torch.h>");
            }
        }
    }

    fn visit_assign(&mut self, a: &Assign) {
        for target in &a.targets {
            // let type = self.checker.types.
            match target {
                Expression::Name(n) => {
                    // This loop should only iterate once
                    for t in self.checker.types.find(n.node.start, n.node.end) {
                        write!(self.output, "{} ", python_type_to_cpp(&t.val));
                    }
                    self.visit_name(n);
                },
                _ => {
                    self.visit_expr(target);
                }
            }
        }
        write!(self.output, " = ");
        self.visit_expr(&a.value);
    }

    fn visit_name(&mut self, name: &Name) {
        write!(self.output, "{}", name.id);
    }

    fn visit_bin_op(&mut self, b: &BinOp) {
        self.visit_expr(&b.left);
        write!(self.output, " {} ", &b.op);
        self.visit_expr(&b.right);
    }

    fn visit_call(&mut self, c: &Call) {
        let typ = self.checker.get_type(&c.func.get_node());
        self.visit_expr(&c.func);
        write!(self.output, "(");
        match typ {
            PythonType::Callable(callable) => {
                let mut num_pos_args = 0;
                // First check all the positional args
                for (i, arg) in callable.signature.iter().enumerate() {
                    match arg {
                        types::CallableArgs::Args(t) => {
                            self.check_type(&c.args[i].get_node(), t);
                            num_pos_args = i;
                        },
                        types::CallableArgs::Positional(t) => {
                            break;
                        },
                        _ => {}
                    }
                }
                // Then check all the star args if there are any
                if num_pos_args < c.args.len() {
                    write!(self.output, "{{");
                    for (i, arg) in c.args[num_pos_args..].iter().enumerate() {
                        self.check_type(&arg.get_node(), callable.signature[num_pos_args].get_type());
                        if i != 0 {
                            write!(self.output, ", ");
                        }
                        self.visit_expr(arg);
                    }
                    write!(self.output, "}}");
                }
            },
            _ => {
                println!("Shouldn't hit this code path");
            }
        }
        // for keyword in &c.keywords {
        //     self.visit_expr(&keyword.value);
        // }
        write!(self.output, ")");
    }

    fn visit_attribute(&mut self, attribute: &Attribute) {
        self.visit_expr(&attribute.value);
        write!(self.output, "::{}", attribute.attr);
    }

    fn visit_function_def(&mut self, f: &Arc<FunctionDef>) {
        write!(self.output, "void {}(", intern_lookup(f.name));
        for arg in f.args.args.iter() {
            write!(self.output, "{} {}", python_type_to_cpp(&self.checker.get_type(&arg.node)), arg.arg);
        }
        writeln!(self.output, ") {{");
        self.indent_level += 1;
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
        self.indent_level -= 1;
        writeln!(self.output, "}}");
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
        write!(self.output, "for(int ");
        self.visit_expr(&f.target);
        write!(self.output, " = 0; ");
        self.visit_expr(&f.target);
        write!(self.output, " < ");
        self.visit_expr(&bound.unwrap());
        write!(self.output, "; ++");
        self.visit_expr(&f.target);
        writeln!(self.output, ") {{");
        self.indent_level += 1;
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
        self.indent_level -= 1;
        self.write_indent();
        writeln!(self.output, "}}");
    }
}

fn python_type_to_cpp(python_type: &PythonType) -> String {
    match python_type {
        PythonType::Class(c) => {
            c.details.name.clone()
        },
        PythonType::Instance(i) => {
            i.class_type.details.name.clone()
        },
        _ => String::from(format!("<unknown type {}>", python_type))
    }
}
