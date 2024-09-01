use std::sync::Arc;

use enderpy_python_parser as parser;
use enderpy_python_parser::ast::Expression;

use parser::ast::{self, GetNode, Name, Statement};

use crate::{
    ast_visitor::TraversalVisitor,
    build::ResolvedImports,
    file::EnderpyFile,
    ruff_python_import_resolver::module_descriptor::ImportModuleDescriptor,
    symbol_table::{
        Alias, AsyncFunction, Class, Declaration, DeclarationPath, Function, Parameter,
        SymbolFlags, SymbolTable, SymbolTableNode, SymbolTableScope, SymbolTableType, TypeAlias,
        Variable,
    },
};

#[allow(unused)]
pub struct SemanticAnalyzer<'a> {
    pub symbol_table: SymbolTable,
    /// Map of module name to import result
    /// The imports inside the file are resolved by this map and
    /// no other imports are resolved
    /// Example:
    /// if we have a file with the following imports this is how we use the map
    /// import os -> imports.get("os")
    /// from os import path -> imports.get("os")
    pub imports: &'a ResolvedImports,
    pub function_information: FunctionInformation,
}

#[derive(Debug, Clone)]
pub struct FunctionInformation {
    pub return_statements: Vec<ast::Return>,
    pub yield_statements: Vec<ast::Yield>,
}

#[allow(unused)]
impl<'a> SemanticAnalyzer<'a> {
    pub fn new(file: &'a EnderpyFile, imports: &'a ResolvedImports) -> Self {
        let symbols = SymbolTable::new(&file.path, file.id);
        SemanticAnalyzer {
            symbol_table: symbols,
            imports,
            function_information: FunctionInformation {
                return_statements: Vec::new(),
                yield_statements: Vec::new(),
            },
        }
    }

    fn create_symbol(&mut self, name: String, decl: Declaration, symbol_flags: SymbolFlags) {
        let symbol_node = SymbolTableNode {
            name,
            declarations: vec![decl],
            flags: symbol_flags,
        };
        self.symbol_table.add_symbol(symbol_node)
    }

    fn is_inside_class(&self) -> bool {
        matches!(
            self.symbol_table.current_scope().kind,
            SymbolTableType::Class(_)
        )
    }

    fn create_variable_declaration_symbol(
        &mut self,
        target: &Expression,
        value: Option<Expression>,
        type_annotation: Option<Expression>,
    ) {
        match target {
            Expression::Name(n) => {
                let declaration_path = DeclarationPath::new(
                    self.symbol_table.id,
                    target.get_node(),
                    self.symbol_table.current_scope_id,
                );
                let decl = Declaration::Variable(Variable {
                    declaration_path,
                    type_annotation,
                    inferred_type_source: value,
                    is_constant: false,
                });

                let mut symbol_flags = SymbolFlags::empty();
                if self.symbol_table.current_scope_type().is_class() {
                    symbol_flags |= SymbolFlags::CLASS_MEMBER;
                }

                self.create_symbol(n.id.clone(), decl, symbol_flags)
            }
            Expression::Tuple(t) => {
                for elm in t.elements.iter() {
                    self.create_variable_declaration_symbol(
                        elm,
                        value.clone(),
                        type_annotation.clone(),
                    )
                }
            }
            Expression::Attribute(a) => {
                let member_access_info = get_member_access_info(&self.symbol_table, &a.value);
                let symbol_flags = if member_access_info.is_some_and(|x| x) {
                    SymbolFlags::INSTANCE_MEMBER
                } else {
                    SymbolFlags::CLASS_MEMBER
                };

                if self.function_assigns_attribute(&self.symbol_table) {
                    let declaration_path = DeclarationPath::new(
                        self.symbol_table.id,
                        a.node,
                        self.symbol_table.current_scope_id,
                    );
                    let declaration = Declaration::Variable(Variable {
                        declaration_path,
                        type_annotation,
                        inferred_type_source: value,
                        is_constant: false,
                    });

                    self.create_symbol(a.attr.clone(), declaration, symbol_flags);
                }
            }
            _ => {}
        }
    }

    fn add_arguments_definitions(&mut self, args: &parser::ast::Arguments) {
        let defaults_len = args.defaults.len();
        for (pos_only, index) in args.posonlyargs.iter().zip(0..args.posonlyargs.len()) {
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                pos_only.node,
                self.symbol_table.current_scope_id,
            );
            let default_value = args
                .defaults
                .get(defaults_len.wrapping_sub(index.wrapping_sub(1)))
                .cloned();

            let flags = SymbolFlags::empty();
            self.create_symbol(
                pos_only.arg.clone(),
                Declaration::Parameter(Parameter {
                    declaration_path,
                    parameter_node: pos_only.clone(),
                    type_annotation: pos_only.annotation.clone(),
                    default_value,
                    is_first: false,
                }),
                flags,
            );
        }

        for (arg, index) in args.args.iter().zip(0..) {
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                arg.node,
                self.symbol_table.current_scope_id,
            );

            let default_value = args
                .defaults
                .get(
                    defaults_len
                        .wrapping_sub(args.posonlyargs.len().wrapping_sub(index).wrapping_sub(1)),
                )
                .cloned();

            let flags = SymbolFlags::empty();
            self.create_symbol(
                arg.arg.clone(),
                Declaration::Parameter(Parameter {
                    declaration_path,
                    parameter_node: arg.clone(),
                    type_annotation: arg.annotation.clone(),
                    default_value,
                    is_first: index == 0,
                }),
                flags,
            );
        }

        for arg in args.kwonlyargs.iter() {
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                arg.node,
                self.symbol_table.current_scope_id,
            );
            let flags = SymbolFlags::empty();
            self.create_symbol(
                arg.arg.clone(),
                Declaration::Parameter(Parameter {
                    declaration_path,
                    parameter_node: arg.clone(),
                    type_annotation: arg.annotation.clone(),
                    default_value: None,
                    is_first: false,
                }),
                flags,
            );
        }

        if let Some(ref arg) = args.vararg {
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                arg.node,
                self.symbol_table.current_scope_id,
            );
            let flags = SymbolFlags::empty();
            self.create_symbol(
                arg.arg.clone(),
                Declaration::Parameter(Parameter {
                    declaration_path,
                    parameter_node: arg.clone(),
                    type_annotation: arg.annotation.clone(),
                    default_value: None,
                    is_first: false,
                }),
                flags,
            );
        }

        if let Some(ref arg) = args.kwarg {
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                arg.node,
                self.symbol_table.current_scope_id,
            );
            let flags = SymbolFlags::empty();
            self.create_symbol(
                arg.arg.clone(),
                Declaration::Parameter(Parameter {
                    declaration_path,
                    parameter_node: arg.clone(),
                    type_annotation: arg.annotation.clone(),
                    default_value: None,
                    is_first: false,
                }),
                flags,
            );
        }
    }

    /// Returns true if the current function assigns an attribute to an object
    /// Functions like __init__ and __new__ are considered to assign attributes
    fn function_assigns_attribute(&self, symbol_table: &SymbolTable) -> bool {
        if let Some(function_def) = symbol_table.current_scope().kind.as_function() {
            if function_def.name == "__init__" || function_def.name == "__new__" {
                return true;
            }
        }
        false
    }
}

impl<'a> TraversalVisitor for SemanticAnalyzer<'a> {
    fn visit_stmt(&mut self, s: &parser::ast::Statement) {
        match s {
            parser::ast::Statement::ExpressionStatement(e) => self.visit_expr(e),
            parser::ast::Statement::Import(i) => self.visit_import(i),
            parser::ast::Statement::ImportFrom(i) => self.visit_import_from(i),
            parser::ast::Statement::AssignStatement(a) => self.visit_assign(a),
            parser::ast::Statement::AnnAssignStatement(a) => self.visit_ann_assign(a),
            parser::ast::Statement::AugAssignStatement(a) => self.visit_aug_assign(a),
            parser::ast::Statement::Assert(a) => self.visit_assert(a),
            parser::ast::Statement::Pass(p) => self.visit_pass(p),
            parser::ast::Statement::Delete(d) => self.visit_delete(d),
            parser::ast::Statement::Return(r) => self.visit_return(r),
            parser::ast::Statement::Raise(r) => self.visit_raise(r),
            parser::ast::Statement::Break(b) => self.visit_break(b),
            parser::ast::Statement::Continue(c) => self.visit_continue(c),
            parser::ast::Statement::Global(g) => self.visit_global(g),
            parser::ast::Statement::Nonlocal(n) => self.visit_nonlocal(n),
            parser::ast::Statement::IfStatement(i) => self.visit_if(i),
            parser::ast::Statement::WhileStatement(w) => self.visit_while(w),
            parser::ast::Statement::ForStatement(f) => self.visit_for(f),
            parser::ast::Statement::WithStatement(w) => self.visit_with(w),
            parser::ast::Statement::TryStatement(t) => self.visit_try(t),
            parser::ast::Statement::TryStarStatement(t) => self.visit_try_star(t),
            parser::ast::Statement::FunctionDef(f) => self.visit_function_def(f),
            parser::ast::Statement::ClassDef(c) => self.visit_class_def(c),
            parser::ast::Statement::Match(m) => self.visit_match(m),
            Statement::AsyncForStatement(f) => self.visit_async_for(f),
            Statement::AsyncWithStatement(w) => self.visit_async_with(w),
            Statement::AsyncFunctionDef(f) => self.visit_async_function_def(f),
            Statement::TypeAlias(a) => self.visit_type_alias(a),
        }
    }

    fn visit_expr(&mut self, e: &parser::ast::Expression) {
        match e {
            parser::ast::Expression::Constant(c) => self.visit_constant(c),
            parser::ast::Expression::List(l) => self.visit_list(l),
            parser::ast::Expression::Tuple(t) => self.visit_tuple(t),
            parser::ast::Expression::Dict(d) => self.visit_dict(d),
            parser::ast::Expression::Set(s) => self.visit_set(s),
            parser::ast::Expression::Name(n) => self.visit_name(n),
            parser::ast::Expression::BoolOp(b) => self.visit_bool_op(b),
            parser::ast::Expression::UnaryOp(u) => self.visit_unary_op(u),
            parser::ast::Expression::BinOp(b) => self.visit_bin_op(b),
            parser::ast::Expression::NamedExpr(n) => self.visit_named_expr(n),
            parser::ast::Expression::Yield(y) => self.visit_yield(y),
            parser::ast::Expression::YieldFrom(y) => self.visit_yield_from(y),
            parser::ast::Expression::Starred(s) => self.visit_starred(s),
            parser::ast::Expression::Generator(g) => self.visit_generator(g),
            parser::ast::Expression::ListComp(l) => self.visit_list_comp(l),
            parser::ast::Expression::SetComp(s) => self.visit_set_comp(s),
            parser::ast::Expression::DictComp(d) => self.visit_dict_comp(d),
            parser::ast::Expression::Attribute(a) => self.visit_attribute(a),
            parser::ast::Expression::Subscript(s) => self.visit_subscript(s),
            parser::ast::Expression::Slice(s) => self.visit_slice(s),
            parser::ast::Expression::Call(c) => self.visit_call(c),
            parser::ast::Expression::Await(a) => self.visit_await(a),
            parser::ast::Expression::Compare(c) => self.visit_compare(c),
            parser::ast::Expression::Lambda(l) => self.visit_lambda(l),
            parser::ast::Expression::IfExp(i) => self.visit_if_exp(i),
            parser::ast::Expression::JoinedStr(j) => self.visit_joined_str(j),
            parser::ast::Expression::FormattedValue(f) => self.visit_formatted_value(f),
        }
    }

    fn visit_import(&mut self, i: &parser::ast::Import) {
        for alias in &i.names {
            let import_result = self
                .imports
                .get(&ImportModuleDescriptor::from(alias))
                .cloned();
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                alias.node,
                self.symbol_table.current_scope_id,
            );

            let declaration = Declaration::Alias(Alias {
                declaration_path,
                import_from_node: None,
                import_node: Some(i.clone()),
                symbol_name: None,
                module_name: Some(alias.name()),
                import_result,
            });

            let flags = SymbolFlags::empty();
            self.create_symbol(alias.name(), declaration, flags);
        }
    }

    fn visit_import_from(&mut self, _i: &parser::ast::ImportFrom) {
        let module_import_result = self.imports.get(&ImportModuleDescriptor::from(_i));
        for alias in &_i.names {
            if alias.name == "*" {
                if let Some(module_import_result) = module_import_result {
                    self.symbol_table
                        .star_imports
                        .push(module_import_result.clone());
                }
                continue;
            }
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                alias.node,
                self.symbol_table.current_scope_id,
            );
            let declaration = Declaration::Alias(Alias {
                declaration_path,
                import_from_node: Some(_i.clone()),
                import_node: None,
                symbol_name: Some(alias.name.clone()),
                module_name: None,
                import_result: module_import_result.cloned(),
            });

            let flags = SymbolFlags::empty();
            self.create_symbol(alias.name(), declaration, flags);
        }
    }

    fn visit_if(&mut self, i: &parser::ast::If) {
        for stmt in &i.body {
            self.visit_stmt(stmt);
        }
        for stmt in &i.orelse {
            self.visit_stmt(stmt);
        }
    }

    fn visit_while(&mut self, w: &parser::ast::While) {
        for stmt in &w.body {
            self.visit_stmt(stmt)
        }
    }

    fn visit_for(&mut self, f: &parser::ast::For) {
        self.create_variable_declaration_symbol(&f.target, Some(f.iter.clone()), None);
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_async_for(&mut self, f: &parser::ast::AsyncFor) {
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_with(&mut self, w: &parser::ast::With) {
        for stmt in &w.body {
            self.visit_stmt(stmt);
        }
        for with_items in &w.items {
            self.visit_expr(&with_items.context_expr);
            match &with_items.optional_vars {
                Some(items) => self.visit_expr(items),
                None => (),
            }
        }
    }

    fn visit_async_with(&mut self, w: &parser::ast::AsyncWith) {
        for stmt in &w.body {
            self.visit_stmt(stmt);
        }
        for with_items in &w.items {
            self.visit_expr(&with_items.context_expr);
            match &with_items.optional_vars {
                Some(items) => self.visit_expr(items),
                None => (),
            }
        }
    }

    fn visit_try(&mut self, t: &parser::ast::Try) {
        for stmt in &t.body {
            self.visit_stmt(stmt);
        }
        for stmt in &t.orelse {
            self.visit_stmt(stmt);
        }
        for stmt in &t.finalbody {
            self.visit_stmt(stmt);
        }
        // TODO: need to visit exception handler name and type but let's keep it simple
        // for now
        for handler in &t.handlers {
            for stmt in &handler.body {
                self.visit_stmt(stmt);
            }
        }
    }

    fn visit_try_star(&mut self, t: &parser::ast::TryStar) {
        for stmt in &t.body {
            self.visit_stmt(stmt);
        }
        for stmt in &t.orelse {
            self.visit_stmt(stmt);
        }
        for stmt in &t.finalbody {
            self.visit_stmt(stmt);
        }
        // TODO: need to visit exception handler name and type but let's keep it simple
        // for now
        for handler in &t.handlers {
            for stmt in &handler.body {
                self.visit_stmt(stmt);
            }
        }
    }

    fn visit_function_def(&mut self, f: &Arc<parser::ast::FunctionDef>) {
        let declaration_path = DeclarationPath::new(
            self.symbol_table.id,
            f.node,
            self.symbol_table.current_scope_id,
        );
        if !f.type_params.is_empty() {
            // TODO
            // Push a PEP 695 scope
            // https://www.python.org/dev/peps/pep-0695/
            // for type_parameter in &f.type_params {
            //     let declaration_path = DeclarationPath {
            //         module_name: self.symbol_table.id,
            //         node: type_parameter.get_node(),
            //     };
            //     let flags = SymbolFlags::empty();
            //     self.create_symbol(
            //         type_parameter.get_name(),
            //         Declaration::TypeParameter(crate::symbol_table::TypeParameter {
            //             declaration_path,
            //             type_parameter_node: type_parameter.clone(),
            //         }),
            //         flags,
            //     );
            // }
        }
        self.symbol_table.push_scope(SymbolTableScope::new(
            crate::symbol_table::SymbolTableType::Function(Arc::clone(f)),
            f.name.clone(),
            f.node.start,
            self.symbol_table.current_scope_id,
        ));

        self.add_arguments_definitions(&f.args);

        // TODO: clone
        let prev_function_information = self.function_information.clone();

        for stmt in f.body.iter() {
            self.visit_stmt(stmt);
        }

        let return_statements = std::mem::take(&mut self.function_information.return_statements);
        let yield_statements = std::mem::take(&mut self.function_information.yield_statements);
        self.function_information = prev_function_information;

        for type_parameter in &f.type_params {
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                type_parameter.get_node(),
                self.symbol_table.current_scope_id,
            );
            let flags = SymbolFlags::empty();
            self.create_symbol(
                type_parameter.get_name(),
                Declaration::TypeParameter(crate::symbol_table::TypeParameter {
                    declaration_path,
                    type_parameter_node: type_parameter.clone(),
                }),
                flags,
            );
        }

        self.symbol_table.exit_scope();

        let function_declaration = Declaration::Function(Function {
            declaration_path,
            function_node: f.clone(),
            is_method: self.is_inside_class(),
            is_generator: !yield_statements.is_empty(),
            return_statements,
            yield_statements,
            raise_statements: vec![],
        });
        let flags = SymbolFlags::empty();
        self.create_symbol(f.name.clone(), function_declaration, flags);
    }

    fn visit_async_function_def(&mut self, f: &Arc<parser::ast::AsyncFunctionDef>) {
        let declaration_path = DeclarationPath::new(
            self.symbol_table.id,
            f.node,
            self.symbol_table.current_scope_id,
        );

        self.symbol_table.push_scope(SymbolTableScope::new(
            SymbolTableType::Function(Arc::new(f.to_function_def())),
            f.name.clone(),
            f.node.start,
            self.symbol_table.current_scope_id,
        ));

        self.add_arguments_definitions(&f.args);

        // TODO: clone
        let prev_function_information = self.function_information.clone();

        for stmt in f.body.iter() {
            self.visit_stmt(stmt);
        }

        let return_statements = std::mem::take(&mut self.function_information.return_statements);
        let yield_statements = std::mem::take(&mut self.function_information.yield_statements);
        self.function_information = prev_function_information;

        for type_parameter in &f.type_params {
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                type_parameter.get_node(),
                self.symbol_table.current_scope_id,
            );
            let flags = SymbolFlags::empty();
            self.create_symbol(
                type_parameter.get_name(),
                Declaration::TypeParameter(crate::symbol_table::TypeParameter {
                    declaration_path,
                    type_parameter_node: type_parameter.clone(),
                }),
                flags,
            );
        }
        self.symbol_table.exit_scope();

        let function_declaration = Declaration::AsyncFunction(AsyncFunction {
            declaration_path,
            function_node: f.clone(),
            is_method: self.is_inside_class(),
            is_generator: !yield_statements.is_empty(),
            return_statements,
            yield_statements,
            raise_statements: vec![],
        });
        let flags = SymbolFlags::empty();
        self.create_symbol(f.name.clone(), function_declaration, flags);
    }

    fn visit_type_alias(&mut self, t: &parser::ast::TypeAlias) {
        let declaration_path = DeclarationPath::new(
            self.symbol_table.id,
            t.node,
            self.symbol_table.current_scope_id,
        );
        let flags = SymbolFlags::empty();
        self.create_symbol(
            t.name.clone(),
            Declaration::TypeAlias(TypeAlias {
                declaration_path,
                type_alias_node: t.clone(),
            }),
            flags,
        );
    }

    fn visit_class_def(&mut self, c: &Arc<parser::ast::ClassDef>) {
        self.symbol_table.push_scope(SymbolTableScope::new(
            SymbolTableType::Class(c.clone()),
            c.name.clone(),
            c.node.start,
            self.symbol_table.current_scope_id,
        ));

        for type_parameter in &c.type_params {
            let declaration_path = DeclarationPath::new(
                self.symbol_table.id,
                type_parameter.get_node(),
                self.symbol_table.current_scope_id,
            );
            let flags = SymbolFlags::empty();
            self.create_symbol(
                type_parameter.get_name(),
                Declaration::TypeParameter(crate::symbol_table::TypeParameter {
                    declaration_path,
                    type_parameter_node: type_parameter.clone(),
                }),
                flags,
            );
        }
        for stmt in &c.body {
            self.visit_stmt(stmt);
        }

        let class_body_scope_id = self.symbol_table.current_scope_id;

        self.symbol_table.exit_scope();

        let class_declaration_path = DeclarationPath::new(
            self.symbol_table.id,
            c.node,
            self.symbol_table.current_scope_id,
        );
        let class_declaration = Declaration::Class(Class::new(
            self.symbol_table
                .file_path
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string(),
            Arc::clone(c),
            class_declaration_path,
            class_body_scope_id,
        ));
        let flags = SymbolFlags::empty();
        self.create_symbol(c.name.clone(), class_declaration, flags);
    }

    fn visit_match(&mut self, m: &parser::ast::Match) {
        for case in &m.cases {
            for stmt in &case.body {
                self.visit_stmt(stmt);
            }
        }
    }

    fn visit_constant(&mut self, _c: &parser::ast::Constant) {}

    fn visit_list(&mut self, _l: &parser::ast::List) {
        for elm in _l.elements.iter() {
            self.visit_expr(elm);
        }
    }

    fn visit_tuple(&mut self, _t: &parser::ast::Tuple) {}

    fn visit_dict(&mut self, _d: &parser::ast::Dict) {}

    fn visit_set(&mut self, _s: &parser::ast::Set) {}

    fn visit_name(&mut self, _n: &Name) {}

    fn visit_bool_op(&mut self, _b: &parser::ast::BoolOperation) {}

    fn visit_unary_op(&mut self, _u: &parser::ast::UnaryOperation) {}

    fn visit_bin_op(&mut self, _b: &parser::ast::BinOp) {}

    fn visit_named_expr(&mut self, _n: &parser::ast::NamedExpression) {}

    // TODO: clone
    fn visit_yield(&mut self, y: &parser::ast::Yield) {
        self.function_information.yield_statements.push(y.clone());
    }

    fn visit_yield_from(&mut self, _y: &parser::ast::YieldFrom) {}

    fn visit_starred(&mut self, _s: &parser::ast::Starred) {}

    fn visit_generator(&mut self, _g: &parser::ast::Generator) {}

    fn visit_list_comp(&mut self, _l: &parser::ast::ListComp) {}

    fn visit_set_comp(&mut self, _s: &parser::ast::SetComp) {}

    fn visit_dict_comp(&mut self, _d: &parser::ast::DictComp) {}

    // Imagine it's always store
    fn visit_attribute(&mut self, _a: &parser::ast::Attribute) {}

    fn visit_subscript(&mut self, _s: &parser::ast::Subscript) {}

    fn visit_slice(&mut self, _s: &parser::ast::Slice) {}

    fn visit_call(&mut self, _c: &parser::ast::Call) {
        // TODO: more arguments
        for arg in &_c.args {
            self.visit_expr(arg);
        }
    }

    fn visit_await(&mut self, _a: &parser::ast::Await) {}

    fn visit_compare(&mut self, _c: &parser::ast::Compare) {}

    fn visit_lambda(&mut self, _l: &parser::ast::Lambda) {}

    fn visit_if_exp(&mut self, _i: &parser::ast::IfExp) {}

    fn visit_joined_str(&mut self, _j: &parser::ast::JoinedStr) {}

    fn visit_formatted_value(&mut self, _f: &parser::ast::FormattedValue) {}

    fn visit_alias(&mut self, _a: &parser::ast::Alias) {}

    fn visit_assign(&mut self, assign: &parser::ast::Assign) {
        let value = &assign.value;
        if assign.targets.len() > 1 {
            todo!("multiple assignment not supported");
        }
        let target = assign
            .targets
            .last()
            .expect("Assignment has at least one target");
        self.create_variable_declaration_symbol(target, Some(value.clone()), None);

        self.visit_expr(&assign.value);
    }

    fn visit_ann_assign(&mut self, a: &parser::ast::AnnAssign) {
        let value = &a.value;
        let target = &a.target;
        self.create_variable_declaration_symbol(target, value.clone(), Some(a.annotation.clone()));

        if let Some(val) = &a.value {
            self.visit_expr(val);
        }
    }

    fn visit_aug_assign(&mut self, a: &parser::ast::AugAssign) {
        self.visit_expr(&a.target);
        self.visit_expr(&a.value);
    }

    fn visit_assert(&mut self, _a: &parser::ast::Assert) {}

    fn visit_pass(&mut self, _p: &parser::ast::Pass) {}

    fn visit_delete(&mut self, _d: &parser::ast::Delete) {}

    // TODO: clone
    fn visit_return(&mut self, r: &parser::ast::Return) {
        self.function_information.return_statements.push(r.clone());
    }

    fn visit_raise(&mut self, _r: &parser::ast::Raise) {}

    fn visit_break(&mut self, _b: &parser::ast::Break) {}

    fn visit_continue(&mut self, _c: &parser::ast::Continue) {}

    fn visit_global(&mut self, _g: &parser::ast::Global) {}

    fn visit_nonlocal(&mut self, _n: &parser::ast::Nonlocal) {}
}

pub struct MemberAccessInfo {}

// determines whether a member access expression is referring to a
// member of a class (either a class or instance member). this will
// typically take the form "self.x" or "cls.x".
// returns None if the expression is not a member access expression or
// or true if the member is an instance member and false if it is a class member
pub fn get_member_access_info(
    symbol_table: &SymbolTable,
    value: &parser::ast::Expression,
) -> Option<bool> {
    let name = value.as_name()?;

    let value_name = &name.id;

    let current_scope = symbol_table.current_scope();
    let function_def = current_scope.kind.as_function()?;
    let parent_scope = symbol_table.parent_scope(symbol_table.current_scope())?;

    let enclosing_class = parent_scope.kind.as_class()?;

    let first_arg = function_def.args.args.first()?;

    let is_value_equal_to_first_arg = value_name == first_arg.arg.as_str();

    if !is_value_equal_to_first_arg {
        return None;
    }

    // Check if one of the decorators is a classmethod or staticmethod
    let mut is_class_member = false;
    for decorator in function_def.decorator_list.iter() {
        if let parser::ast::Expression::Call(call) = decorator {
            if let Some(name) = call.func.as_name() {
                if name.id == "classmethod" {
                    is_class_member = true;
                }
            }
        }
    }

    // e.g. "MyClass.x = 1"
    if value_name == enclosing_class.name.as_str() || is_class_member {
        Some(false)
    } else {
        Some(true)
    }
}
