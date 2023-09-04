use crate::type_check::builtins;
use ast::{Expression, Statement};
use parser::ast::{self, *};

use crate::{
    ast_visitor::TraversalVisitor, settings::Settings, state::State, symbol_table::Declaration,
    type_check::rules::is_reassignment_valid,
};

use super::{
    type_inference::{self, bin_op_result_type, type_check_bin_op},
    types::{CallableType, Type},
};

pub struct TypeChecker<'a> {
    pub errors: Vec<String>,
    // TODO: currently only supporting a single file
    pub module: &'a State,
    pub options: &'a Settings,
}

#[allow(unused)]
impl<'a> TypeChecker<'a> {
    pub fn new(module: &'a State, options: &'a Settings) -> Self {
        TypeChecker {
            errors: vec![],
            module,
            options,
        }
    }

    pub fn type_check(&mut self) {
        for stmt in &self.module.file.body {
            self.visit_stmt(stmt);
        }
    }

    fn infer_declaration_type(&mut self, declaration: &Declaration) -> Type {
        match declaration {
            Declaration::Variable(v) => {
                if let Some(type_annotation) = &v.type_annotation {
                    type_inference::get_type_from_annotation(type_annotation)
                } else if let Some(source) = &v.inferred_type_source {
                    self.infer_expr_type(source)
                } else {
                    Type::Unknown
                }
            }
            Declaration::Function(f) => {
                let return_type = if let Some(type_annotation) = f.function_node.returns.clone() {
                    type_inference::get_type_from_annotation(&type_annotation)
                } else {
                    panic!("Infer the type based on the return statement")
                };

                let arguments = f.function_node.args.clone();
                let name = f.function_node.name.clone();

                Type::Callable(Box::new(CallableType {
                    name,
                    arguments,
                    return_type,
                }))
            }
            _ => panic!("TODO: infer type from declaration"),
        }
    }

    fn infer_type_from_symbol_table(&mut self, name: &str, position: usize) -> Type {
        match self.module.symbol_table.lookup_in_scope(name) {
            Some(symbol) => match symbol.declaration_until_position(position) {
                Some(declaration) => self.infer_declaration_type(declaration),
                None => Type::Unknown,
            },
            None => Type::Unknown,
        }
    }

    fn infer_expr_type(&mut self, expr: &ast::Expression) -> Type {
        match expr {
            ast::Expression::Constant(c) => match c.value {
                ast::ConstantValue::Int(_) => Type::Int,
                ast::ConstantValue::Float(_) => Type::Float,
                ast::ConstantValue::Str(_) => Type::Str,
                ast::ConstantValue::Bool(_) => Type::Bool,
                ast::ConstantValue::None => Type::None,
                _ => Type::Unknown,
            },
            ast::Expression::Name(n) => self.infer_type_from_symbol_table(&n.id, n.node.start),
            ast::Expression::Call(call) => {
                let func = *call.func.clone();
                match func {
                    ast::Expression::Name(n) => {
                        let f_type = self.infer_type_from_symbol_table(n.id.as_str());
                        // we know this must be a callable type otherwise raise error cannot call
                        match f_type {
                            Type::Callable(callable_type) => callable_type.return_type,
                            _ => {
                                self.errors
                                    .push(format!("Cannot call type '{}' as a function", f_type));
                                Type::Unknown
                            }
                        }
                    }
                    ast::Expression::Attribute(a) => panic!("TODO: infer type from attribute"),
                    _ => panic!("TODO: infer type from call"),
                }
            }
            ast::Expression::BinOp(b) => bin_op_result_type(
                &self.infer_expr_type(&b.left),
                &self.infer_expr_type(&b.right),
                &b.op,
            ),
            Expression::List(l) => {
                let mut prev_elm_type = Type::Unknown;
                for elm in &l.elements {
                    let elm_type = self.infer_expr_type(elm);
                    if prev_elm_type == Type::Unknown {
                        prev_elm_type = elm_type;
                    } else if prev_elm_type != elm_type {
                        prev_elm_type = Type::Unknown;
                        break;
                    }
                }
                let final_elm_type = prev_elm_type;
                Type::Class(super::types::ClassType {
                    name: builtins::LIST_TYPE.to_string(),
                    args: vec![final_elm_type],
                })
            }
            Expression::Tuple(_) => todo!(),
            Expression::Dict(_) => todo!(),
            Expression::Set(_) => todo!(),
            Expression::BoolOp(_) => todo!(),
            Expression::UnaryOp(_) => todo!(),
            Expression::NamedExpr(_) => todo!(),
            Expression::Yield(_) => todo!(),
            Expression::YieldFrom(_) => todo!(),
            Expression::Starred(_) => todo!(),
            Expression::Generator(_) => todo!(),
            Expression::ListComp(_) => todo!(),
            Expression::SetComp(_) => todo!(),
            Expression::DictComp(_) => todo!(),
            Expression::Attribute(_) => todo!(),
            Expression::Subscript(s) => {
                let value_type = &self.infer_expr_type(&s.value);
                // if the type of value is subscriptable, then return the type of the subscript

                // the type is subscriptable if it is a list, tuple, dict, or set
                match value_type {
                    Type::Class(class_type) => {
                        if let Some(args) = class_type.args.last() {
                            args.clone()
                        } else {
                            Type::Unknown
                        }
                    }
                    _ => Type::Unknown,
                }
            }
            Expression::Slice(_) => todo!(),
            Expression::Await(_) => todo!(),
            Expression::Compare(_) => todo!(),
            Expression::Lambda(_) => todo!(),
            Expression::IfExp(_) => todo!(),
            Expression::JoinedStr(_) => todo!(),
            Expression::FormattedValue(_) => todo!(),
        }
    }
}
#[allow(unused)]
impl<'a> TraversalVisitor for TypeChecker<'a> {
    fn visit_stmt(&mut self, s: &Statement) {
        // map all statements and call visit
        match s {
            Statement::ExpressionStatement(e) => self.visit_expr(e),
            Statement::Import(i) => self.visit_import(i),
            Statement::ImportFrom(i) => self.visit_import_from(i),
            Statement::AssignStatement(a) => self.visit_assign(a),
            Statement::AnnAssignStatement(a) => self.visit_ann_assign(a),
            Statement::AugAssignStatement(a) => self.visit_aug_assign(a),
            Statement::Assert(a) => self.visit_assert(a),
            Statement::Pass(p) => self.visit_pass(p),
            Statement::Delete(d) => self.visit_delete(d),
            Statement::Return(r) => self.visit_return(r),
            Statement::Raise(r) => self.visit_raise(r),
            Statement::Break(b) => self.visit_break(b),
            Statement::Continue(c) => self.visit_continue(c),
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
            Statement::Match(m) => self.visit_match(m),
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

    fn visit_import(&mut self, _i: &Import) {
        todo!();
    }

    fn visit_import_from(&mut self, _i: &ImportFrom) {
        todo!();
    }

    fn visit_if(&mut self, i: &parser::ast::If) {
        // self.visit_stmt(i.test);
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
        // TODO: need to visit exception handler name and type but let's keep it simple for now
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
        // TODO: need to visit exception handler name and type but let's keep it simple for now
        for handler in &t.handlers {
            for stmt in &handler.body {
                self.visit_stmt(stmt);
            }
        }
    }

    fn visit_function_def(&mut self, f: &parser::ast::FunctionDef) {
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_class_def(&mut self, c: &parser::ast::ClassDef) {
        for stmt in &c.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_match(&mut self, m: &parser::ast::Match) {
        for case in &m.cases {
            for stmt in &case.body {
                self.visit_stmt(stmt);
            }
        }
    }

    fn visit_constant(&mut self, _c: &Constant) {}

    fn visit_list(&mut self, _l: &List) {
        // TODO: check that all elements in the list are of the same type
    }

    fn visit_tuple(&mut self, _t: &Tuple) {
        todo!()
    }

    fn visit_dict(&mut self, _d: &Dict) {
        todo!()
    }

    fn visit_set(&mut self, _s: &Set) {
        todo!()
    }

    fn visit_name(&mut self, _n: &Name) {}

    fn visit_bool_op(&mut self, _b: &BoolOperation) {
        todo!()
    }

    fn visit_unary_op(&mut self, _u: &UnaryOperation) {
        todo!()
    }

    fn visit_bin_op(&mut self, b: &BinOp) {
        self.visit_expr(&b.left);
        self.visit_expr(&b.right);
        let l_type = self.infer_expr_type(&b.left);
        let r_type = self.infer_expr_type(&b.right);

        if !type_check_bin_op(&l_type, &r_type, &b.op) {
            self.errors.push(format!(
                "Operator '{}' not supported for types '{}' and '{}'",
                b.op, l_type, r_type
            ));
        }
    }

    fn visit_named_expr(&mut self, _n: &NamedExpression) {
        todo!()
    }

    fn visit_yield(&mut self, _y: &Yield) {
        todo!()
    }

    fn visit_yield_from(&mut self, _y: &YieldFrom) {
        todo!()
    }

    fn visit_starred(&mut self, _s: &Starred) {
        todo!()
    }

    fn visit_generator(&mut self, _g: &Generator) {
        todo!()
    }

    fn visit_list_comp(&mut self, _l: &ListComp) {
        todo!()
    }

    fn visit_set_comp(&mut self, _s: &SetComp) {
        todo!()
    }

    fn visit_dict_comp(&mut self, _d: &DictComp) {
        todo!()
    }

    fn visit_attribute(&mut self, _a: &Attribute) {
        todo!()
    }

    fn visit_subscript(&mut self, _s: &Subscript) {
        let value = &self.infer_expr_type(&_s.value);
        // let slice = match &_s.slice {
        //     SubscriptSlice::Index(i) => self.infer_expr_type(i),
        //     SubscriptSlice::Slice(s) => self.infer_expr_type(s),
        // };
    }

    fn visit_slice(&mut self, _s: &Slice) {
        todo!()
    }

    fn visit_call(&mut self, _c: &Call) {}

    fn visit_await(&mut self, _a: &Await) {
        todo!()
    }

    fn visit_compare(&mut self, _c: &Compare) {
        todo!()
    }

    fn visit_lambda(&mut self, _l: &Lambda) {
        todo!()
    }

    fn visit_if_exp(&mut self, _i: &IfExp) {
        todo!()
    }

    fn visit_joined_str(&mut self, _j: &JoinedStr) {
        todo!()
    }

    fn visit_formatted_value(&mut self, _f: &FormattedValue) {
        todo!()
    }

    fn visit_alias(&mut self, _a: &Alias) {
        todo!()
    }

    fn visit_assign(&mut self, _a: &Assign) {
        self.visit_expr(&_a.value);
        for target in &_a.targets {
            println!("target: {:?}", target);
            match target {
                ast::Expression::Name(n) => {
                    let prev_target_type = self.infer_type_from_symbol_table(&n.id, n.node.start);
                    let value_type = self.infer_expr_type(&_a.value);
                    if !is_reassignment_valid(&prev_target_type, &value_type) {
                        self.errors.push(format!(
                            "Cannot assign type '{}' to variable of type '{}'",
                            value_type, prev_target_type
                        ));
                    }
                }
                _ => todo!(),
            }
        }
    }
    fn visit_ann_assign(&mut self, _a: &AnnAssign) {}

    fn visit_aug_assign(&mut self, _a: &AugAssign) {
        todo!()
    }

    fn visit_assert(&mut self, _a: &Assert) {
        todo!()
    }

    fn visit_pass(&mut self, _p: &Pass) {
        todo!()
    }

    fn visit_delete(&mut self, _d: &Delete) {
        todo!()
    }

    fn visit_return(&mut self, _r: &Return) {
        // TODO: check that the return type matches the function return type
    }

    fn visit_raise(&mut self, _r: &Raise) {
        todo!()
    }

    fn visit_break(&mut self, _b: &Break) {
        todo!()
    }

    fn visit_continue(&mut self, _c: &Continue) {
        todo!()
    }

    fn visit_global(&mut self, _g: &Global) {
        todo!()
    }

    fn visit_nonlocal(&mut self, _n: &Nonlocal) {
        todo!()
    }
}
