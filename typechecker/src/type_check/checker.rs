use ast::{Expression, Statement};
use parser::ast::{self, *};

use crate::{
    ast_visitor::TraversalVisitor, settings::Settings, state::State, symbol_table::SymbolTable,
    type_check::rules::is_reassignment_valid,
};

use super::{type_evaluator::TypeEvaluator, type_inference::type_check_bin_op, types::Type};

// TODO: currently only supporting a single file
pub struct TypeChecker<'a> {
    pub errors: Vec<String>,
    // The symbol table of the module being type checked
    symbol_table: SymbolTable,
    pub options: &'a Settings,
    type_evaluator: TypeEvaluator,
}

#[allow(unused)]
impl<'a> TypeChecker<'a> {
    pub fn new(module: &'a State, options: &'a Settings) -> Self {
        let symbol_table = module.get_symbol_table();
        TypeChecker {
            errors: vec![],
            symbol_table,
            options,
            type_evaluator: TypeEvaluator::new(module.get_symbol_table()),
        }
    }

    pub fn type_check(&mut self, statement: &Statement) {
        self.visit_stmt(statement);
    }

    fn infer_expr_type(&mut self, expr: &Expression) -> Type {
        match self.type_evaluator.get_type(expr) {
            Ok(t) => t,
            Err(e) => {
            self.make_error(
                &format!("Failed to infer type for expression: {}", e),
                &format!("{:?}", expr),
                expr.get_node().start,
                expr.get_node().end,
            );
            return Type::Unknown
            }
        }
    }

    fn make_error(&mut self, msg: &str, code: &str, start: usize, end: usize) {
        let error = format!("{}: {} at ({})", msg, code, start);
        self.errors.push(error);
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

    fn visit_import(&mut self, _i: &Import) {}

    fn visit_import_from(&mut self, _i: &ImportFrom) {}

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

    fn visit_tuple(&mut self, _t: &Tuple) {}

    fn visit_dict(&mut self, _d: &Dict) {}

    fn visit_set(&mut self, _s: &Set) {}

    fn visit_name(&mut self, _n: &Name) {}

    fn visit_bool_op(&mut self, _b: &BoolOperation) {}

    fn visit_unary_op(&mut self, _u: &UnaryOperation) {}

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

    fn visit_named_expr(&mut self, _n: &NamedExpression) {}

    fn visit_yield(&mut self, _y: &Yield) {}

    fn visit_yield_from(&mut self, _y: &YieldFrom) {}

    fn visit_starred(&mut self, _s: &Starred) {}

    fn visit_generator(&mut self, _g: &Generator) {}

    fn visit_list_comp(&mut self, _l: &ListComp) {}

    fn visit_set_comp(&mut self, _s: &SetComp) {}

    fn visit_dict_comp(&mut self, _d: &DictComp) {}

    fn visit_attribute(&mut self, _a: &Attribute) {}

    fn visit_subscript(&mut self, _s: &Subscript) {
        let value = &self.infer_expr_type(&_s.value);
        // let slice = match &_s.slice {
        //     SubscriptSlice::Index(i) => self.infer_expr_type(i),
        //     SubscriptSlice::Slice(s) => self.infer_expr_type(s),
        // };
    }

    fn visit_slice(&mut self, _s: &Slice) {}

    fn visit_call(&mut self, _c: &Call) {}

    fn visit_await(&mut self, _a: &Await) {}

    fn visit_compare(&mut self, _c: &Compare) {}

    fn visit_lambda(&mut self, _l: &Lambda) {}

    fn visit_if_exp(&mut self, _i: &IfExp) {}

    fn visit_joined_str(&mut self, _j: &JoinedStr) {}

    fn visit_formatted_value(&mut self, _f: &FormattedValue) {}

    fn visit_alias(&mut self, _a: &Alias) {}

    fn visit_assign(&mut self, _a: &Assign) {
        self.visit_expr(&_a.value);
        for target in &_a.targets {
            match target {
                ast::Expression::Name(n) => {
                    let symbol = self.symbol_table.lookup_in_scope(&n.id);
                    if let Some(symbol) = symbol {
                        let prev_target_type = self
                            .type_evaluator
                            .get_symbol_node_type(symbol, n.node.start).unwrap_or(Type::Unknown);
                        let value_type = self.infer_expr_type(&_a.value);
                        if !is_reassignment_valid(&prev_target_type, &value_type) {
                            self.errors.push(format!(
                                "Cannot assign type '{}' to variable of type '{}'",
                                value_type, prev_target_type
                            ));
                        }
                    }
                }
                _ => todo!(),
            }
        }
    }
    fn visit_ann_assign(&mut self, _a: &AnnAssign) {}

    fn visit_aug_assign(&mut self, _a: &AugAssign) {}

    fn visit_assert(&mut self, _a: &Assert) {}

    fn visit_pass(&mut self, _p: &Pass) {}

    fn visit_delete(&mut self, _d: &Delete) {}

    fn visit_return(&mut self, _r: &Return) {
        // TODO: check that the return type matches the function return type
    }

    fn visit_raise(&mut self, _r: &Raise) {}

    fn visit_break(&mut self, _b: &Break) {}

    fn visit_continue(&mut self, _c: &Continue) {}

    fn visit_global(&mut self, _g: &Global) {}

    fn visit_nonlocal(&mut self, _n: &Nonlocal) {}
}
