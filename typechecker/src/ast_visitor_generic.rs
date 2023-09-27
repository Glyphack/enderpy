#![allow(dead_code)]
#![allow(unused_variables)]

use ast::{Expression, Statement};


use enderpy_python_parser as parser;
use enderpy_python_parser::ast::{self, *};

pub trait TraversalVisitorImmutGeneric<T> {
    fn visit_stmt(&self, s: &Statement) -> T {
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
            Statement::AsyncForStatement(f) => self.visit_async_for(f),
            Statement::AsyncWithStatement(w) => self.visit_async_with(w), 
            Statement::AsyncFunctionDef(f) => self.visit_async_function_def(f),
        }
    }
    fn visit_expr(&self, e: &Expression) -> T {
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
    fn visit_import(&self, _i: &Import) -> T {
        todo!();
    }

    fn visit_import_from(&self, _i: &ImportFrom) -> T {
        todo!();
    }

    fn visit_if(&self, i: &parser::ast::If) -> T {
        todo!();
    }

    fn visit_while(&self, w: &parser::ast::While) -> T {
        todo!();
    }

    fn visit_for(&self, f: &parser::ast::For) -> T {
        todo!();
    }

    fn visit_async_for(&self, f: &parser::ast::AsyncFor) -> T {
        todo!();
    }

    fn visit_with(&self, w: &parser::ast::With) -> T {
        todo!();
    }

    fn visit_async_with(&self, w: &parser::ast::AsyncWith) -> T {
        todo!();
    }

    fn visit_try(&self, t: &parser::ast::Try) -> T {
        todo!();
    }

    fn visit_try_star(&self, t: &parser::ast::TryStar) -> T {
        todo!();
    }

    fn visit_function_def(&self, f: &parser::ast::FunctionDef) -> T {
        todo!();
    }

    fn visit_async_function_def(&self, f: &parser::ast::AsyncFunctionDef) -> T {
        todo!();
    }

    fn visit_class_def(&self, c: &parser::ast::ClassDef) -> T {
        todo!();
    }

    fn visit_match(&self, m: &parser::ast::Match) -> T {
        todo!();
    }

    fn visit_constant(&self, _c: &Constant) -> T {
        todo!();
    }
    fn visit_list(&self, _l: &List) -> T {
        todo!()
    }
    fn visit_tuple(&self, _t: &Tuple) -> T {
        todo!()
    }
    fn visit_dict(&self, _d: &Dict) -> T {
        todo!()
    }
    fn visit_set(&self, _s: &Set) -> T {
        todo!()
    }
    fn visit_name(&self, _n: &Name) -> T {
        todo!()
    }
    fn visit_bool_op(&self, _b: &BoolOperation) -> T {
        todo!()
    }
    fn visit_unary_op(&self, _u: &UnaryOperation) -> T {
        todo!()
    }
    fn visit_bin_op(&self, _b: &BinOp) -> T {
        todo!()
    }
    fn visit_named_expr(&self, _n: &NamedExpression) -> T {
        todo!()
    }
    fn visit_yield(&self, _y: &Yield) -> T {
        todo!()
    }
    fn visit_yield_from(&self, _y: &YieldFrom) -> T {
        todo!()
    }
    fn visit_starred(&self, _s: &Starred) -> T {
        todo!()
    }
    fn visit_generator(&self, _g: &Generator) -> T {
        todo!()
    }
    fn visit_list_comp(&self, _l: &ListComp) -> T {
        todo!()
    }
    fn visit_set_comp(&self, _s: &SetComp) -> T {
        todo!()
    }
    fn visit_dict_comp(&self, _d: &DictComp) -> T {
        todo!()
    }
    fn visit_attribute(&self, _a: &Attribute) -> T {
        todo!()
    }
    fn visit_subscript(&self, _s: &Subscript) -> T {
        todo!()
    }
    fn visit_slice(&self, _s: &Slice) -> T {
        todo!()
    }
    fn visit_call(&self, _c: &Call) -> T {
        todo!()
    }
    fn visit_await(&self, _a: &Await) -> T {
        todo!()
    }
    fn visit_compare(&self, _c: &Compare) -> T {
        todo!()
    }
    fn visit_lambda(&self, _l: &Lambda) -> T {
        todo!()
    }
    fn visit_if_exp(&self, _i: &IfExp) -> T {
        todo!()
    }
    fn visit_joined_str(&self, _j: &JoinedStr) -> T {
        todo!()
    }
    fn visit_formatted_value(&self, _f: &FormattedValue) -> T {
        todo!()
    }

    fn visit_alias(&self, _a: &Alias) -> T {
        todo!()
    }

    fn visit_assign(&self, _a: &Assign) -> T {
        todo!()
    }

    fn visit_ann_assign(&self, _a: &AnnAssign) -> T {
        todo!()
    }

    fn visit_aug_assign(&self, _a: &AugAssign) -> T {
        todo!()
    }

    fn visit_assert(&self, _a: &Assert) -> T {
        todo!()
    }

    fn visit_pass(&self, _p: &Pass) -> T {
        todo!()
    }

    fn visit_delete(&self, _d: &Delete) -> T {
        todo!()
    }

    fn visit_return(&self, _r: &Return) -> T {
        todo!()
    }

    fn visit_raise(&self, _r: &Raise) -> T {
        todo!()
    }

    fn visit_break(&self, _b: &Break) -> T {
        todo!()
    }

    fn visit_continue(&self, _c: &Continue) -> T {
        todo!()
    }

    fn visit_global(&self, _g: &Global) -> T {
        todo!()
    }

    fn visit_nonlocal(&self, _n: &Nonlocal) -> T {
        todo!()
    }
}
