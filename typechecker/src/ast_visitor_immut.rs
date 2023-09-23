use ast::{Expression, Statement};

use enderpy_python_parser::ast::{self, *};

use enderpy_python_parser as parser;

pub trait TraversalVisitorImmut {
    fn visit_stmt(&self, s: &Statement) {
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
    fn visit_expr(&self, e: &Expression) {
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
    fn visit_import(&self, _i: &Import) {
        todo!();
    }

    fn visit_import_from(&self, _i: &ImportFrom) {
        todo!();
    }

    fn visit_if(&self, i: &parser::ast::If) {
        for stmt in &i.body {
            self.visit_stmt(stmt);
        }
        for stmt in &i.orelse {
            self.visit_stmt(stmt);
        }
    }

    fn visit_while(&self, w: &parser::ast::While) {
        for stmt in &w.body {
            self.visit_stmt(stmt)
        }
    }

    fn visit_for(&self, f: &parser::ast::For) {
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_with(&self, w: &parser::ast::With) {
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

    fn visit_try(&self, t: &parser::ast::Try) {
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

    fn visit_try_star(&self, t: &parser::ast::TryStar) {
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

    fn visit_function_def(&self, f: &parser::ast::FunctionDef) {
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_class_def(&self, c: &parser::ast::ClassDef) {
        for stmt in &c.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_match(&self, m: &parser::ast::Match) {
        for case in &m.cases {
            for stmt in &case.body {
                self.visit_stmt(stmt);
            }
        }
    }

    fn visit_constant(&self, _c: &Constant) {
        todo!()
    }
    fn visit_list(&self, _l: &List) {
        todo!()
    }
    fn visit_tuple(&self, _t: &Tuple) {
        todo!()
    }
    fn visit_dict(&self, _d: &Dict) {
        todo!()
    }
    fn visit_set(&self, _s: &Set) {
        todo!()
    }
    fn visit_name(&self, _n: &Name) {}
    fn visit_bool_op(&self, _b: &BoolOperation) {
        todo!()
    }
    fn visit_unary_op(&self, _u: &UnaryOperation) {
        todo!()
    }
    fn visit_bin_op(&self, _b: &BinOp) {
        todo!()
    }
    fn visit_named_expr(&self, _n: &NamedExpression) {
        todo!()
    }
    fn visit_yield(&self, _y: &Yield) {
        todo!()
    }
    fn visit_yield_from(&self, _y: &YieldFrom) {
        todo!()
    }
    fn visit_starred(&self, _s: &Starred) {
        todo!()
    }
    fn visit_generator(&self, _g: &Generator) {
        todo!()
    }
    fn visit_list_comp(&self, _l: &ListComp) {
        todo!()
    }
    fn visit_set_comp(&self, _s: &SetComp) {
        todo!()
    }
    fn visit_dict_comp(&self, _d: &DictComp) {
        todo!()
    }
    fn visit_attribute(&self, _a: &Attribute) {
        todo!()
    }
    fn visit_subscript(&self, _s: &Subscript) {
        todo!()
    }
    fn visit_slice(&self, _s: &Slice) {
        todo!()
    }
    fn visit_call(&self, _c: &Call) {
        todo!()
    }
    fn visit_await(&self, _a: &Await) {
        todo!()
    }
    fn visit_compare(&self, _c: &Compare) {
        todo!()
    }
    fn visit_lambda(&self, _l: &Lambda) {
        todo!()
    }
    fn visit_if_exp(&self, _i: &IfExp) {
        todo!()
    }
    fn visit_joined_str(&self, _j: &JoinedStr) {
        todo!()
    }
    fn visit_formatted_value(&self, _f: &FormattedValue) {
        todo!()
    }

    fn visit_alias(&self, _a: &Alias) {
        todo!()
    }

    fn visit_assign(&self, _a: &Assign) {
        todo!()
    }

    fn visit_ann_assign(&self, _a: &AnnAssign) {
        todo!()
    }

    fn visit_aug_assign(&self, _a: &AugAssign) {
        todo!()
    }

    fn visit_assert(&self, _a: &Assert) {
        todo!()
    }

    fn visit_pass(&self, _p: &Pass) {
        todo!()
    }

    fn visit_delete(&self, _d: &Delete) {
        todo!()
    }

    fn visit_return(&self, _r: &Return) {
        todo!()
    }

    fn visit_raise(&self, _r: &Raise) {
        todo!()
    }

    fn visit_break(&self, _b: &Break) {
        todo!()
    }

    fn visit_continue(&self, _c: &Continue) {
        todo!()
    }

    fn visit_global(&self, _g: &Global) {
        todo!()
    }

    fn visit_nonlocal(&self, _n: &Nonlocal) {
        todo!()
    }
}
