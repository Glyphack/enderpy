use crate::{ast::*, nodes::EnderpyFile};

// pub trait FileVisitor<T>: StmtVisitor<T> + ExprVisitor<T> {
//     fn visit_file(&mut self, f: &EnderpyFile) -> T {
//         self.visit_module(&f.ast)
//     }
//     fn visit_module(&mut self, m: &Module) -> T {
//         for stmt in m.body {
//             self.visit_stmt(stmt)
//         }
//     }
// }
//
// pub trait StmtVisitor<T> {
//     fn visit_stmt(&mut self, s: &Statement) -> T;
//     fn visit_expr(&mut self, e: &Expression) -> T;
//     fn visit_import(&mut self, i: &Import) -> T;
//     fn visit_import_from(&mut self, i: &ImportFrom) -> T;
//     fn visit_alias(&mut self, a: &Alias) -> T;
//     fn visit_assign(&mut self, a: &Assign) -> T;
//     fn visit_ann_assign(&mut self, a: &AnnAssign) -> T;
//     fn visit_aug_assign(&mut self, a: &AugAssign) -> T;
//     fn visit_assert(&mut self, a: &Assert) -> T;
//     fn visit_pass(&mut self, p: &Pass) -> T;
//     fn visit_delete(&mut self, d: &Delete) -> T;
//     fn visit_return(&mut self, r: &Return) -> T;
//     fn visit_raise(&mut self, r: &Raise) -> T;
//     fn visit_break(&mut self, b: &Break) -> T;
//     fn visit_continue(&mut self, c: &Continue) -> T;
//     fn visit_global(&mut self, g: &Global) -> T;
//     fn visit_nonlocal(&mut self, n: &Nonlocal) -> T;
//     fn visit_if(&mut self, i: &If) -> T;
//     fn visit_while(&mut self, w: &While) -> T;
//     fn visit_for(&mut self, f: &For) -> T;
//     fn visit_with(&mut self, w: &With) -> T;
//     fn visit_try(&mut self, t: &Try) -> T;
//     fn visit_try_star(&mut self, t: &TryStar) -> T;
//     fn visit_function_def(&mut self, f: &FunctionDef) -> T;
//     fn visit_class_def(&mut self, c: &ClassDef) -> T;
//     fn visit_match(&mut self, m: &Match) -> T;
// }
//
// pub trait ExprVisitor<T> {
//     fn visit_constant(&mut self, c: &Constant) -> T;
//     fn visit_list(&mut self, l: &List) -> T;
//     fn visit_tuple(&mut self, t: &Tuple) -> T;
//     fn visit_dict(&mut self, d: &Dict) -> T;
//     fn visit_set(&mut self, s: &Set) -> T;
//     fn visit_name(&mut self, n: &Name) -> T;
//     fn visit_bool_op(&mut self, b: &BoolOperation) -> T;
//     fn visit_unary_op(&mut self, u: &UnaryOperation) -> T;
//     fn visit_bin_op(&mut self, b: &BinOp) -> T;
//     fn visit_named_expr(&mut self, n: &NamedExpression) -> T;
//     fn visit_yield(&mut self, y: &Yield) -> T;
//     fn visit_yield_from(&mut self, y: &YieldFrom) -> T;
//     fn visit_starred(&mut self, s: &Starred) -> T;
//     fn visit_generator(&mut self, g: &Generator) -> T;
//     fn visit_list_comp(&mut self, l: &ListComp) -> T;
//     fn visit_set_comp(&mut self, s: &SetComp) -> T;
//     fn visit_dict_comp(&mut self, d: &DictComp) -> T;
//     fn visit_attribute(&mut self, a: &Attribute) -> T;
//     fn visit_subscript(&mut self, s: &Subscript) -> T;
//     fn visit_slice(&mut self, s: &Slice) -> T;
//     fn visit_call(&mut self, c: &Call) -> T;
//     fn visit_await(&mut self, a: &Await) -> T;
//     fn visit_compare(&mut self, c: &Compare) -> T;
//     fn visit_lambda(&mut self, l: &Lambda) -> T;
//     fn visit_if_exp(&mut self, i: &IfExp) -> T;
//     fn visit_joined_str(&mut self, j: &JoinedStr) -> T;
//     fn visit_formatted_value(&mut self, f: &FormattedValue) -> T;
// }

/// A visitor that traverses the AST and calls the visit method for each node
/// This is useful for visitors that only need to visit a few nodes
/// and don't want to implement all the methods.
/// The overriden methods must make sure to continue the traversal.
pub trait TraversalVisitor<T> {
    fn visit_stmt(&mut self, s: &Statement) -> T {
        // map all statements and call visit
        match s {
            Statement::Expression(e) => self.visit_expr(e),
            Statement::Import(i) => self.visit_import(i),
            Statement::ImportFrom(i) => self.visit_import_from(i),
            Statement::Assign(a) => self.visit_assign(a),
            Statement::AnnAssign(a) => self.visit_ann_assign(a),
            Statement::AugAssign(a) => self.visit_aug_assign(a),
            Statement::Assert(a) => self.visit_assert(a),
            Statement::Pass(p) => self.visit_pass(p),
            Statement::Delete(d) => self.visit_delete(d),
            Statement::Return(r) => self.visit_return(r),
            Statement::Raise(r) => self.visit_raise(r),
            Statement::Break(b) => self.visit_break(b),
            Statement::Continue(c) => self.visit_continue(c),
            Statement::Global(g) => self.visit_global(g),
            Statement::Nonlocal(n) => self.visit_nonlocal(n),
            Statement::If(i) => self.visit_if(i),
            Statement::While(w) => self.visit_while(w),
            Statement::For(f) => self.visit_for(f),
            Statement::With(w) => self.visit_with(w),
            Statement::Try(t) => self.visit_try(t),
            Statement::TryStar(t) => self.visit_try_star(t),
            Statement::FunctionDef(f) => self.visit_function_def(f),
            Statement::ClassDef(c) => self.visit_class_def(c),
            Statement::Match(m) => self.visit_match(m),
        }
    }
    fn visit_expr(&mut self, e: &Expression) -> T {
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
    fn visit_import(&mut self, i: &Import) -> T {
        todo!()
    }
    fn visit_import_from(&mut self, i: &ImportFrom) -> T {
        todo!()
    }
    fn visit_alias(&mut self, a: &Alias) -> T {
        todo!()
    }
    fn visit_assign(&mut self, a: &Assign) -> T {
        todo!()
    }
    fn visit_ann_assign(&mut self, a: &AnnAssign) -> T {
        todo!()
    }
    fn visit_aug_assign(&mut self, a: &AugAssign) -> T {
        todo!()
    }
    fn visit_assert(&mut self, a: &Assert) -> T {
        todo!()
    }
    fn visit_pass(&mut self, p: &Pass) -> T {
        todo!()
    }
    fn visit_delete(&mut self, d: &Delete) -> T {
        todo!()
    }
    fn visit_return(&mut self, r: &Return) -> T {
        todo!()
    }
    fn visit_raise(&mut self, r: &Raise) -> T {
        todo!()
    }
    fn visit_break(&mut self, b: &Break) -> T {
        todo!()
    }
    fn visit_continue(&mut self, c: &Continue) -> T {
        todo!()
    }
    fn visit_global(&mut self, g: &Global) -> T {
        todo!()
    }
    fn visit_nonlocal(&mut self, n: &Nonlocal) -> T {
        todo!()
    }
    fn visit_if(&mut self, i: &If) -> T {
        todo!()
    }
    fn visit_while(&mut self, w: &While) -> T {
        todo!()
    }
    fn visit_for(&mut self, f: &For) -> T {
        todo!()
    }
    fn visit_with(&mut self, w: &With) -> T {
        todo!()
    }
    fn visit_try(&mut self, t: &Try) -> T {
        todo!()
    }
    fn visit_try_star(&mut self, t: &TryStar) -> T {
        todo!()
    }
    fn visit_function_def(&mut self, f: &FunctionDef) -> T {
        todo!()
    }
    fn visit_class_def(&mut self, c: &ClassDef) -> T {
        todo!()
    }
    fn visit_match(&mut self, m: &Match) -> T {
        todo!()
    }

    fn visit_constant(&mut self, c: &Constant) -> T {
        todo!()
    }
    fn visit_list(&mut self, l: &List) -> T {
        todo!()
    }
    fn visit_tuple(&mut self, t: &Tuple) -> T {
        todo!()
    }
    fn visit_dict(&mut self, d: &Dict) -> T {
        todo!()
    }
    fn visit_set(&mut self, s: &Set) -> T {
        todo!()
    }
    fn visit_name(&mut self, n: &Name) -> T {
        todo!()
    }
    fn visit_bool_op(&mut self, b: &BoolOperation) -> T {
        todo!()
    }
    fn visit_unary_op(&mut self, u: &UnaryOperation) -> T {
        todo!()
    }
    fn visit_bin_op(&mut self, b: &BinOp) -> T {
        todo!()
    }
    fn visit_named_expr(&mut self, n: &NamedExpression) -> T {
        todo!()
    }
    fn visit_yield(&mut self, y: &Yield) -> T {
        todo!()
    }
    fn visit_yield_from(&mut self, y: &YieldFrom) -> T {
        todo!()
    }
    fn visit_starred(&mut self, s: &Starred) -> T {
        todo!()
    }
    fn visit_generator(&mut self, g: &Generator) -> T {
        todo!()
    }
    fn visit_list_comp(&mut self, l: &ListComp) -> T {
        todo!()
    }
    fn visit_set_comp(&mut self, s: &SetComp) -> T {
        todo!()
    }
    fn visit_dict_comp(&mut self, d: &DictComp) -> T {
        todo!()
    }
    fn visit_attribute(&mut self, a: &Attribute) -> T {
        todo!()
    }
    fn visit_subscript(&mut self, s: &Subscript) -> T {
        todo!()
    }
    fn visit_slice(&mut self, s: &Slice) -> T {
        todo!()
    }
    fn visit_call(&mut self, c: &Call) -> T {
        todo!()
    }
    fn visit_await(&mut self, a: &Await) -> T {
        todo!()
    }
    fn visit_compare(&mut self, c: &Compare) -> T {
        todo!()
    }
    fn visit_lambda(&mut self, l: &Lambda) -> T {
        todo!()
    }
    fn visit_if_exp(&mut self, i: &IfExp) -> T {
        todo!()
    }
    fn visit_joined_str(&mut self, j: &JoinedStr) -> T {
        todo!()
    }
    fn visit_formatted_value(&mut self, f: &FormattedValue) -> T {
        todo!()
    }
}
