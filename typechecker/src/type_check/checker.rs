use ast::{Expression, Statement};
use enderpy_python_parser as parser;
use enderpy_python_parser::ast::{self, *};

use crate::diagnostic::CharacterSpan;
use crate::symbol_table::LookupSymbolRequest;
use crate::{
    ast_visitor::TraversalVisitor, settings::Settings, state::State, symbol_table::SymbolTable,
};

use super::{type_evaluator::TypeEvaluator, type_inference::type_check_bin_op, types::PythonType};

pub struct TypeChecker<'a> {
    pub errors: Vec<TypeCheckError>,
    // The symbol table of the module being type checked
    symbol_table: SymbolTable,
    pub options: &'a Settings,
    type_evaluator: TypeEvaluator,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeCheckError {
    pub msg: String,
    pub span: CharacterSpan,
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

    fn infer_expr_type(&mut self, expr: &Expression, emit_error: bool) -> PythonType {
        match self.type_evaluator.get_type(expr) {
            Ok(t) => t,
            Err(e) => {
                if emit_error {
                    self.make_error(
                        e.to_string().as_str(),
                        expr.get_node().start,
                        expr.get_node().end,
                    );
                }
                PythonType::Unknown
            }
        }
    }

    fn make_error(&mut self, msg: &str, start: usize, end: usize) {
        let error = TypeCheckError {
            msg: msg.to_string(),
            span: CharacterSpan(start, end),
        };
        // check error doesn't already exist
        for e in &self.errors {
            if e == &error {
                return;
            }
        }
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
            Expression::Name(n) => {
                self.infer_expr_type(e, true);
                self.visit_name(n)
            }
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
            Expression::Call(c) => {
                self.infer_expr_type(e, true);
                self.visit_call(c)
            }
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
        self.visit_expr(&i.test);
        for stmt in &i.body {
            self.visit_stmt(stmt);
        }
        for stmt in &i.orelse {
            self.visit_stmt(stmt);
        }
    }

    fn visit_while(&mut self, w: &parser::ast::While) {
        self.visit_expr(&w.test);
        for stmt in &w.body {
            self.visit_stmt(stmt)
        }
        for stmt in &w.orelse {
            self.visit_stmt(stmt)
        }
    }

    fn visit_for(&mut self, f: &parser::ast::For) {
        self.visit_expr(&f.iter);
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
        for handler in &t.handlers {
            for stmt in &handler.body {
                self.visit_stmt(stmt);
            }
            if let Some(typ) = &handler.typ {
                self.visit_expr(typ);
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
        for handler in &t.handlers {
            for stmt in &handler.body {
                self.visit_stmt(stmt);
            }
            if let Some(typ) = &handler.typ {
                self.visit_expr(typ);
            }
        }
    }

    fn visit_function_def(&mut self, f: &parser::ast::FunctionDef) {
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_class_def(&mut self, c: &parser::ast::ClassDef) {
        for base in &c.bases {
            self.visit_expr(base);
        }
        for dec in &c.decorator_list {
            self.visit_expr(dec);
        }
        for stmt in &c.body {
            self.visit_stmt(stmt);
        }
        for keyword in &c.keywords {
            self.visit_expr(&keyword.value);
        }
    }

    fn visit_match(&mut self, m: &parser::ast::Match) {
        self.visit_expr(&m.subject);
        for case in &m.cases {
            for stmt in &case.body {
                self.visit_stmt(stmt);
            }
            if let Some(guard) = &case.guard {
                self.visit_expr(guard);
            }
            self.visit_match_pattern(&case.pattern);
        }
    }

    fn visit_match_pattern(&mut self, _m: &parser::ast::MatchPattern) {
        match _m {
            MatchPattern::MatchValue(m) => self.visit_expr(&m.value),
            MatchPattern::MatchSingleton(m) => self.visit_expr(m),
            MatchPattern::MatchSequence(m) => {
                for item in m.iter() {
                    self.visit_match_pattern(item);
                }
            }
            MatchPattern::MatchStar(m) => self.visit_expr(m),
            MatchPattern::MatchMapping(m) => {
                for key in &m.keys {
                    self.visit_expr(key);
                }
                for pattern in &m.patterns {
                    self.visit_match_pattern(pattern);
                }
            }
            MatchPattern::MatchAs(m) => {
                if let Some(pattern) = &m.pattern {
                    self.visit_match_pattern(pattern);
                }
            }
            MatchPattern::MatchClass(m) => {
                self.visit_expr(&m.cls);
                for pattern in &m.patterns {
                    self.visit_match_pattern(pattern);
                }
            }
            MatchPattern::MatchOr(m) => {
                for pattern in m.iter() {
                    self.visit_match_pattern(pattern);
                }
            }
        }
    }

    fn visit_constant(&mut self, _c: &Constant) {}

    fn visit_list(&mut self, _l: &List) {
        for expr in &_l.elements {
            self.visit_expr(expr);
        }
    }

    fn visit_tuple(&mut self, _t: &Tuple) {
        for expr in &_t.elements {
            self.visit_expr(expr);
        }
    }

    fn visit_dict(&mut self, _d: &Dict) {
        for key in &_d.keys {
            self.visit_expr(key);
        }

        for values in &_d.values {
            self.visit_expr(values);
        }
    }

    fn visit_set(&mut self, _s: &Set) {
        for expr in &_s.elements {
            self.visit_expr(expr);
        }
    }

    fn visit_name(&mut self, _n: &Name) {}

    fn visit_bool_op(&mut self, _b: &BoolOperation) {
        for expr in &_b.values {
            self.visit_expr(expr);
        }
    }

    fn visit_unary_op(&mut self, _u: &UnaryOperation) {
        self.visit_expr(&_u.operand);
    }

    fn visit_bin_op(&mut self, b: &BinOp) {
        let l_type = self.infer_expr_type(&b.left, true);
        let r_type = self.infer_expr_type(&b.right, true);

        if !type_check_bin_op(&l_type, &r_type, &b.op) {
            let msg = format!(
                "Operator '{}' not supported for types '{}' and '{}'",
                b.op, l_type, r_type
            );
            self.errors.push(TypeCheckError {
                msg,
                span: CharacterSpan(b.left.get_node().start, b.right.get_node().end),
            });
        }
    }

    fn visit_named_expr(&mut self, _n: &NamedExpression) {
        self.visit_expr(&_n.value);
        self.visit_expr(&_n.target);
    }

    fn visit_yield(&mut self, _y: &Yield) {
        if let Some(expr) = &_y.value {
            self.visit_expr(expr);
        }
    }

    fn visit_yield_from(&mut self, _y: &YieldFrom) {
        self.visit_expr(&_y.value);
    }

    fn visit_starred(&mut self, _s: &Starred) {
        self.visit_expr(&_s.value);
    }

    fn visit_generator(&mut self, _g: &Generator) {
        self.visit_expr(&_g.element);
        for comprehension in &_g.generators {
            self.visit_expr(&comprehension.iter);
            for if_expr in &comprehension.ifs {
                self.visit_expr(if_expr);
            }
        }
    }

    fn visit_list_comp(&mut self, _l: &ListComp) {
        self.visit_expr(&_l.element);
        for comprehension in &_l.generators {
            self.visit_expr(&comprehension.iter);
            for if_expr in &comprehension.ifs {
                self.visit_expr(if_expr);
            }
        }
    }

    fn visit_set_comp(&mut self, _s: &SetComp) {
        self.visit_expr(&_s.element);
        for comprehension in &_s.generators {
            self.visit_expr(&comprehension.iter);
            for if_expr in &comprehension.ifs {
                self.visit_expr(if_expr);
            }
        }
    }

    fn visit_dict_comp(&mut self, _d: &DictComp) {
        self.visit_expr(&_d.key);
        self.visit_expr(&_d.value);
        for comprehension in &_d.generators {
            self.visit_expr(&comprehension.iter);
            for if_expr in &comprehension.ifs {
                self.visit_expr(if_expr);
            }
        }
    }

    fn visit_attribute(&mut self, _a: &Attribute) {
        self.visit_expr(&_a.value);
    }

    fn visit_subscript(&mut self, _s: &Subscript) {
        self.visit_expr(&_s.slice);
        self.visit_expr(&_s.value);
    }

    fn visit_slice(&mut self, _s: &Slice) {
        if let Some(lower) = &_s.lower {
            self.visit_expr(lower);
        }
        if let Some(upper) = &_s.upper {
            self.visit_expr(upper);
        }
        if let Some(step) = &_s.step {
            self.visit_expr(step);
        }
    }

    fn visit_call(&mut self, _c: &Call) {
        for arg in &_c.args {
            self.visit_expr(arg);
        }
        for keyword in &_c.keywords {
            self.visit_expr(&keyword.value);
        }
    }

    fn visit_await(&mut self, _a: &Await) {
        self.visit_expr(&_a.value);
    }

    fn visit_compare(&mut self, _c: &Compare) {
        self.visit_expr(&_c.left);
        for comprators in &_c.comparators {
            self.visit_expr(comprators);
        }
    }

    fn visit_lambda(&mut self, _l: &Lambda) {
        self.visit_expr(&_l.body);
        // todo: this can share a visit args with functions
    }

    fn visit_if_exp(&mut self, _i: &IfExp) {
        self.visit_expr(&_i.body);
        self.visit_expr(&_i.test);
        self.visit_expr(&_i.orelse);
    }

    fn visit_joined_str(&mut self, _j: &JoinedStr) {
        for value in &_j.values {
            self.visit_expr(value);
        }
    }

    fn visit_formatted_value(&mut self, _f: &FormattedValue) {
        self.visit_expr(&_f.value);
    }

    fn visit_alias(&mut self, _a: &Alias) {}

    fn visit_assign(&mut self, _a: &Assign) {
        self.visit_expr(&_a.value);
        for target in &_a.targets {
            match target {
                ast::Expression::Name(n) => {
                    let lookup_request = LookupSymbolRequest {
                        name: n.id.clone(),
                        position: None,
                    };
                    let symbol = self.symbol_table.lookup_in_scope(lookup_request);
                    if let Some(symbol) = symbol {
                        let prev_target_type = self
                            .type_evaluator
                            .get_symbol_node_type(symbol, Some(n.node.start))
                            .unwrap_or(PythonType::Unknown);
                        let value_type = self.infer_expr_type(&_a.value, true);
                        // TODO: Check reassignment
                    }
                }
                _ => {}
            }
        }
    }
    fn visit_ann_assign(&mut self, _a: &AnnAssign) {
        if let Some(value) = &_a.value {
            self.visit_expr(value);
        }
    }

    fn visit_aug_assign(&mut self, _a: &AugAssign) {
        self.visit_expr(&_a.value);
        self.visit_expr(&_a.target);
    }

    fn visit_assert(&mut self, _a: &Assert) {
        self.visit_expr(&_a.test);
        if let Some(msg) = &_a.msg {
            self.visit_expr(msg);
        }
    }

    fn visit_pass(&mut self, _p: &Pass) {}

    fn visit_delete(&mut self, _d: &Delete) {
        for target in &_d.targets {
            self.visit_expr(target);
        }
    }

    fn visit_return(&mut self, _r: &Return) {
        if let Some(value) = &_r.value {
            self.visit_expr(value);
        }
    }

    fn visit_raise(&mut self, _r: &Raise) {
        if let Some(exc) = &_r.exc {
            self.visit_expr(exc);
        }
        if let Some(cause) = &_r.cause {
            self.visit_expr(cause);
        }
    }

    fn visit_break(&mut self, _b: &Break) {}

    fn visit_continue(&mut self, _c: &Continue) {}

    fn visit_global(&mut self, _g: &Global) {}

    fn visit_nonlocal(&mut self, _n: &Nonlocal) {}
}
