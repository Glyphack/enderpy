use std::path::PathBuf;
use std::sync::Arc;

use ast::{Expression, Statement};
use dashmap::DashMap;
use enderpy_python_parser as parser;
use enderpy_python_parser::ast::{self, *};

use super::{type_evaluator::TypeEvaluator, types::PythonType};
use crate::symbol_table::Id;
use crate::types::ModuleRef;
use crate::{ast_visitor::TraversalVisitor, diagnostic::CharacterSpan, symbol_table::SymbolTable};
use rust_lapper::{Interval, Lapper};

#[derive(Clone, Debug)]
pub struct TypeChecker<'a> {
    pub errors: Vec<TypeCheckError>,
    pub types: Lapper<u32, PythonType>,
    type_evaluator: TypeEvaluator<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeCheckError {
    pub msg: String,
    pub span: CharacterSpan,
}

#[allow(unused)]
impl<'a> TypeChecker<'a> {
    pub fn new(
        symbol_table: SymbolTable,
        symbol_tables: &'a DashMap<Id, SymbolTable>,
        ids: &'a DashMap<PathBuf, Id>,
    ) -> Self {
        TypeChecker {
            errors: vec![],
            type_evaluator: TypeEvaluator::new(symbol_table, symbol_tables, ids),
            types: Lapper::new(vec![]),
        }
    }

    pub fn type_check(&mut self, statement: &Statement) {
        self.visit_stmt(statement);
    }

    fn infer_expr_type(&mut self, expr: &Expression) -> PythonType {
        let t = match self.type_evaluator.get_type(expr, None, None) {
            Ok(t) => t,
            Err(e) => {
                log::error!("type evaluator error: {} for expr {expr:?}", e);
                PythonType::Unknown
            }
        };

        self.types.insert(Interval {
            start: expr.get_node().start,
            stop: expr.get_node().end,
            val: t.clone(),
        });
        t
    }

    fn infer_name_type(&mut self, name: &str, start: u32, stop: u32) {
        let name_type = self
            .type_evaluator
            .get_name_type(
                name,
                None,
                &self.type_evaluator.symbol_table,
                Some(self.type_evaluator.symbol_table.current_scope_id),
            )
            .unwrap_or(PythonType::Unknown);
        self.types.insert(Interval {
            start,
            stop,
            val: name_type,
        });
    }

    fn make_error(&mut self, msg: &str, start: u32, end: u32) {
        let error = TypeCheckError {
            msg: msg.to_string(),
            span: CharacterSpan(start as usize, end as usize),
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
        self.infer_expr_type(e);
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
        for name in _i.names.iter() {
            self.infer_name_type(&name.name, name.node.start, name.node.end);
        }
    }

    fn visit_import_from(&mut self, _i: &ImportFrom) {
        for alias in _i.names.iter() {
            self.infer_name_type(&alias.name, alias.node.start, alias.node.end)
        }

        // Just to show type module when modules are hovered in imports.
        let start = _i.node.start + 5;
        let stop = start + _i.module.len() as u32 + 1;
        self.types.insert(Interval {
            start,
            stop,
            // TODO: this is not the actual type.
            val: PythonType::Module(ModuleRef { module_id: Id(0) }),
        });
    }

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

    fn visit_function_def(&mut self, f: &Arc<parser::ast::FunctionDef>) {
        self.infer_name_type(
            &f.name,
            f.node.start + 4,
            f.node.start + 4 + f.name.len() as u32,
        );
        if let Some(ret_type) = &f.returns {
            self.visit_expr(ret_type);
        }
        self.type_evaluator.symbol_table.set_scope(f.node.start);
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
        for (arg, _index) in f.args.args.iter().zip(0..) {
            if let Some(annotation) = &arg.annotation {
                self.infer_expr_type(annotation);
            }
            self.infer_name_type(&arg.arg, arg.node.start, arg.node.end)
        }
        self.type_evaluator.symbol_table.revert_scope();
    }

    fn visit_async_function_def(&mut self, f: &Arc<parser::ast::AsyncFunctionDef>) {
        self.infer_name_type(
            &f.name,
            f.node.start + 9,
            f.node.start + 9 + f.name.len() as u32,
        );
        self.type_evaluator.symbol_table.set_scope(f.node.start);
        for stmt in &f.body {
            self.visit_stmt(stmt);
        }
        self.type_evaluator.symbol_table.revert_scope();
    }

    fn visit_class_def(&mut self, c: &Arc<parser::ast::ClassDef>) {
        self.infer_name_type(
            &c.name,
            c.node.start + 6,
            c.node.start + 6 + c.name.len() as u32,
        );

        self.type_evaluator.symbol_table.set_scope(c.node.start);
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

        self.type_evaluator.symbol_table.revert_scope();
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
        let l_type = self.infer_expr_type(&b.left);
        let r_type = self.infer_expr_type(&b.right);
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

    fn visit_attribute(&mut self, a: &Attribute) {
        self.infer_expr_type(&a.value);
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

    fn visit_call(&mut self, c: &Call) {
        self.infer_expr_type(&c.func);
        for arg in &c.args {
            self.visit_expr(arg);
        }
        for keyword in &c.keywords {
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

    fn visit_assign(&mut self, a: &Assign) {
        self.visit_expr(&a.value);
        for target in &a.targets {
            self.visit_expr(target);
        }
    }
    fn visit_ann_assign(&mut self, _a: &AnnAssign) {
        if let Some(value) = &_a.value {
            self.visit_expr(value);
        }
        self.infer_expr_type(&_a.target);
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

    fn visit_global(&mut self, _g: &Global) {
        let mut cur_offset = _g.node.start + 6;
        for name in _g.names.iter() {
            self.infer_name_type(name, _g.node.start, _g.node.end);
            cur_offset += name.len() as u32;
        }
    }

    fn visit_nonlocal(&mut self, _n: &Nonlocal) {}
}

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};

    use crate::{build::BuildManager, settings::Settings};

    fn snapshot_type_checker_types(path: PathBuf) -> String {
        let settings = Settings::test_settings();
        let manager = BuildManager::new(settings);
        let root = &PathBuf::from("");
        manager.build(root);
        manager.build_one(root, &path);
        let checker = manager.type_check(&path);
        let module = manager.get_state(&path);

        let result = checker.types;

        // sort result by key
        let mut str = String::new();
        let mut last_line = None;

        for r in result {
            let pos = module.get_position(r.start, r.stop);
            let cur_line = pos.line;

            if last_line.is_none() {
                let line_content = module.get_line_content(cur_line as usize);
                str.push_str(format!("Line {}: {}", cur_line, line_content).as_str());
                str.push_str("\nExpr types in the line --->:\n");
                last_line = Some(cur_line);
            }
            // So we also print the first line
            if let Some(last_line_num) = last_line {
                if last_line_num < cur_line {
                    str.push_str("\n---\n");
                    let line_content = module.get_line_content(cur_line as usize);
                    str.push_str(format!("Line {}: {}", cur_line, line_content).as_str());
                    str.push_str("\nExpr types in the line --->:\n");
                    last_line = Some(cur_line);
                }
            }
            let source_text = &module.source[r.start as usize..r.stop as usize];
            str.push_str(&format!("        {} => {}\n", source_text, r.val))
        }
        str.push_str("\n---\n");

        str
    }

    macro_rules! type_eval_test {
        ($test_name:ident, $test_file:expr) => {
            #[test]
            fn $test_name() {
                let path = PathBuf::from($test_file);
                let contents = fs::read_to_string(&path).unwrap();
                let result = snapshot_type_checker_types(path);

                let mut content_with_line_numbers = String::new();
                for (i, line) in contents.lines().enumerate() {
                    content_with_line_numbers.push_str(&format!("{}: {}\n", i + 1, line));
                }

                // TODO move this redaction setting to a central place
                let mut settings = insta::Settings::clone_current();
                settings.add_filter(r"module_name: .*.typeshed.", "module_name: [TYPESHED].");
                settings.set_snapshot_path("../test_data/output/");
                settings.set_description(content_with_line_numbers);
                // TODO CLEAN THE classes name
                settings.bind(|| {
                    insta::assert_snapshot!(result);
                });
            }
        };
    }

    type_eval_test!(basic_types, "test_data/inputs/basic_types.py");
    type_eval_test!(basic_generics, "test_data/inputs/basic_generics.py");
    type_eval_test!(import_star_lookup, "test_data/inputs/import_star_test/a.py");
    type_eval_test!(
        annotations_coroutine,
        "test_data/inputs/conformance_tests/annotations_coroutine.py"
    );
}
