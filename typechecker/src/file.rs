use core::panic;
use std::path::Path;
use std::{collections::HashMap, path::PathBuf};

use enderpy_python_parser as parser;
use enderpy_python_parser::ast::{Import, ImportFrom, Statement};
use parser::Parser;

use crate::checker::TypeChecker;
use crate::get_module_name;
use crate::{
    ast_visitor::TraversalVisitor,
    diagnostic::Position,
    ruff_python_import_resolver::{
        import_result::ImportResult, module_descriptor::ImportModuleDescriptor,
    },
    semantic_analyzer::SemanticAnalyzer,
    symbol_table::SymbolTable,
};

#[derive(Clone, Debug)]
pub enum ImportKinds {
    Import(Import),
    ImportFrom(ImportFrom),
}

/// EnderpyFile holds information about the files in the analyze
/// and methods to perform semantic analysis and type check on them
#[derive(Clone, Debug)]
pub struct EnderpyFile {
    pub source: String,
    pub module: String,
    // if this source is found by following an import
    pub followed: bool,
    pub path: PathBuf,
    // all the imports inside the file
    pub imports: Vec<ImportKinds>,
    // All high level statements inside the file
    pub body: Vec<Statement>,
    pub parser: Parser,
    // Available after populating
    pub symbol_table: Option<SymbolTable>,
    pub type_checker: Option<TypeChecker>,
}

impl EnderpyFile {
    pub fn new(path: &Path, followed: bool) -> Self {
        let source =
            std::fs::read_to_string(path).unwrap_or_else(|_| panic!("cannot read file {path:?}"));
        let module = get_module_name(path);
        let mut parser = Parser::new(source.clone(), path.to_str().unwrap().to_string());
        let tree = parser.parse();

        let mut file = Self {
            imports: vec![],
            body: vec![],
            symbol_table: None,
            type_checker: None,
            parser,
            followed,
            module,
            path: path.to_path_buf(),
            source,
        };

        for stmt in &tree.body {
            file.visit_stmt(stmt);
            file.body.push(stmt.clone());
        }

        file
    }
    pub fn module_name(&self) -> String {
        self.module.clone()
    }

    pub fn path(&self) -> PathBuf {
        self.path.to_path_buf()
    }

    pub fn path_str(&self) -> String {
        self.path.to_str().unwrap().to_string()
    }

    pub fn source(&self) -> String {
        self.source.clone()
    }

    /// Return source of the line number
    pub fn get_line_content(&self, line: usize) -> String {
        let line_starts = self.parser.line_starts();
        let line_start_offset = line_starts[line] as usize;
        let line_end_offset = if line == line_starts.len() {
            self.source.len()
        } else {
            line_starts[line + 1] as usize
        };
        self.source[line_start_offset..line_end_offset].to_string()
    }

    pub fn get_position(&self, pos: u32) -> Position {
        let line_starts = self.parser.line_starts();
        let line_number = match line_starts.binary_search(&pos) {
            Ok(index) => index,
            Err(index) => {
                if index == 0 {
                    index
                } else {
                    index - 1
                }
            }
        };
        let line_start = line_starts[line_number];
        Position {
            line: line_number as u32,
            character: (pos - line_start),
        }
    }

    /// entry point to fill up the symbol table from the global definitions
    pub fn populate_symbol_table(
        &mut self,
        imports: HashMap<ImportModuleDescriptor, ImportResult>,
    ) {
        let mut sem_anal = SemanticAnalyzer::new(self.clone(), imports.clone());
        for stmt in &self.body {
            sem_anal.visit_stmt(stmt)
        }
        let mut sym_table = sem_anal.symbol_table;
        sym_table.current_scope_id = 0;
        self.symbol_table = Some(sym_table);
    }

    pub fn type_check(&mut self, symbol_tables: Vec<SymbolTable>) {
        let mut checker = TypeChecker::new(self.clone(), symbol_tables);
        for stmt in &self.body {
            checker.type_check(stmt);
        }
        self.type_checker = Some(checker);
    }

    pub fn get_checker(&self) -> TypeChecker {
        self.type_checker
            .clone()
            .expect("Accessing type checker before type check")
    }

    pub fn get_symbol_table(&self) -> SymbolTable {
        self.symbol_table
            .clone()
            .expect("Accessing symbol table before initialize")
    }
}

impl TraversalVisitor for EnderpyFile {
    fn visit_stmt(&mut self, s: &Statement) {
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

    fn visit_import(&mut self, i: &Import) {
        let import = i.clone();
        self.imports.push(ImportKinds::Import(import));
    }

    fn visit_import_from(&mut self, i: &ImportFrom) {
        let import = i.clone();
        self.imports.push(ImportKinds::ImportFrom(import));
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

    fn visit_function_def(&mut self, _f: &parser::ast::FunctionDef) {}

    fn visit_class_def(&mut self, _c: &parser::ast::ClassDef) {}

    fn visit_match(&mut self, m: &parser::ast::Match) {
        for case in &m.cases {
            for stmt in &case.body {
                self.visit_stmt(stmt);
            }
        }
    }

    fn visit_constant(&mut self, _c: &parser::ast::Constant) {}

    fn visit_list(&mut self, _l: &parser::ast::List) {}

    fn visit_tuple(&mut self, _t: &parser::ast::Tuple) {}

    fn visit_dict(&mut self, _d: &parser::ast::Dict) {}

    fn visit_set(&mut self, _s: &parser::ast::Set) {}

    fn visit_name(&mut self, _n: &parser::ast::Name) {}

    fn visit_bool_op(&mut self, _b: &parser::ast::BoolOperation) {}

    fn visit_unary_op(&mut self, _u: &parser::ast::UnaryOperation) {}

    fn visit_bin_op(&mut self, _b: &parser::ast::BinOp) {}

    fn visit_named_expr(&mut self, _n: &parser::ast::NamedExpression) {}

    fn visit_yield(&mut self, _y: &parser::ast::Yield) {}

    fn visit_yield_from(&mut self, _y: &parser::ast::YieldFrom) {}

    fn visit_starred(&mut self, _s: &parser::ast::Starred) {}

    fn visit_generator(&mut self, _g: &parser::ast::Generator) {}

    fn visit_list_comp(&mut self, _l: &parser::ast::ListComp) {}

    fn visit_set_comp(&mut self, _s: &parser::ast::SetComp) {}

    fn visit_dict_comp(&mut self, _d: &parser::ast::DictComp) {}

    fn visit_attribute(&mut self, _a: &parser::ast::Attribute) {}

    fn visit_subscript(&mut self, _s: &parser::ast::Subscript) {}

    fn visit_slice(&mut self, _s: &parser::ast::Slice) {}

    fn visit_call(&mut self, _c: &parser::ast::Call) {}

    fn visit_await(&mut self, _a: &parser::ast::Await) {}

    fn visit_compare(&mut self, _c: &parser::ast::Compare) {}

    fn visit_lambda(&mut self, _l: &parser::ast::Lambda) {}

    fn visit_if_exp(&mut self, _i: &parser::ast::IfExp) {}

    fn visit_joined_str(&mut self, _j: &parser::ast::JoinedStr) {}

    fn visit_formatted_value(&mut self, _f: &parser::ast::FormattedValue) {}

    fn visit_alias(&mut self, _a: &parser::ast::Alias) {}

    fn visit_assign(&mut self, _a: &parser::ast::Assign) {}

    fn visit_ann_assign(&mut self, _a: &parser::ast::AnnAssign) {}

    fn visit_aug_assign(&mut self, _a: &parser::ast::AugAssign) {}

    fn visit_assert(&mut self, _a: &parser::ast::Assert) {}

    fn visit_pass(&mut self, _p: &parser::ast::Pass) {}

    fn visit_delete(&mut self, _d: &parser::ast::Delete) {}

    fn visit_return(&mut self, _r: &parser::ast::Return) {}

    fn visit_raise(&mut self, _r: &parser::ast::Raise) {}

    fn visit_break(&mut self, _b: &parser::ast::Break) {}

    fn visit_continue(&mut self, _c: &parser::ast::Continue) {}

    fn visit_global(&mut self, _g: &parser::ast::Global) {}

    fn visit_nonlocal(&mut self, _n: &parser::ast::Nonlocal) {}

    fn visit_type_alias(&mut self, _t: &parser::ast::TypeAlias) {}
}
