// This file holds nodes that represent the AST of a file.
// This holds the same information as the parser AST, but
// in a more convenient format for the type checker.
// NOTE: at this point we're not sure what nodes need to be
// here, so this has the minimum amount of nodes needed to
// get the type checker working. But can be expanded.

use std::{collections::HashMap, path::PathBuf};

use enderpy_python_parser as parser;
use enderpy_python_parser::ast::{Import, ImportFrom, Statement};
use parser::{error::ParsingError, Parser};

use crate::{
    ast_visitor::TraversalVisitor,
    build_source::BuildSource,
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

#[derive(Clone, Debug)]
pub struct EnderpyFile {
    // all the imports inside the file
    pub imports: Vec<ImportKinds>,
    // high level definitions inside the file
    pub defs: Vec<Statement>,

    // All high level statements inside the file
    pub body: Vec<Statement>,
    pub build_source: BuildSource,
    // Parser Errors
    pub errors: Vec<ParsingError>,
    pub symbol_table: SymbolTable,
}

impl EnderpyFile {
    pub fn module_name(&self) -> String {
        self.build_source.module.clone()
    }

    pub fn path(&self) -> PathBuf {
        self.build_source.path.clone()
    }

    pub fn source(&self) -> String {
        self.build_source.source.clone()
    }

    /// Return source of the line number
    pub fn get_line_content(&self, line: u32) -> String {
        let mut line_number = 1;
        let mut line_start = 0;
        for (i, c) in self.build_source.source.chars().enumerate() {
            if line_number == line {
                line_start = i;
                break;
            }
            if c == '\n' {
                line_number += 1;
            }
        }

        let mut line_end = line_start;

        for (i, c) in self.build_source.source.chars().enumerate() {
            if i > line_start {
                if c == '\n' {
                    line_end = i;
                    break;
                }
            }
        }

        self.build_source.source[line_start..line_end].to_string()
    }

    pub fn get_position(&self, pos: usize) -> Position {
        let mut line_number = 1;
        let mut line_start = 0;
        if pos == 0 {
            return Position {
                line: 1,
                character: 0,
            };
        }
        for (i, c) in self.build_source.source.chars().enumerate() {
            if i == pos {
                break;
            }
            if c == '\n' {
                line_number += 1;
                line_start = i;
            }
        }
        Position {
            line: line_number,
            character: (pos - line_start - 1) as u32,
        }
    }

    /// entry point to fill up the symbol table from the global definitions
    pub fn populate_symbol_table(
        &mut self,
        imports: &HashMap<ImportModuleDescriptor, ImportResult>,
    ) {
        let mut sem_anal = SemanticAnalyzer::new(self.clone(), imports.clone());
        for stmt in &self.body {
            sem_anal.visit_stmt(stmt)
        }
        // TODO: Hacky way to add the global scope to all scopes in symbol table after
        // finishing
        sem_anal.globals.exit_scope();
        self.symbol_table = sem_anal.globals;
    }

    pub fn get_symbol_table(&self) -> SymbolTable {
        self.symbol_table.clone()
    }
}

impl From<BuildSource> for EnderpyFile {
    fn from(build_source: BuildSource) -> Self {
        let mut parser = Parser::new(
            build_source.source.clone(),
            build_source.path.as_path().to_str().unwrap().to_owned(),
        );
        let tree = parser.parse();
        let symbol_table =
            SymbolTable::global(build_source.module.clone(), build_source.path.clone());

        let mut file = EnderpyFile {
            defs: vec![],
            imports: vec![],
            body: vec![],
            build_source,
            errors: parser.errors,
            symbol_table,
        };

        for stmt in &tree.body {
            file.visit_stmt(stmt);
            file.body.push(stmt.clone());
        }

        file
    }
}

impl TraversalVisitor for EnderpyFile {
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

    fn visit_function_def(&mut self, f: &parser::ast::FunctionDef) {
        let func = f.clone();
        self.defs.push(Statement::FunctionDef(func));
    }

    fn visit_class_def(&mut self, c: &parser::ast::ClassDef) {
        let class = c.clone();
        self.defs.push(Statement::ClassDef(class));
    }

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

    fn visit_assign(&mut self, a: &parser::ast::Assign) {
        let stmt = a.clone();
        self.defs.push(Statement::AssignStatement(stmt));
    }

    fn visit_ann_assign(&mut self, a: &parser::ast::AnnAssign) {
        let stmt = a.clone();
        self.defs.push(Statement::AnnAssignStatement(stmt));
    }

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
