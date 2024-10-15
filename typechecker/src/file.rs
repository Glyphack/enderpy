use core::panic;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;

use crate::ast_visitor::TraversalVisitor;
use enderpy_python_parser as parser;
use enderpy_python_parser::ast::*;
use enderpy_python_parser::intern::Interner;
use parser::{ast, get_row_col_position, parser::parser::Parser};
use std::sync::atomic::Ordering;

use crate::build::ResolvedImports;
use crate::{diagnostic::Position, semantic_analyzer::SemanticAnalyzer, symbol_table::SymbolTable};
use crate::{get_module_name, symbol_table};

#[derive(Clone, Debug)]
pub enum ImportKinds<'a> {
    Import(&'a Import),
    ImportFrom(&'a ImportFrom),
}

/// EnderpyFile<'a>holds information about the files in the analyze
/// and methods to perform semantic analysis and type check on them
#[derive(Clone, Debug)]
pub struct EnderpyFile {
    pub id: symbol_table::Id,
    pub module: String,
    // if this source is found by following an import
    pub followed: bool,
    pub path: Arc<PathBuf>,
    pub source: String,
    pub line_starts: Vec<u32>,
    pub tree: ast::Module,
    pub interner: Interner,
}

impl<'a> Eq for EnderpyFile {}

impl<'a> PartialEq for EnderpyFile {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.path == other.path
    }
}

impl<'a> std::hash::Hash for EnderpyFile {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.path.hash(state);
    }
}

fn get_id() -> u32 {
    static COUNTER: AtomicUsize = AtomicUsize::new(1);
    COUNTER.fetch_add(1, Ordering::SeqCst) as u32
}

impl<'a> EnderpyFile {
    pub fn new(path: PathBuf, followed: bool) -> Self {
        let source =
            std::fs::read_to_string(&path).unwrap_or_else(|_| panic!("cannot read file {path:?}"));
        let module = get_module_name(&path);

        let mut parser = Parser::new(&source);
        let parse_result = catch_unwind(AssertUnwindSafe(|| parser.parse()));
        let tree = match parse_result {
            Ok(Ok(module)) => module,
            Ok(Err(_)) => {
                panic!("Cannot parse file : {path:?}");
            }
            Err(_) => {
                panic!("Cannot parse file : {path:?}");
            }
        };
        let line_starts = parser.lexer.line_starts.clone();
        let interner = parser.interner;

        let id = if path.ends_with("builtins.pyi") {
            symbol_table::Id(0)
        } else {
            symbol_table::Id(get_id())
        };

        Self {
            id,
            source,
            line_starts,
            followed,
            module,
            tree,
            path: Arc::new(path),
            interner,
        }
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

    /// Return source of the line number
    pub fn get_line_content(&self, line: usize) -> String {
        let line_starts = &self.line_starts;
        let line_start_offset = line_starts[line - 1] as usize;
        let line_end_offset = if line == line_starts.len() {
            self.source.len()
        } else {
            line_starts[line] as usize
        };
        self.source[line_start_offset..line_end_offset].to_string()
    }

    pub fn get_imports(&self) -> Vec<ImportKinds> {
        let imports = vec![];
        let mut import_collector = ImportCollector { imports };
        for s in self.tree.body.iter() {
            import_collector.visit_stmt(s);
        }
        import_collector.imports
    }

    pub fn get_position(&self, start: u32, end: u32) -> Position {
        let (start_line_num, start_line_column, _end_line_num, _end_line_column) =
            get_row_col_position(start, end, &self.line_starts);
        Position {
            line: start_line_num,
            character: (end - start_line_column),
        }
    }

    /// entry point to fill up the symbol table from the global definitions
    pub fn populate_symbol_table(&mut self, imports: &ResolvedImports) -> SymbolTable {
        let mut sem_anal = SemanticAnalyzer::new(self, imports);
        for stmt in self.tree.body.iter() {
            sem_anal.visit_stmt(stmt)
        }
        let mut sym_table = sem_anal.symbol_table;
        sym_table.current_scope_id = 0;
        sym_table
    }
}

struct ImportCollector<'a> {
    imports: Vec<ImportKinds<'a>>,
}

impl<'a> ImportCollector<'a> {
    fn visit_stmt(&mut self, s: &'a Statement) {
        match s {
            Statement::Import(i) => self.imports.push(ImportKinds::Import(i)),
            Statement::ImportFrom(i) => self.imports.push(ImportKinds::ImportFrom(i)),
            Statement::IfStatement(i) => {
                for stmt in &i.body {
                    self.visit_stmt(stmt);
                }
                for stmt in &i.orelse {
                    self.visit_stmt(stmt);
                }
            }
            Statement::WhileStatement(w) => {
                for stmt in &w.body {
                    self.visit_stmt(stmt);
                }
            }
            Statement::ForStatement(f) => {
                for stmt in &f.body {
                    self.visit_stmt(stmt);
                }
            }
            Statement::AsyncForStatement(f) => {
                for stmt in &f.body {
                    self.visit_stmt(stmt);
                }
            }
            Statement::WithStatement(w) => {
                for stmt in &w.body {
                    self.visit_stmt(stmt);
                }
            }
            Statement::AsyncWithStatement(w) => {
                for stmt in &w.body {
                    self.visit_stmt(stmt);
                }
            }
            Statement::TryStatement(t) => {
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
                }
            }
            Statement::TryStarStatement(t) => {
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
                }
            }
            Statement::FunctionDef(f) => {
                for s in f.body.iter() {
                    self.visit_stmt(s);
                }
            }
            Statement::AsyncFunctionDef(f) => {
                for s in f.body.iter() {
                    self.visit_stmt(s);
                }
            }
            Statement::ClassDef(c) => {
                for s in c.body.iter() {
                    self.visit_stmt(s);
                }
            }
            Statement::MatchStmt(m) => {
                for case in &m.cases {
                    for stmt in &case.body {
                        self.visit_stmt(stmt);
                    }
                }
            }
            _ => {}
        };
    }
}
