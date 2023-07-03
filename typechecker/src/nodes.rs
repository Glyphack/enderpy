// This file holds nodes that represent the AST of a file.
// This holds the same information as the parser AST, but
// in a more convenient format for the type checker.
// NOTE: at this point we're not sure what nodes need to be
// here, so this has the minimum amount of nodes needed to
// get the type checker working. But can be expanded.

use std::path::PathBuf;

use parser::{
    ast::{Import, ImportFrom, Module, Statement},
    Parser,
};

use crate::{
    ast_visitor::TraversalVisitor,
    symbol_table::{SymbolTable, SymbolTableType},
};

#[derive(Debug, Clone)]
pub enum ImportKinds {
    Import(Import),
    ImportFrom(ImportFrom),
}

pub struct EnderpyFile {
    pub ast: Module,
    pub names: SymbolTable,
    // all the imports inside the file
    pub imports: Vec<ImportKinds>,
    // highlevel definitions inside the file
    pub defs: Vec<Statement>,
}

impl EnderpyFile {
    pub fn new(path: PathBuf) -> Self {
        let mut converter = ASTConverter::new();
        converter.convert(&path)
    }
}

/// Converts python AST to Enderpy file. This is a high level structure used by
/// the rest of type checker components.
/// This has many reponsibilities and tightly coupled to EnderpyFile.
struct ASTConverter {
    imports: Vec<ImportKinds>,
    defs: Vec<Statement>,
}

impl ASTConverter {
    pub fn new() -> Self {
        return ASTConverter {
            imports: vec![],
            defs: vec![],
        };
    }
}

impl ASTConverter {
    fn convert(&mut self, path: &PathBuf) -> EnderpyFile {
        let source = std::fs::read_to_string(&path).unwrap();
        let mut parser = Parser::new(source);
        let tree = parser.parse();
        for stmt in &tree.body {
            self.visit_stmt(stmt);
        }

        EnderpyFile {
            ast: tree,
            names: SymbolTable::new(SymbolTableType::Module, 1),
            defs: self.defs.clone(),
            imports: self.imports.clone(),
        }
    }
}

impl TraversalVisitor for ASTConverter {
    fn visit_import(&mut self, i: &Import) {
        let import = i.clone();
        self.imports.push(ImportKinds::Import(import));
    }

    fn visit_import_from(&mut self, i: &ImportFrom) {
        let import = i.clone();
        self.imports.push(ImportKinds::ImportFrom(import));
    }

    fn visit_function_def(&mut self, f: &parser::ast::FunctionDef) {
        let func = f.clone();
        self.defs.push(Statement::FunctionDef(func));
    }
}
