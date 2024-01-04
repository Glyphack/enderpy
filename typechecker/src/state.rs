use std::collections::HashMap;

use crate::{
    ast_visitor::TraversalVisitor,
    diagnostic::Diagnostic,
    nodes::EnderpyFile,
    ruff_python_import_resolver::{
        import_result::ImportResult, module_descriptor::ImportModuleDescriptor,
    },
    semantic_analyzer::SemanticAnalyzer,
    symbol_table::SymbolTable,
};

#[derive(Debug, Clone)]
pub struct State {
    pub file: EnderpyFile,
    symbol_table: SymbolTable,
    pub diagnostics: Vec<Diagnostic>,
}

impl State {
    pub fn new(file: EnderpyFile) -> Self {
        let symbol_table = SymbolTable::global(file.clone());
        Self {
            file,
            symbol_table,
            diagnostics: Vec::new(),
        }
    }
    /// entry point to fill up the symbol table from the global definitions
    pub fn populate_symbol_table(
        &mut self,
        imports: &HashMap<ImportModuleDescriptor, ImportResult>,
    ) {
        let mut sem_anal = SemanticAnalyzer::new(self.file.clone(), imports.clone());
        for stmt in &self.file.body {
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
