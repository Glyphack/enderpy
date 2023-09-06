use crate::{
    ast_visitor::TraversalVisitor, nodes::EnderpyFile, semantic_analyzer::SemanticAnalyzer,
    symbol_table::SymbolTable,
};

pub struct State {
    pub file: Box<EnderpyFile>,
    symbol_table: SymbolTable,
}

impl State {
    pub fn new(file: Box<EnderpyFile>) -> Self {
        Self {
            file,
            symbol_table: SymbolTable::new(crate::symbol_table::SymbolTableType::Module, 0),
        }
    }
    /// entry point to fill up the symbol table from the global definitions
    pub fn populate_symbol_table(&mut self) {
        let mut sem_anal = SemanticAnalyzer::new(self.file.clone());
        for stmt in &self.file.defs {
            sem_anal.visit_stmt(stmt)
        }
        self.symbol_table = sem_anal.globals
    }

    pub fn get_symbol_table(&self) -> SymbolTable {
        self.symbol_table.clone()
    }
}
