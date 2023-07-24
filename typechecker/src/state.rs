use crate::{
    ast_visitor::TraversalVisitor, nodes::EnderpyFile, semantic_analyzer::SemanticAnalyzer,
    symbol_table::SymbolTable,
};

pub struct State {
    pub file: Box<EnderpyFile>,
    pub symbol_table: SymbolTable,
}

impl State {
    /// entry point to fill up the symbol table from the global definitions
    pub fn populate_symbol_table(&mut self) {
        let mut sem_anal = SemanticAnalyzer::new(self.file.clone());
        for stmt in &self.file.defs {
            sem_anal.visit_stmt(stmt)
        }
        self.symbol_table = sem_anal.globals
    }
}
