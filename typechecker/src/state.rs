use crate::{
    ast_visitor::TraversalVisitor, nodes::EnderpyFile, semantic_analyzer::SemanticAnalyzer,
    symbol_table::SymbolTable,
};

pub struct State {
    pub file: EnderpyFile,
    pub symbol_table: SymbolTable,
}

impl State {
    pub fn process_top_levels(&mut self) -> SymbolTable {
        let mut sem_anal = SemanticAnalyzer::new();
        for stmt in &self.file.defs {
            sem_anal.visit_stmt(stmt)
        }
        sem_anal.globals
    }
}
