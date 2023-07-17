use crate::{
    ast_visitor::TraversalVisitor, build::BuildManager, nodes::EnderpyFile,
    semantic_analyzer::SemanticAnalyzer,
};

pub struct State<'a> {
    pub manager: &'a BuildManager<'a>,
    pub file: EnderpyFile<'a>,
}

impl<'a> State<'a> {
    pub fn process_top_levels(&'a mut self) {
        let mut sem_anal = SemanticAnalyzer::new(&mut self.file.names);
        for stmt in &self.file.defs {
            sem_anal.visit_stmt(stmt)
        }
    }
}
