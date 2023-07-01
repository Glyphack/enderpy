use parser::ast::Module;

use crate::{
    build::{BuildManager, BuildSource},
    symbol_table::SymbolTable,
};

pub struct State {
    pub manager: BuildManager,
    pub smybol_table: SymbolTable,
    pub build_source: BuildSource,
    // tree: Module,
}

impl State {
    /// parses the file and run the first pass to extract imports.
    pub fn parse_and_first_pass(&self) {
        // self.tree = self.manager.parse_file(&self.build_source.source);
    }

    pub fn analyze_imports(&self, tree: Module) {}
}
