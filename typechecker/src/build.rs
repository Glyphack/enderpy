use std::{collections::HashMap, path::PathBuf};

use crate::semantic_analyzer::SemanticAnalyzer;
use crate::settings::Settings;
use crate::symbol_table::SymbolTable;

struct BuildSource {
    path: PathBuf,
    module: String,
    source: String,
}

///
struct BuildManager<'a> {
    errors: Vec<String>,
    modules: HashMap<String, SymbolTable<'a>>,
    missing_modules: Vec<String>,
    semantic_analyzer: SemanticAnalyzer,
    options: Settings,
}

impl<'a> BuildManager<'a> {
    fn new(source: BuildSource, semantic_analyzer: SemanticAnalyzer, options: Settings) -> Self {
        BuildManager {
            errors: vec![],
            modules: HashMap::new(),
            missing_modules: vec![],
            semantic_analyzer,
            options,
        }
    }

    // Adds a source file to the build manager
    fn add_source_file(&mut self, file_path: String) {}

    // Finds imports in the source files and creates an import graph
    fn find_imports(&mut self) {
        // Logic to parse source files and extract imports
        // Populates self.import_graph based on the imports found
    }

    // Performs pre-analysis on the source files
    fn pre_analysis(&mut self) {
        // Logic to perform any pre-analysis required before type checking
    }

    // Performs type checking passes over the code
    fn type_check(&mut self) {
        panic!("not implemented")
    }

    // Performs type checking for a specific file
    fn type_check_file(&mut self, file_path: &str) {
        // Logic to perform type checking for a file
        // Updates self.type_checked_files after successful type checking
    }
}
