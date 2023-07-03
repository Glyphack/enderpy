use std::{collections::HashMap, path::PathBuf};

use parser::Parser;

use crate::nodes::EnderpyFile;
use crate::semantic_analyzer::SemanticAnalyzer;
use crate::settings::Settings;
use crate::state::State;

pub struct BuildSource {
    pub path: PathBuf,
    pub module: Option<String>,
    pub source: String,
    // If this source was found by following an import
    pub followed: bool,
}

pub struct BuildManager {
    errors: Vec<String>,
    sources: Vec<BuildSource>,
    modules: HashMap<String, State>,
    missing_modules: Vec<String>,
    semantic_analyzer: SemanticAnalyzer,
    options: Settings,
}

impl BuildManager {
    pub fn new(
        sources: Vec<BuildSource>,
        semantic_analyzer: SemanticAnalyzer,
        options: Settings,
    ) -> Self {
        if sources.len() > 1 {
            panic!("analyzing more than 1 input is not supported");
        }

        BuildManager {
            errors: vec![],
            modules: HashMap::new(),
            sources,
            missing_modules: vec![],
            semantic_analyzer,
            options,
        }
    }

    // Entry point to analyze the program
    pub fn build(self) {
        let build_source = self.sources.last().unwrap();
        let tree = self.parse_file(&build_source.source, &build_source.path);

        println!("{:?}", tree.imports);
        let mut modules = HashMap::new();
        modules.insert(
            self.get_mod_name(build_source),
            State {
                manager: self,
                tree,
            },
        );
    }

    pub fn get_mod_name(&self, source: &BuildSource) -> String {
        if let Some(module) = &source.module {
            module.clone()
        } else {
            // TODO: fix how module name is determined
            source
                .path
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()
        }
    }

    pub fn parse_file(&self, source: &String, path: &PathBuf) -> EnderpyFile {
        let mut parser = Parser::new(source.clone());
        let tree = parser.parse();
        EnderpyFile::new(path.clone())
    }

    // Adds a source file to the build manager
    fn add_source_file(&mut self, file_path: String) {}

    // Finds imports in the source files and creates an import graph
    fn find_imports(&mut self) {
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
