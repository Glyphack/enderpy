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

pub struct BuildManager<'a> {
    errors: Vec<String>,
    sources: Vec<BuildSource>,
    modules: HashMap<String, State<'a>>,
    missing_modules: Vec<String>,
    options: Settings,
}

impl<'a> BuildManager<'a> {
    pub fn new(sources: Vec<BuildSource>, options: Settings) -> Self {
        if sources.len() > 1 {
            panic!("analyzing more than 1 input is not supported");
        }

        BuildManager {
            errors: vec![],
            sources,
            modules: HashMap::new(),
            missing_modules: vec![],
            options,
        }
    }

    fn prepare_modules(&self) {
        let mut modules = HashMap::new();
        for build_source in &self.sources {
            let file = self.parse_file(&build_source.source, &build_source.path);

            modules.insert(
                self.get_module_name(build_source),
                State {
                    manager: self,
                    file,
                },
            );
        }
    }

    // Entry point to analyze the program
    pub fn build(&self) {
        self.prepare_modules();
        self.pre_analysis()
    }

    pub fn get_module_name(&self, source: &BuildSource) -> String {
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
        EnderpyFile::from(&tree)
    }

    // Adds a source file to the build manager
    fn add_source_file(&mut self, file_path: String) {}

    // Finds imports in the source files and creates an import graph
    fn find_imports(&mut self) {
        // Populates self.import_graph based on the imports found
    }

    // Performs pre-analysis on the source files
    // Fills up the symbol table for each module
    fn pre_analysis(&mut self) {
        for state in self.modules.values().into_iter() {
            let semanal = SemanticAnalyzer {
                state: state,
                errors: vec![],
                scope: crate::symbol_table::SymbolScope::Global,
            };
            semanal.analyze()
        }
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

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;

    // Write a temp file with source code and return the path for test. Cleanup after this goes out of scope
    fn write_temp_source(source: &str) -> PathBuf {
        let path = PathBuf::from("./tmp/test.py");
        let mut file = std::fs::File::create(&path).unwrap();
        file.write_all(source.as_bytes()).unwrap();
        path
    }

    #[test]
    fn test_build_manager() {
        let mut manager = BuildManager::new(vec![], Settings::test_settings());
        manager.build();
    }
}
