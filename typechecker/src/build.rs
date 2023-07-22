use std::{collections::HashMap, path::PathBuf};

use parser::Parser;

use crate::nodes::EnderpyFile;
use crate::settings::Settings;
use crate::state::State;
use crate::symbol_table::SymbolTable;

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
    pub modules: HashMap<String, State>,
    missing_modules: Vec<String>,
    options: Settings,
}

impl BuildManager {
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

    fn prepare_modules(&mut self) {
        for build_source in &self.sources {
            let file = self.parse_file(&build_source.source);
            let symbol_table = SymbolTable::new(crate::symbol_table::SymbolTableType::Module, 0);

            self.modules.insert(
                self.get_module_name(build_source),
                State { file, symbol_table },
            );
        }
    }

    // Entry point to analyze the program
    pub fn build(&mut self) {
        self.prepare_modules();
        self.pre_analysis();
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

    pub fn parse_file(&self, source: &String) -> EnderpyFile {
        let mut parser = Parser::new(source.clone());
        let tree = parser.parse();
        EnderpyFile::from(tree)
    }

    // Performs pre-analysis on the source files
    // Fills up the symbol table for each module
    fn pre_analysis(&mut self) {
        for state in self.modules.iter_mut() {
            state.1.populate_symbol_table();
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
    use super::*;
    use insta::assert_debug_snapshot;

    // Write a temp file with source code and return the path for test. Cleanup after this goes out of scope
    fn write_temp_source(source: &str) -> PathBuf {
        let path = PathBuf::from("./tmp/test.py");
        // let mut file = std::fs::File::create(&path).unwrap();
        // file.write_all(source.as_bytes()).unwrap();
        path
    }

    #[test]
    fn create_symbol_table() {
        let source = "a = 'hello world'\nb = a + 1";
        let path = write_temp_source(source);
        let mut manager = BuildManager::new(
            vec![BuildSource {
                path,
                module: Some(String::from("test")),
                source: source.to_string(),
                followed: false,
            }],
            Settings::test_settings(),
        );
        manager.build();
        let module = manager.modules.values().last().unwrap();
        insta::with_settings!({
                description => "simple assignment", // the template source code
                omit_expression => true // do not include the default expression
            }, {
                assert_debug_snapshot!(module.symbol_table);
        });
    }
}
