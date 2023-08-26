use std::{collections::HashMap, path::PathBuf};

use parser::Parser;

use crate::nodes::EnderpyFile;
use crate::settings::Settings;
use crate::state::State;
use crate::symbol_table::SymbolTable;
use crate::type_check::checker::TypeChecker;

pub struct BuildSource {
    pub path: PathBuf,
    pub module: String,
    pub source: String,
    // If this source was found by following an import
    pub followed: bool,
}

pub struct BuildManager {
    errors: Vec<String>,
    pub modules: HashMap<String, State>,
    missing_modules: Vec<String>,
    options: Settings,
}

impl BuildManager {
    pub fn new(sources: Vec<BuildSource>, options: Settings) -> Self {
        if sources.len() > 1 {
            panic!("analyzing more than 1 input is not supported");
        }

        let mut modules = HashMap::new();

        for build_source in sources {
            let mod_name = Self::get_module_name(&build_source);
            let file = Box::new(Self::parse_file(&build_source.source, build_source.module));
            let symbol_table = SymbolTable::new(crate::symbol_table::SymbolTableType::Module, 0);

            modules.insert(mod_name, State { file, symbol_table });
        }

        BuildManager {
            errors: vec![],
            modules,
            missing_modules: vec![],
            options,
        }
    }

    pub fn parse_file(source: &String, module_name: String) -> EnderpyFile {
        let mut parser = Parser::new(source.clone());
        let tree = parser.parse();
        EnderpyFile::from(tree, module_name)
    }

    pub fn get_module_name(source: &BuildSource) -> String {
        source
            .path
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string()
    }

    // Entry point to analyze the program
    pub fn build(&mut self) {
        self.pre_analysis();
    }

    // Performs pre-analysis on the source files
    // Fills up the symbol table for each module
    fn pre_analysis(&mut self) {
        for state in self.modules.iter_mut() {
            state.1.populate_symbol_table();
        }
    }

    // TODO: separate this can build to be able to test pre analysis and type checking separately
    // Performs type checking passes over the code
    pub fn type_check(&mut self) {
        self.pre_analysis();
        for state in self.modules.iter_mut() {
            let mut checker = TypeChecker::new(state.1, &self.options);
            checker.type_check();
            self.errors.append(&mut checker.errors);
        }
    }
}

fn snapshot_symbol_table(source: &str) -> String {
    let mut manager = BuildManager::new(
        vec![BuildSource {
            path: PathBuf::from("test.py"),
            module: String::from("test"),
            source: source.to_string(),
            followed: false,
        }],
        Settings::test_settings(),
    );
    manager.build();

    let module = manager.modules.values().last().unwrap();

    format!("{}", module.symbol_table)
}

fn snapshot_type_check(source: &str) -> String {
    let mut manager = BuildManager::new(
        vec![BuildSource {
            path: PathBuf::from("test.py"),
            module: String::from("test"),
            source: source.to_string(),
            followed: false,
        }],
        Settings::test_settings(),
    );
    manager.type_check();

    format!("{}", manager.errors.join("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! snap {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let result = snapshot_symbol_table(contents);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.set_description(contents);
                settings.bind(|| {
                    insta::assert_snapshot!(result);
                });
            }
        };
    }

    macro_rules! snap_type {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let result = snapshot_type_check(contents);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.set_description(contents);
                settings.bind(|| {
                    insta::assert_snapshot!(result);
                });
            }
        };
    }

    snap!(
        test_simple_var_assignments,
        "../testdata/inputs/simple_var_assignment.py"
    );

    snap!(
        test_function_def,
        "../testdata/inputs/function_definition.py"
    );

    snap!(test_class_def, "../testdata/inputs/class_definition.py");

    snap_type!(test_type_check_var, "../testdata/inputs/type_check_var.py");
}
