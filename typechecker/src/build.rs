use std::{collections::HashMap, path::PathBuf};

use parser::Parser;
use ruff_python_resolver::config::Config;
use ruff_python_resolver::execution_environment;

use crate::nodes::EnderpyFile;
use crate::settings::Settings;
use crate::state::State;
use crate::symbol_table::SymbolTable;
use crate::type_check::checker::TypeChecker;

use ruff_python_resolver::resolver::resolve_import;

pub struct BuildSource {
    pub path: PathBuf,
    //  module name should come from path??
    pub module: String,
    pub source: String,
    // If this source was found by following an import
    pub followed: bool,
}

pub struct BuildManager {
    errors: Vec<String>,
    pub modules: HashMap<String, State>,
    options: Settings,
}
#[allow(unused)]
impl BuildManager {
    pub fn new(sources: Vec<BuildSource>, options: Settings) -> Self {
        if sources.len() > 1 {
            panic!("analyzing more than 1 input is not supported");
        }

        let mut modules = HashMap::new();

        for build_source in sources {
            let mod_name = Self::get_module_name(&build_source);
            let file = Box::new(Self::parse_file(build_source));
            let symbol_table = SymbolTable::new(crate::symbol_table::SymbolTableType::Module, 0);

            modules.insert(mod_name, State::new(file));
        }

        BuildManager {
            errors: vec![],
            modules,
            options,
        }
    }

    pub fn parse_file(build_source: BuildSource) -> EnderpyFile {
        let mut parser = Parser::new(build_source.source.clone());
        let tree = parser.parse();
        EnderpyFile::from(
            tree,
            build_source.module,
            build_source.source.clone(),
            build_source.path,
        )
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
        self.resolve_imports();
        self.pre_analysis();
    }

    // Performs pre-analysis on the source files
    // Fills up the symbol table for each module
    fn pre_analysis(&mut self) {
        for state in self.modules.iter_mut() {
            state.1.populate_symbol_table();
        }
    }

    // Performs type checking passes over the code
    pub fn type_check(&mut self) {
        self.build();
        for state in self.modules.iter_mut() {
            let mut checker = TypeChecker::new(state.1, &self.options);
            for stmt in &state.1.file.body {
                checker.type_check(stmt);
            }
            for error in checker.errors {
                let line = get_line_number_of_character_position(&state.1.file.source, error.start);
                let error = format!("{} line {}: {}", state.0, line, error.msg);
                self.errors.push(error);
            }
        }
    }

    fn resolve_imports(&mut self) {
        let execution_environment = &execution_environment::ExecutionEnvironment {
            root: self.options.root.clone(),
            python_version: ruff_python_resolver::python_version::PythonVersion::Py310,
            python_platform: ruff_python_resolver::python_platform::PythonPlatform::Linux,
            extra_paths: vec![],
        };
        let import_config = &Config {
            typeshed_path: None,
            stub_path: None,
            venv_path: None,
            venv: None,
        };
        let host = &ruff_python_resolver::host::StaticHost::new(vec![]);
        let mut new_modules = HashMap::new();
        for state in self.modules.iter() {
            for import in state.1.file.imports.iter() {
                let import_desc = match import {
                    crate::nodes::ImportKinds::Import(i) => {
                        ruff_python_resolver::module_descriptor::ImportModuleDescriptor {
                            leading_dots: 0,
                            name_parts: i.names.iter().map(|x| x.name.clone()).collect(),
                            imported_symbols: vec![],
                        }
                    }
                    crate::nodes::ImportKinds::ImportFrom(i) => {
                        ruff_python_resolver::module_descriptor::ImportModuleDescriptor {
                            leading_dots: i.level,
                            name_parts: i
                                .module
                                .chars()
                                .skip_while(|c| *c == '.')
                                .collect::<String>()
                                .split('.')
                                .map(std::string::ToString::to_string)
                                .collect(),
                            imported_symbols: i.names.iter().map(|x| x.name.clone()).collect(),
                        }
                    }
                };
                let mut resolved = resolve_import(
                    state.1.file.path.as_path(),
                    execution_environment,
                    &import_desc,
                    import_config,
                    host,
                );
                if resolved.is_import_found {
                    for resolved_path in resolved.resolved_paths.iter() {
                        let source = std::fs::read_to_string(resolved_path).unwrap();
                        let build_source = BuildSource {
                            path: resolved_path.clone(),
                            module: String::from(""),
                            source,
                            followed: true,
                        };
                        let mod_name = Self::get_module_name(&build_source);
                        let file = Box::new(Self::parse_file(build_source));
                        new_modules.insert(mod_name, State::new(file));
                    }
                }
            }
        }
        self.modules.extend(new_modules);
    }
}

fn get_line_number_of_character_position(source: &str, pos: usize) -> usize {
    let mut line_number = 1;
    for (i, c) in source.chars().enumerate() {
        if i == pos {
            break;
        }
        if c == '\n' {
            line_number += 1;
        }
    }
    line_number
}

#[cfg(test)]
mod tests {
    use super::*;
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

        format!("{}", module.get_symbol_table())
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

        manager.errors.join("\n").to_string()
    }

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
    snap_type!(
        test_type_check_call,
        "../testdata/inputs/type_check_call.py"
    );
    snap_type!(
        test_type_check_list,
        "../testdata/inputs/type_check_list.py"
    );

    snap_type!(
        test_type_check_undefined,
        "../testdata/inputs/type_check_undefined.py"
    );
}
