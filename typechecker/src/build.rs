use std::{collections::HashMap, path::PathBuf};
use log::info;

use enderpy_python_parser::Parser;

use crate::ruff_python_import_resolver::config::Config;
use crate::ruff_python_import_resolver::{execution_environment, resolver};
use crate::ruff_python_import_resolver as ruff_python_resolver;
use crate::nodes::EnderpyFile;
use crate::settings::Settings;
use crate::state::State;
use crate::symbol_table::SymbolTable;
use crate::type_check::checker::TypeChecker;


pub struct BuildSource {
    pub path: PathBuf,
    //  module name should come from path??
    pub module: String,
    pub source: String,
    // If this source was found by following an import
    pub followed: bool,
}

#[derive(Debug, Clone)]
pub struct BuildError {
    pub msg: String,
    pub line: u32,
    pub start: u32,
    pub end: u32,
}

#[derive(Debug)]
pub struct BuildManager {
    errors: Vec<BuildError>,
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
            let mod_name = Self::get_module_name(&build_source.path);
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

    pub fn add_source(&mut self, path: &PathBuf) {
        let source = std::fs::read_to_string(path).unwrap();
        let module = Self::get_module_name(path);
        let build_source = BuildSource {
            path: path.clone(),
            module: module.clone(),
            source,
            followed: false,
        };
        let file = Box::new(Self::parse_file(build_source));
        self.modules.insert(module, State::new(file));
    }

    pub fn get_errors(&self) -> Vec<BuildError> {
        self.errors.clone()
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

    pub fn get_module_name(path: &PathBuf) -> String {
        path.to_str()
            .unwrap_or_default()
            .replace(['/', '\\'], ".")
    }

    // Entry point to analyze the program
    pub fn build(&mut self) {
        let files = self.modules.values().collect();
        info!("files: {:#?}", files);
        let new_files = self.gather_files(files);
        for file in new_files {
            self.modules.insert(file.file.module_name.clone(), file);
        }

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
                self.errors.push(BuildError {
                    msg: error.msg,
                    line: line as u32,
                    start: error.start as u32,
                    end: error.end as u32,
                });
            }
        }
    }

    fn gather_files(&self, current_files: Vec<&State>) -> Vec<State> {
        let mut new_imports = vec![];
        let mut discovered_files = vec![];
        for state in current_files {
            let resolved_imports = self.resolve_imports(state);
            // check if the resolved_imports are not in the current files and add them to the new imports
            for (_, state) in resolved_imports {
                if !self.modules.contains_key(&state.file.module_name) {
                    new_imports.push(state);
                }
            }
        }

        discovered_files.extend(new_imports.clone());

        while !new_imports.is_empty() {
            let mut next_imports = vec![];
            for state in new_imports {
                let resolved_imports = self.resolve_imports(&state);
                // check if the resolved_imports are not in the current files and add them to the new imports
                for (_, state) in resolved_imports {
                    if !self.modules.contains_key(&state.file.module_name) {
                        // check no discovered file with the same name exists
                        if !discovered_files
                            .iter()
                            .any(|x| x.file.module_name == state.file.module_name)
                        {
                            next_imports.push(state);
                        }
                    }
                }
            }
            discovered_files.extend(next_imports.clone());
            new_imports = next_imports;
        }
        discovered_files
    }

    // Resolves imports in a file and return the resolved paths
    fn resolve_imports(&self, state: &State) -> HashMap<String, State> {
        let execution_environment = &execution_environment::ExecutionEnvironment {
            root: self.options.root.clone(),
            python_version: ruff_python_resolver::python_version::PythonVersion::Py311,
            python_platform: ruff_python_resolver::python_platform::PythonPlatform::Linux,
            // Adding a blank path to the extra paths is a hack to make the resolver work
            extra_paths: vec![PathBuf::from("")],
        };
        let import_config = &Config {
            typeshed_path: None,
            stub_path: None,
            venv_path: Some(self.options.root.clone()),
            venv: None,
        };
        let host = &ruff_python_resolver::host::StaticHost::new(vec![]);
        let mut resolved_paths = HashMap::new();
        for import in state.file.imports.iter() {
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
            let mut resolved = resolver::resolve_import(
                state.file.path.as_path(),
                execution_environment,
                &import_desc,
                import_config,
                host,
            );
            if !resolved.is_import_found {
                let error = format!("cannot import name '{:#?}'", import_desc.name_parts,);
                println!("import error: {:#?}", error);
            }
            if resolved.is_import_found {
                for resolved_path in resolved.resolved_paths.iter() {
                    let source = std::fs::read_to_string(resolved_path).unwrap();
                    let module = Self::get_module_name(resolved_path);
                    let build_source = BuildSource {
                        path: resolved_path.clone(),
                        module: module.clone(), 
                        source,
                        followed: true,
                    };
                    let file = Box::new(Self::parse_file(build_source));
                    resolved_paths.insert(module, State::new(file));
                }

                for  (name, implicit_import) in resolved.implicit_imports.iter() {
                    let source = std::fs::read_to_string(implicit_import.path.clone()).unwrap();
                    let module = Self::get_module_name(&implicit_import.path);
                    let build_source = BuildSource {
                        path: implicit_import.path.clone(),
                        module: module.clone(),
                        source,
                        followed: true,
                    };
                    let file = Box::new(Self::parse_file(build_source));
                    resolved_paths.insert(module, State::new(file));
                }
            }
        }

        resolved_paths
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

        let errors = manager.get_errors();
        errors
            .iter()
            .map(|x| format!("{}:{}:{}: {}", x.line, x.start, x.end, x.msg))
            .collect::<Vec<String>>()
            .join("\n")
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

    snap_type!(test_undefined_names, "../testdata/inputs/test_undefined_name.py");
}
