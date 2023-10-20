use enderpy_python_parser::error::ParsingError;
use env_logger::Builder;
use log::info;
use std::{collections::HashMap, path::PathBuf};

use enderpy_python_parser::Parser;

use crate::build_source::BuildSource;
use crate::diagnostic::Diagnostic;
use crate::nodes::EnderpyFile;
use crate::ruff_python_import_resolver as ruff_python_resolver;
use crate::ruff_python_import_resolver::config::Config;
use crate::ruff_python_import_resolver::{execution_environment, resolver};
use crate::settings::Settings;
use crate::state::State;
use crate::type_check::checker::TypeChecker;

#[derive(Debug)]
pub struct BuildManager {
    pub errors: Vec<Diagnostic>,
    pub modules: HashMap<String, State>,
    build_sources: Vec<BuildSource>,
    options: Settings,
}
#[allow(unused)]
impl BuildManager {
    pub fn new(sources: Vec<BuildSource>, options: Settings) -> Self {
        if sources.len() > 1 {
            panic!("analyzing more than 1 input is not supported");
        }

        let mut modules = HashMap::new();

        let mut builder = Builder::new();
        if options.debug {
            builder.filter(None, log::LevelFilter::Debug);
        } else {
            builder.filter(None, log::LevelFilter::Warn);
        }

        BuildManager {
            errors: vec![],
            build_sources: sources,
            modules,
            options,
        }
    }

    pub fn get_result(&self) -> Vec<State> {
        self.modules.values().cloned().collect()
    }

    pub fn get_state(&self, path: PathBuf) -> Option<&State> {
        for state in self.modules.values() {
            info!("state: {:#?}", state.file.path());
            if state.file.path() == path {
                return Some(state);
            }
        }
        None
    }
    pub fn parse(&self, build_source: &BuildSource) -> EnderpyFile {
        let file_path = build_source.path.to_str().unwrap_or("");
        let mut parser = Parser::new(build_source.source.clone(), file_path.into());
        let tree = parser.parse();
        EnderpyFile::from(
            tree,
            Box::new(build_source.clone()),
            parser.errors,
        )
    }

    // Entry point to analyze the program
    pub fn build(&mut self) {
        self.populate_modules();
        self.pre_analysis();
    }

    // Performs pre-analysis on the source files
    // Fills up the symbol table for each module
    fn pre_analysis(&mut self) {
        for state in self.modules.iter_mut() {
            state.1.populate_symbol_table();
        }
    }

    fn populate_modules(&mut self) {
        for build_source in self.build_sources.iter() {
            let file = self.parse(build_source);
            let state = State::new(Box::new(file));
            self.modules.insert(build_source.module.clone(), state);
        }
        let initial_files = self.modules.values().collect();
        let new_files = self.gather_files(initial_files);
        for file in new_files {
            self.modules.insert(file.file.module_name().clone(), file);
        }
        for module in self.modules.values() {
            info!("file: {:#?}", module.file.module_name());
        }
    }

    // Performs type checking passes over the code
    pub fn type_check(&mut self) {
        self.build();
        for state in self.modules.iter_mut() {
            if !state.1.file.errors.is_empty() {
                for err in state.1.file.errors.iter() {
                    match err {
                        ParsingError::InvalidSyntax {
                            msg,
                            input,
                            advice,
                            span,
                        } => {
                            self.errors.push(Diagnostic {
                                body: msg.to_string(),
                                suggestion: Some(advice.to_string()),
                                range: crate::diagnostic::Range {
                                    start: state.1.file.get_position(span.0),
                                    end: state.1.file.get_position(span.1),
                                },
                            });
                            state.1.diagnostics.push(Diagnostic {
                                body: msg.to_string(),
                                suggestion: Some(advice.to_string()),
                                range: crate::diagnostic::Range {
                                    start: state.1.file.get_position(span.0),
                                    end: state.1.file.get_position(span.1),
                                },
                            });
                        }
                    }
                }
            }
            let mut checker = TypeChecker::new(state.1, &self.options);
            for stmt in &state.1.file.body {
                checker.type_check(stmt);
            }
            for error in checker.errors {
                self.errors.push(Diagnostic {
                    body: error.msg.to_string(),
                    suggestion: Some("".into()),
                    range: crate::diagnostic::Range {
                        start: state.1.file.get_position(error.span.0),
                        end: state.1.file.get_position(error.span.1),
                    },
                });
                state.1.diagnostics.push(Diagnostic {
                    body: error.msg.to_string(),
                    suggestion: Some("".into()),
                    range: crate::diagnostic::Range {
                        start: state.1.file.get_position(error.span.0),
                        end: state.1.file.get_position(error.span.1),
                    },
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
                if !self.modules.contains_key(&state.file.module_name()) {
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
                    if !self.modules.contains_key(&state.file.module_name()) {
                        // check no discovered file with the same name exists
                        if !discovered_files
                            .iter()
                            .any(|x| x.file.module_name() == state.file.module_name())
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
        log::debug!("import options: {:?}", execution_environment);

        let import_config = &Config {
            typeshed_path: None,
            stub_path: None,
            venv_path: Some(self.options.root.clone()),
            venv: None,
        };
        let host = &ruff_python_resolver::host::StaticHost::new(vec![]);
        let mut resolved_paths = HashMap::new();
        let mut resolved_imports = vec![];
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
                state.file.path().as_path(),
                execution_environment,
                &import_desc,
                import_config,
                host,
            );
            if !resolved.is_import_found {
                let error = format!("cannot import name '{}'", import_desc.name());
                log::warn!("{}", error);
            }
            if resolved.is_import_found {
                for resolved_path in resolved.resolved_paths.iter() {
                    let source = match std::fs::read_to_string(resolved_path) {
                        Ok(source) => source,
                        Err(e) => {
                            log::warn!("cannot read file: {}", e);
                            continue;
                        }
                    };
                    let build_source = BuildSource::from_path(resolved_path.clone(), true);
                    resolved_imports.push(build_source);
                }

                for (name, implicit_import) in resolved.implicit_imports.iter() {
                    let source = std::fs::read_to_string(implicit_import.path.clone()).unwrap();
                    let build_source = BuildSource::from_path(implicit_import.path.clone(), true);
                    resolved_imports.push(build_source);
                }
            }
        }

        for resolved_import in resolved_imports {
            let file = self.parse(&resolved_import);
            let state = State::new(Box::new(file));
            resolved_paths.insert(state.file.module_name().clone(), state);
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

        let errors = manager.errors;
        errors
            .iter()
            .map(|x| format!("{:?}", x))
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

    snap_type!(
        test_undefined_names,
        "../testdata/inputs/test_undefined_name.py"
    );
}
