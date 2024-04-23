use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use dashmap::{DashMap, DashSet};
use enderpy_python_parser::error::ParsingError;
use env_logger::Builder;

use crate::{
    build_source::BuildSource,
    checker::TypeChecker,
    diagnostic::Diagnostic,
    nodes::{EnderpyFile, ImportKinds},
    ruff_python_import_resolver as ruff_python_resolver,
    ruff_python_import_resolver::{
        config::Config, execution_environment, import_result::ImportResult,
        module_descriptor::ImportModuleDescriptor, resolver,
    },
    settings::Settings,
};

#[derive(Debug)]
pub struct BuildManager {
    pub errors: DashSet<Diagnostic>,
    pub modules: DashMap<String, EnderpyFile>,
    build_sources: DashSet<BuildSource>,
    // options: Settings,
    // Map of file name to list of diagnostics
    pub diagnostics: DashMap<PathBuf, Vec<Diagnostic>>,
    pub settings: Settings,
}
#[allow(unused)]
impl BuildManager {
    pub fn new(sources: Vec<BuildSource>, settings: Settings) -> Self {
        if sources.len() > 1 {
            panic!("analyzing more than 1 input is not supported");
        }

        let mut modules = DashMap::new();
        let mut builder = Builder::new();
        builder
            .filter(None, log::LevelFilter::Debug)
            .format_timestamp(None)
            .try_init();

        let builtins_file = settings.typeshed_path.join("stdlib/builtins.pyi");
        let builtins = BuildSource::from_path(builtins_file, true)
            .expect("builtins does not exist in typeshed");
        let mut sources_with_builtins = DashSet::new();
        sources_with_builtins.insert(builtins.clone());
        sources_with_builtins.extend(sources.clone());

        BuildManager {
            errors: DashSet::new(),
            build_sources: sources_with_builtins,
            modules,
            settings,
            // options,
            diagnostics: DashMap::new(),
        }
    }

    // pub fn get_result(&self) -> Vec<EnderpyFile> {
    //     self.modules.values().cloned().collect()
    // }

    pub fn get_state(&self, path: &str) -> Option<EnderpyFile> {
        let result = self.modules.get(path);
        let module_keys = self
            .modules
            .iter()
            .map(|x| x.key().clone())
            .collect::<Vec<String>>();
        match result {
            None => panic!("module not found, available modules: {:?}", module_keys),
            Some(x) => Some(x.value().clone()),
        }
    }

    // Entry point to analyze the program
    pub fn build(&self, root: &Path) {
        for build_source in self.build_sources.iter() {
            let build_source: BuildSource = build_source.clone();
            let state: EnderpyFile = build_source.into();
            self.modules.insert(state.path_str(), state);
        }
        let (new_files, imports) = self.gather_files(self.build_sources.clone(), root);
        log::debug!("Imports resolved");
        for build_source in new_files {
            let file: EnderpyFile = build_source.into();
            self.modules.insert(file.path_str(), file);
        }
        for mut module in self.modules.iter_mut() {
            module.populate_symbol_table(imports.clone());
        }
        log::debug!("Symbol tables populated");
    }

    // Performs type checking passes over the code
    // This step happens after the binding phase
    pub fn type_check(&self) {
        // TODO: This is a hack to get all the symbol tables so we can resolve imports
        let mut all_symbol_tables = Vec::new();
        for module in self.modules.iter() {
            all_symbol_tables.push(module.value().get_symbol_table());
        }

        for state in self.modules.iter() {
            // do not type check builtins
            if state.value().path().ends_with("builtins.pyi") {
                continue;
            }
            if state.value().build_source.followed {
                continue;
            }
            if !state.value().errors.is_empty() {
                for err in state.value().errors.iter() {
                    match err {
                        ParsingError::InvalidSyntax {
                            msg,
                            input,
                            advice,
                            span,
                        } => {
                            self.errors.insert(Diagnostic {
                                body: msg.to_string(),
                                suggestion: Some(advice.to_string()),
                                range: crate::diagnostic::Range {
                                    start: state.value().get_position(span.0),
                                    end: state.value().get_position(span.1),
                                },
                            });
                            self.diagnostics
                                .entry(state.value().path())
                                .or_default()
                                .push(Diagnostic {
                                    body: msg.to_string(),
                                    suggestion: Some(advice.to_string()),
                                    range: crate::diagnostic::Range {
                                        start: state.value().get_position(span.0),
                                        end: state.value().get_position(span.1),
                                    },
                                });
                        }
                    }
                }
            }
            let mut checker = TypeChecker::new(state.value().clone(), all_symbol_tables.clone());
            for stmt in &state.value().body {
                checker.type_check(stmt);
            }
            for error in checker.errors {
                self.errors.insert(Diagnostic {
                    body: error.msg.to_string(),
                    suggestion: Some("".into()),
                    range: crate::diagnostic::Range {
                        start: state.value().get_position(error.span.0),
                        end: state.value().get_position(error.span.1),
                    },
                });
                self.diagnostics
                    .entry(state.value().path())
                    .or_default()
                    .push(Diagnostic {
                        body: error.msg.to_string(),
                        suggestion: Some("".into()),
                        range: crate::diagnostic::Range {
                            start: state.value().get_position(error.span.0),
                            end: state.value().get_position(error.span.1),
                        },
                    });
            }
        }
    }

    // Given a list of files, this function will resolve the imports in the files
    // and add them to the modules.
    fn gather_files(
        &self,
        initial_files: DashSet<BuildSource>,
        root: &Path,
    ) -> (
        Vec<BuildSource>,
        HashMap<ImportModuleDescriptor, ImportResult>,
    ) {
        let execution_environment = &execution_environment::ExecutionEnvironment {
            root: root.to_path_buf(),
            python_version: ruff_python_resolver::python_version::PythonVersion::Py312,
            python_platform: ruff_python_resolver::python_platform::PythonPlatform::Darwin,
            extra_paths: vec![],
        };
        let import_config = &Config {
            typeshed_path: Some(self.settings.typeshed_path.clone()),
            stub_path: None,
            venv_path: None,
            venv: None,
        };
        let host = &ruff_python_resolver::host::StaticHost::new(vec![]);

        let mut files_to_resolve: Vec<BuildSource> = vec![];
        files_to_resolve.extend(initial_files);
        let mut import_results = HashMap::new();
        let mut imported_sources = Vec::new();
        let mut new_imports: Vec<BuildSource> = vec![];
        while let Some(source) = files_to_resolve.pop() {
            let enderpy_file = EnderpyFile::from(source);
            let resolved_imports = self.resolve_file_imports(
                enderpy_file,
                execution_environment,
                import_config,
                host,
                &import_results,
            );
            // check if the resolved_imports are not in the current files and add them to
            // the new imports
            for (import_desc, resolved) in resolved_imports {
                import_results.insert(import_desc, resolved.clone());
                if resolved.is_import_found {
                    for resolved_path in resolved.resolved_paths.iter() {
                        let source = match std::fs::read_to_string(resolved_path) {
                            Ok(source) => source,
                            Err(e) => {
                                log::warn!("cannot read file: {}", e);
                                continue;
                            }
                        };
                        match BuildSource::from_path(resolved_path.to_path_buf(), true) {
                            Ok(build_source) => {
                                imported_sources.push(build_source.clone());
                                files_to_resolve.push(build_source);
                            }
                            Err(e) => {
                                log::warn!("cannot read file: {}", e);
                                continue;
                            }
                        };
                    }
                }
            }
        }
        (imported_sources, import_results)
    }

    fn resolve_file_imports(
        &self,
        file: EnderpyFile,
        execution_environment: &ruff_python_resolver::execution_environment::ExecutionEnvironment,
        import_config: &ruff_python_resolver::config::Config,
        host: &ruff_python_resolver::host::StaticHost,
        cached_imports: &HashMap<ImportModuleDescriptor, ImportResult>,
    ) -> HashMap<ImportModuleDescriptor, ImportResult> {
        let mut imports = HashMap::new();
        for import in file.imports.iter() {
            let import_descriptions = match import {
                ImportKinds::Import(i) => i
                    .names
                    .iter()
                    .map(|x| {
                        ruff_python_resolver::module_descriptor::ImportModuleDescriptor::from(x)
                    })
                    .collect::<Vec<ImportModuleDescriptor>>(),
                ImportKinds::ImportFrom(i) => {
                    vec![ruff_python_resolver::module_descriptor::ImportModuleDescriptor::from(i)]
                }
            };

            for import_desc in import_descriptions {
                let resolved = match cached_imports.contains_key(&import_desc) {
                    true => continue,
                    false => resolver::resolve_import(
                        file.path().as_path(),
                        execution_environment,
                        &import_desc,
                        import_config,
                        host,
                    ),
                };

                if !resolved.is_import_found {
                    let error = format!("cannot import name '{}'", import_desc.name());
                    log::warn!("{}", error);
                    continue;
                }
                imports.insert(import_desc, resolved.clone());
            }
        }
        imports
    }

    pub fn add_source(&self, source: BuildSource) {
        self.build_sources.insert(source);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    macro_rules! symbol_table_test {
        ($test_name:ident, $test_file:expr) => {
            #[test]
            fn $test_name() {
                let path = PathBuf::from($test_file);
                let content = fs::read_to_string(path.clone()).unwrap();
                let manager = BuildManager::new(
                    vec![BuildSource::from_path(path, false).unwrap()],
                    Settings::test_settings(),
                );
                manager.build(&Path::new(""));

                let module = manager
                    .get_state(PathBuf::from($test_file).as_path().to_str().unwrap())
                    .expect("module not found");

                let result = format!("{}", module.get_symbol_table());
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../test_data/output/");
                settings.set_description(content);
                settings.add_filter(r"/.*/typechecker", "[TYPECHECKER]");
                settings.add_filter(r"/.*/typeshed", "[TYPESHED]");
                settings.add_filter(
                    r"module_name: .*.typechecker.test_data.inputs.symbol_table..*.py",
                    "module_name: [REDACTED]",
                );
                settings.add_filter(r"\(id: .*\)", "(id: [REDACTED])");
                settings.bind(|| {
                    insta::assert_snapshot!(result);
                });
            }
        };
    }

    symbol_table_test!(
        test_symbols_class_definition,
        "test_data/inputs/symbol_table/class_definition.py"
    );

    symbol_table_test!(
        test_symbols_function_definition,
        "test_data/inputs/symbol_table/function_definition.py"
    );
    symbol_table_test!(
        test_symbols_imports,
        "test_data/inputs/symbol_table/imports.py"
    );
    symbol_table_test!(
        test_symbols_simple_var_assignment,
        "test_data/inputs/symbol_table/simple_var_assignment.py"
    );
    symbol_table_test!(
        test_symbols_type_alias,
        "test_data/inputs/symbol_table/type_alias.py"
    );
    symbol_table_test!(
        test_symbols_variables,
        "test_data/inputs/symbol_table/variables.py"
    );
}
