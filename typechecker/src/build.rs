use core::panic;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    usize,
};

use dashmap::{DashMap, DashSet};
use enderpy_python_parser::error::ParsingError;
use env_logger::Builder;
use log::{debug, info};

use crate::{
    diagnostic::Diagnostic,
    nodes::{EnderpyFile, ImportKinds},
    ruff_python_import_resolver::{
        self as ruff_python_resolver, config::Config, execution_environment,
        import_result::ImportResult, module_descriptor::ImportModuleDescriptor, resolver,
    },
    settings::Settings,
    types::PythonType,
};

#[derive(Debug)]
pub struct BuildManager {
    pub errors: DashSet<Diagnostic>,
    pub modules: DashMap<PathBuf, EnderpyFile>,
    pub diagnostics: DashMap<PathBuf, Vec<Diagnostic>>,
    pub settings: Settings,
}
#[allow(unused)]
impl BuildManager {
    pub fn new(sources: Vec<PathBuf>, settings: Settings) -> Self {
        if sources.len() > 1 {
            panic!("analyzing more than 1 input is not supported");
        }

        let mut builder = Builder::new();
        builder
            .filter(None, log::LevelFilter::Info)
            .format_timestamp(None)
            .try_init();

        let builtins_file = settings.typeshed_path.join("stdlib/builtins.pyi");
        let builtins = EnderpyFile::new(&builtins_file, true);
        let mut modules = DashMap::new();
        modules.insert(builtins.path(), builtins);
        for src in sources.iter() {
            let module = EnderpyFile::new(src, false);
            modules.insert(module.path(), module);
        }

        BuildManager {
            errors: DashSet::new(),
            modules,
            settings,
            diagnostics: DashMap::new(),
        }
    }

    pub fn get_state(&self, path: &Path) -> EnderpyFile {
        let result = self.modules.get(path);
        match result {
            None => {
                let available: Vec<PathBuf> = self
                    .modules
                    .clone()
                    .into_iter()
                    .map(|f| f.0.clone())
                    .collect();
                panic!("Not found, available {available:?}")
            }
            Some(x) => x.value().clone(),
        }
    }

    // Entry point to analyze the program
    pub fn build(&self, root: &Path) {
        let imports = self.gather_files(
            self.modules.iter().map(|f| f.value().clone()).collect(),
            root,
        );
        for imported_module in imports.iter() {
            for path in imported_module.1.resolved_paths.iter() {
                let module = EnderpyFile::new(path, true);
                self.modules.insert(module.path(), module);
            }
        }
        log::debug!("Imports resolved");
        for mut module in self.modules.iter_mut() {
            module.populate_symbol_table(imports.clone());
        }
        log::info!("Symbol tables populated");
    }

    // Resolves imports and adds file and its imports to modules
    pub fn build_one(&self, root: &Path, file: &Path) {
        let enderpy_file = EnderpyFile::new(file, false);
        self.modules
            .insert(enderpy_file.path(), enderpy_file.clone());
        let imports = self.gather_files(vec![enderpy_file], root);
        for imported_module in imports.iter() {
            for path in imported_module.1.resolved_paths.iter() {
                let module = EnderpyFile::new(path, true);
                self.modules.insert(module.path(), module);
            }
        }
        for mut module in self.modules.iter_mut() {
            if module.symbol_table.is_none() {
                module.populate_symbol_table(imports.clone());
            }
        }
        log::info!("Symbol tables populated");
    }

    // Performs type checking passes over the code
    // This step happens after the binding phase
    pub fn type_check(&self) {
        self.diagnostics.clear();
        // TODO: This is a hack to get all the symbol tables so we can resolve imports
        let mut all_symbol_tables = Vec::new();
        for module in self.modules.iter() {
            all_symbol_tables.push(module.value().get_symbol_table());
        }

        let mut checker_count = 0;

        for mut state in self.modules.iter_mut() {
            // do not type check builtins
            if state.value().path().ends_with("builtins.pyi") {
                continue;
            }
            if state.value().followed {
                continue;
            }
            let path = &state.value().path;
            checker_count += 1;
            if !state.value().parser.errors.is_empty() {
                for err in state.value().parser.errors.iter() {
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
                                    start: state.value().get_position(span.0 as u32),
                                    end: state.value().get_position(span.1 as u32),
                                },
                            });
                            self.diagnostics
                                .entry(state.value().path())
                                .or_default()
                                .push(Diagnostic {
                                    body: msg.to_string(),
                                    suggestion: Some(advice.to_string()),
                                    range: crate::diagnostic::Range {
                                        start: state.value().get_position(span.0 as u32),
                                        end: state.value().get_position(span.1 as u32),
                                    },
                                });
                        }
                    }
                }
            }
            state.type_check(all_symbol_tables.clone());
            let checker = state.get_checker();
            for error in checker.errors {
                self.errors.insert(Diagnostic {
                    body: error.msg.to_string(),
                    suggestion: Some("".into()),
                    range: crate::diagnostic::Range {
                        start: state.value().get_position(error.span.0 as u32),
                        end: state.value().get_position(error.span.1 as u32),
                    },
                });
                self.diagnostics
                    .entry(state.value().path())
                    .or_default()
                    .push(Diagnostic {
                        body: error.msg.to_string(),
                        suggestion: Some("".into()),
                        range: crate::diagnostic::Range {
                            start: state.value().get_position(error.span.0 as u32),
                            end: state.value().get_position(error.span.1 as u32),
                        },
                    });
            }
        }
        info!("checked {checker_count:} modules");
    }

    pub fn get_hover_information(&self, path: &Path, line: u32, column: u32) -> (String, String) {
        let module = self.get_state(path);
        let symbol_table = module.get_symbol_table();
        let hovered_offset = module.parser.line_starts()[line as usize] + column;
        let line_number = match module.parser.line_starts().binary_search(&hovered_offset) {
            Ok(index) => index,
            Err(index) => index - 1,
        };
        let symbol_name = match module
            .parser
            .identifiers_start_offset
            .binary_search_by_key(&hovered_offset, |x| x.0)
        {
            Ok(index) => Some(&module.parser.identifiers_start_offset[index].2),
            Err(index) => {
                if index < 1 {
                    None
                } else {
                    let found_identifier = &module.parser.identifiers_start_offset[index - 1].2;
                    let start = module.parser.identifiers_start_offset[index - 1].0;
                    let end = module.parser.identifiers_start_offset[index - 1].1;
                    info!(
                        "start: {}, end: {}, hovered offset: {} found_identifier: {}",
                        start, end, hovered_offset, found_identifier
                    );
                    if start <= hovered_offset && end > hovered_offset {
                        Some(found_identifier)
                    } else {
                        None
                    }
                }
            }
        };
        let hovered_offset_end = if let Some(name) = symbol_name {
            name.len() as u32 + hovered_offset
        } else {
            return ("".to_string(), "".to_string());
        };
        debug!("line: {line}, column: {column} offset: {hovered_offset} symbol: {symbol_name:?}");
        let symbol_info = match symbol_name {
            Some(symbol_name) => {
                let scope_id = symbol_table
                    .scope_starts
                    .find(hovered_offset, hovered_offset_end)
                    .last()
                    .expect("scope id not found")
                    .val;
                match symbol_table.lookup_in_scope(crate::symbol_table::LookupSymbolRequest {
                    name: symbol_name,
                    scope: Some(scope_id),
                }) {
                    Some(symbol) => symbol.to_string(),
                    None => "symbol not found".to_string(),
                }
            }
            None => "".to_string(),
        };

        let Some(checker) = module.type_checker else {
            return (symbol_info, String::from(""));
        };

        let type_info = &checker
            .types
            .find(hovered_offset, hovered_offset_end)
            .last();
        let type_str = if let Some(type_info) = type_info {
            &type_info.val
        } else {
            &PythonType::Unknown
        };

        (symbol_info, format!("{type_str:}"))
    }

    // Given a list of files, this function will resolve the imports in the files
    // and add them to the modules.
    fn gather_files(
        &self,
        initial_files: Vec<EnderpyFile>,
        root: &Path,
    ) -> HashMap<ImportModuleDescriptor, ImportResult> {
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

        let mut files_to_resolve: Vec<EnderpyFile> = vec![];
        files_to_resolve.extend(initial_files);
        let mut import_results = HashMap::new();
        while let Some(source) = files_to_resolve.pop() {
            let resolved_imports = self.resolve_file_imports(
                source.clone(),
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
                        let e = EnderpyFile::new(resolved_path, true);
                        files_to_resolve.push(e);
                    }
                }
            }
        }
        import_results
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
                let manager = BuildManager::new(vec![path], Settings::test_settings());
                manager.build(&Path::new(""));

                let module = manager.get_state(PathBuf::from($test_file).as_path());

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
