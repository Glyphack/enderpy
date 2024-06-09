use core::panic;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    usize,
};

use dashmap::{DashMap, DashSet};
use env_logger::Builder;
use log::{debug, warn};

use crate::{
    diagnostic::Diagnostic,
    file::{EnderpyFile, ImportKinds},
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
    pub fn new(settings: Settings) -> Self {
        let mut builder = Builder::new();
        builder
            .filter(None, log::LevelFilter::Info)
            .format_timestamp(None)
            .try_init();

        let builtins_file = settings.typeshed_path.join("stdlib/builtins.pyi");
        let builtins = EnderpyFile::new(&builtins_file, true);
        let mut modules = DashMap::new();
        modules.insert(builtins.path(), builtins);

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
    // this only prepares necessary python files.
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
            for (_, implicit_import) in imported_module.1.implicit_imports.iter() {
                let module = EnderpyFile::new(&implicit_import.path, true);
                self.modules.insert(module.path(), module);
            }
        }
        log::debug!("Imports resolved");
        for mut module in self.modules.iter_mut() {
            module.populate_symbol_table(imports.clone());
        }
        log::debug!("Prebuild finished");
    }

    // Resolves imports and adds file and its imports to modules
    pub fn build_one(&self, root: &Path, file: &Path) {
        debug!("building {file:?}");
        let enderpy_file = EnderpyFile::new(file, false);
        self.modules
            .insert(enderpy_file.path(), enderpy_file.clone());
        let imports = self.gather_files(vec![enderpy_file], root);
        for imported_module in imports.iter() {
            for path in imported_module.1.resolved_paths.iter() {
                if !path.is_file() {
                    continue;
                }
                let module = EnderpyFile::new(path, true);
                self.modules.insert(module.path(), module);
            }
        }
        for mut module in self.modules.iter_mut() {
            if module.symbol_table.is_none() {
                module.populate_symbol_table(imports.clone());
            }
        }
        log::debug!("Symbol tables populated");
    }

    // Performs type checking passes over the code
    // This step happens after the binding phase
    pub fn type_check(&self, path: &Path) {
        let mut module_to_check = self.get_state(path);
        // TODO: This is a hack to get all the symbol tables so we can resolve imports
        let mut all_symbol_tables = Vec::new();
        for module in self.modules.iter() {
            all_symbol_tables.push(module.value().get_symbol_table());
        }
        module_to_check.type_check(all_symbol_tables);
        self.modules.insert(path.to_path_buf(), module_to_check);
    }

    pub fn get_hover_information(&self, path: &Path, line: u32, column: u32) -> String {
        let module = self.get_state(path);
        let symbol_table = module.get_symbol_table();
        let hovered_offset = module.offset_line_number[line as usize] + column;
        let line_number = match module.offset_line_number.binary_search(&hovered_offset) {
            Ok(index) => index,
            Err(index) => index - 1,
        };

        let Some(checker) = module.type_checker else {
            warn!("type checker not ran yet");
            return String::from("");
        };

        let hovered_offset_start = hovered_offset.saturating_sub(1);
        let type_info = &checker
            .types
            .find(hovered_offset_start, hovered_offset + 1)
            .last();
        let type_str = if let Some(type_info) = type_info {
            &type_info.val
        } else {
            &PythonType::Unknown
        };

        format!("{type_str:}")
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
            let resolved_imports = resolve_file_imports(
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

                    for (_, implicit_import) in resolved.implicit_imports.iter() {
                        let source = match std::fs::read_to_string(&implicit_import.path) {
                            Ok(source) => source,
                            Err(e) => {
                                panic!("cannot read implicit import");
                            }
                        };
                        let e = EnderpyFile::new(&implicit_import.path, true);
                        files_to_resolve.push(e);
                    }
                }
            }
        }
        import_results
    }
}

fn resolve_file_imports(
    file: EnderpyFile,
    execution_environment: &ruff_python_resolver::execution_environment::ExecutionEnvironment,
    import_config: &ruff_python_resolver::config::Config,
    host: &ruff_python_resolver::host::StaticHost,
    cached_imports: &HashMap<ImportModuleDescriptor, ImportResult>,
) -> HashMap<ImportModuleDescriptor, ImportResult> {
    let mut imports = HashMap::new();
    debug!("resolving imports for file {:?}", file.path());
    for import in file.imports.iter() {
        let import_descriptions = match import {
            ImportKinds::Import(i) => i
                .names
                .iter()
                .map(ruff_python_resolver::module_descriptor::ImportModuleDescriptor::from)
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
                let manager = BuildManager::new(Settings::test_settings());
                let root = &Path::new("");
                manager.build(root);
                manager.build_one(root, &path);

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
    symbol_table_test!(
        test_symbols_import_star,
        "test_data/inputs/import_star_test/a.py"
    );
}
