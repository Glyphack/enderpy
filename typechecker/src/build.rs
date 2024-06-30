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
pub struct BuildManager<'a> {
    pub errors: DashSet<Diagnostic>,
    pub modules: DashMap<PathBuf, EnderpyFile<'a>>,
    pub diagnostics: DashMap<PathBuf, Vec<Diagnostic>>,
    pub settings: Settings,
}
#[allow(unused)]
impl<'a> BuildManager<'a> {
    pub fn new(settings: Settings) -> Self {
        let mut builder = Builder::new();
        builder
            .filter(None, log::LevelFilter::Info)
            .format_timestamp(None)
            .try_init();

        let mut modules = DashMap::new();

        BuildManager {
            errors: DashSet::new(),
            modules,
            settings,
            diagnostics: DashMap::new(),
        }
    }

    pub fn get_state(&self, path: &Path) -> EnderpyFile<'a> {
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
        let builtins_file = self.settings.typeshed_path.join("stdlib/builtins.pyi");
        let builtins = EnderpyFile::new(&builtins_file, true);
        let initial_files = vec![builtins];
        let (imports, mut new_modules) =
            gather_files(initial_files, root, self.settings.typeshed_path.clone());
        log::debug!("Imports resolved");
        for mut module in new_modules.iter_mut() {
            module.populate_symbol_table(imports.clone());
        }
        for module in new_modules {
            self.modules.insert(module.path(), module);
        }
        log::debug!("Prebuild finished");
    }

    // Resolves imports and adds file and its imports to modules
    pub fn build_one(&self, root: &Path, file: &Path) {
        debug!("building {file:?}");
        let enderpy_file = EnderpyFile::new(file, false);
        let (imports, mut new_modules) = gather_files(
            vec![enderpy_file],
            root,
            self.settings.typeshed_path.clone(),
        );
        log::debug!("Imports resolved");
        for mut module in new_modules.iter_mut() {
            module.populate_symbol_table(imports.clone());
        }
        for module in new_modules {
            self.modules.insert(module.path(), module);
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
}

fn gather_files<'a>(
    mut initial_files: Vec<EnderpyFile<'a>>,
    root: &Path,
    typeshed_path: PathBuf,
) -> (
    HashMap<ImportModuleDescriptor, ImportResult>,
    Vec<EnderpyFile<'a>>,
) {
    let execution_environment = &execution_environment::ExecutionEnvironment {
        root: root.to_path_buf(),
        python_version: ruff_python_resolver::python_version::PythonVersion::Py312,
        python_platform: ruff_python_resolver::python_platform::PythonPlatform::Darwin,
        extra_paths: vec![],
    };
    let import_config = &Config {
        typeshed_path: Some(typeshed_path.clone()),
        stub_path: None,
        venv_path: None,
        venv: None,
    };
    let host = &ruff_python_resolver::host::StaticHost::new(vec![]);
    let mut new_modules = Vec::with_capacity(initial_files.len() * 5);
    let mut import_results = HashMap::new();

    let cache: &mut HashMap<PathBuf, HashMap<ImportModuleDescriptor, ImportResult>> =
        &mut HashMap::new();

    while let Some(module) = initial_files.pop() {
        if cache.get(&module.path).is_some() {
            continue;
        }
        let resolved_imports = resolve_file_imports(
            &module,
            execution_environment,
            import_config,
            host,
            &import_results,
        );
        new_modules.push(module);
        for (import_desc, resolved) in resolved_imports {
            if resolved.is_import_found {
                for resolved_path in resolved.resolved_paths.iter() {
                    if !initial_files.iter().any(|m| m.path == *resolved_path) {
                        let e = EnderpyFile::new(resolved_path, true);
                        initial_files.push(e);
                    }
                }

                for (_, implicit_import) in resolved.implicit_imports.iter() {
                    let e = EnderpyFile::new(&implicit_import.path, true);
                    initial_files.push(e);
                }
            }
            import_results.insert(import_desc, resolved);
        }
    }

    new_modules.extend(initial_files);
    (import_results, new_modules)
}

fn resolve_file_imports(
    file: &EnderpyFile<'_>,
    execution_environment: &ruff_python_resolver::execution_environment::ExecutionEnvironment,
    import_config: &ruff_python_resolver::config::Config,
    host: &ruff_python_resolver::host::StaticHost,
    cached_imports: &HashMap<ImportModuleDescriptor, ImportResult>,
) -> HashMap<ImportModuleDescriptor, ImportResult> {
    let mut imports = HashMap::new();
    debug!("resolving imports for file {:?}", file.path());
    for import in file.get_imports().iter() {
        let import_descriptions = match import {
            ImportKinds::Import(i) => i
                .names
                .iter()
                .map(ruff_python_resolver::module_descriptor::ImportModuleDescriptor::from)
                .collect::<Vec<ImportModuleDescriptor>>(),
            ImportKinds::ImportFrom(i) => {
                vec![ruff_python_resolver::module_descriptor::ImportModuleDescriptor::from(*i)]
            }
        };

        for import_desc in import_descriptions {
            let resolved = match cached_imports.contains_key(&import_desc) {
                true => continue,
                false => resolver::resolve_import(
                    &file.path(),
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
            imports.insert(import_desc, resolved);
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
