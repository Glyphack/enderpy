use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};
use tracing::{span, Level};
use tracing_subscriber::EnvFilter;

use dashmap::DashMap;
use env_logger::Builder;
use log::debug;

use crate::{
    checker::TypeChecker,
    diagnostic::Diagnostic,
    file::{EnderpyFile, ImportKinds},
    ruff_python_import_resolver::{
        self as ruff_python_resolver, config::Config, execution_environment,
        import_result::ImportResult, module_descriptor::ImportModuleDescriptor, resolver,
    },
    settings::Settings,
    symbol_table::{Id, SymbolTable},
};

#[derive(Debug)]
pub struct BuildManager {
    pub modules: DashMap<Id, EnderpyFile>,
    pub diagnostics: DashMap<PathBuf, Vec<Diagnostic>>,
    pub symbol_tables: DashMap<Id, SymbolTable>,
    pub module_ids: DashMap<PathBuf, Id>,
    pub settings: Settings,
    import_config: Config,
    host: ruff_python_resolver::host::StaticHost,
}
#[allow(unused)]
impl<'a> BuildManager {
    pub fn new(settings: Settings) -> Self {
        let mut builder = Builder::new();

        let log_level = match std::env::var("DEBUG") {
            Ok(_) => log::LevelFilter::Debug,
            _ => log::LevelFilter::Info,
        };
        tracing_subscriber::fmt()
            .with_env_filter(EnvFilter::from_default_env())
            .with_ansi(false)
            .pretty()
            .try_init();

        builder
            .filter(None, log_level)
            .format_timestamp(None)
            .try_init();

        let mut modules = DashMap::new();
        let import_config = Config {
            typeshed_path: Some(settings.typeshed_path.clone()),
            stub_path: None,
            venv_path: None,
            venv: None,
        };
        let host = ruff_python_resolver::host::StaticHost::new(vec![]);

        BuildManager {
            modules,
            settings,
            diagnostics: DashMap::new(),
            symbol_tables: DashMap::new(),
            module_ids: DashMap::new(),
            import_config,
            host,
        }
    }

    pub fn get_state(&self, path: &Path) -> EnderpyFile {
        let id = self.module_ids.get(path).expect("path not found");
        let result = self.modules.get(&id).unwrap();
        return result.value().clone();
    }

    // Entry point to analyze the program
    // this only prepares necessary python files.
    pub fn build(&self, root: &Path) {
        let builtins_file = self.settings.typeshed_path.join("stdlib/builtins.pyi");
        let builtins = EnderpyFile::new(builtins_file, true);
        let (imports, mut new_modules) =
            gather_files(vec![builtins], root, &self.import_config, &self.host);
        log::debug!("Imports resolved");
        for mut module in new_modules {
            let sym_table = module.populate_symbol_table(&imports);
            self.symbol_tables.insert(module.id, sym_table);
            self.module_ids.insert(module.path.to_path_buf(), module.id);
            self.modules.insert(module.id, module);
        }
        log::debug!("Prebuild finished");
    }

    // Resolves imports and adds file and its imports to modules
    pub fn build_one(&self, root: &Path, file: &Path) {
        debug!("building {file:?}");
        let enderpy_file = EnderpyFile::new(file.to_path_buf(), false);
        let (imports, mut new_modules) =
            gather_files(vec![enderpy_file], root, &self.import_config, &self.host);
        log::debug!("Imports resolved");
        for mut module in new_modules {
            let sym_table = module.populate_symbol_table(&imports);
            self.symbol_tables.insert(module.id, sym_table);
            self.module_ids.insert(module.path.to_path_buf(), module.id);
            self.modules.insert(module.id, module);
        }
        log::debug!("Symbol tables populated");
    }

    // Performs type checking passes over the code
    // This step happens after the binding phase
    pub fn type_check(&self, path: &Path) -> TypeChecker {
        let mut module_to_check = self.get_state(path);

        let span = span!(Level::TRACE, "type check", path = %path.display());
        let _guard = span.enter();
        let mut checker = TypeChecker::new(
            self.get_symbol_table(path),
            &self.symbol_tables,
            &self.module_ids,
            module_to_check.clone(),
        );
        for stmt in module_to_check.tree.body.iter() {
            checker.type_check(stmt);
        }
        checker
    }

    pub fn get_symbol_table(&self, path: &Path) -> SymbolTable {
        let module_id = self.module_ids.get(path).expect("incorrect ID");
        let symbol_table = self.symbol_tables.get(module_id.value());

        return symbol_table
            .expect("symbol table not found")
            .value()
            .clone();
    }

    pub fn get_hover_information(&self, path: &Path, line: u32, column: u32) -> String {
        let module = self.get_state(path);
        let checker = self.type_check(path);
        let symbol_table = self.get_symbol_table(path);
        let hovered_offset = module.line_starts[line as usize] + column;

        let hovered_offset_start = hovered_offset.saturating_sub(1);
        let type_info = &checker
            .types
            .find(hovered_offset_start, hovered_offset + 1)
            .last();
        let type_str = if let Some(type_info) = type_info {
            &type_info.val
        } else {
            return String::new();
        };

        format!("{type_str:}")
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedImport {
    pub resolved_ids: Vec<Id>,
    _result: ImportResult,
}

pub type ResolvedImports = HashMap<ImportModuleDescriptor, Arc<ResolvedImport>>;

fn gather_files<'a>(
    mut initial_files: Vec<EnderpyFile>,
    root: &Path,
    import_config: &ruff_python_resolver::config::Config,
    host: &ruff_python_resolver::host::StaticHost,
) -> (ResolvedImports, HashSet<EnderpyFile>) {
    let execution_environment = &execution_environment::ExecutionEnvironment {
        root: root.to_path_buf(),
        python_version: ruff_python_resolver::python_version::PythonVersion::Py312,
        python_platform: ruff_python_resolver::python_platform::PythonPlatform::Darwin,
        extra_paths: vec![],
    };
    let mut new_modules = HashSet::with_capacity(initial_files.len() * 5);
    let mut import_results = HashMap::new();
    let mut seen = HashSet::new();

    while let Some(module) = initial_files.pop() {
        if seen.contains(&module.path) {
            continue;
        }
        seen.insert(module.path.clone());
        let resolved_imports = resolve_file_imports(
            &module,
            execution_environment,
            import_config,
            host,
            // &import_results,
        );
        new_modules.insert(module);
        for (import_desc, mut resolved) in resolved_imports {
            if !resolved.is_import_found {
                continue;
            }
            let mut resolved_ids = Vec::with_capacity(resolved.resolved_paths.len());
            for resolved_path in resolved.resolved_paths.iter_mut() {
                if let Some(found) = new_modules.iter().find(|m| *m.path == *resolved_path) {
                    resolved_ids.push(found.id);
                } else if let Some(found) = initial_files.iter().find(|m| *m.path == *resolved_path)
                {
                    resolved_ids.push(found.id);
                } else {
                    let e = EnderpyFile::new(std::mem::take(resolved_path), true);
                    resolved_ids.push(e.id);
                    initial_files.push(e);
                }
            }

            // TODO: don't know if the implicit imports should be in the resolved list or not
            // For imports like from os import path it points to the path.py file which is in the
            // implicit imports so without this we cannot resolved that.
            for (_, implicit_import) in resolved.implicit_imports.iter_mut() {
                let resolved_path = &mut implicit_import.path;
                if let Some(found) = new_modules.iter().find(|m| *m.path == *resolved_path) {
                    resolved_ids.push(found.id);
                } else if let Some(found) = initial_files.iter().find(|m| *m.path == *resolved_path)
                {
                    resolved_ids.push(found.id);
                } else {
                    let e = EnderpyFile::new(std::mem::take(resolved_path), true);
                    resolved_ids.push(e.id);
                    initial_files.push(e);
                }
            }
            import_results.insert(
                import_desc,
                Arc::new(ResolvedImport {
                    resolved_ids,
                    _result: resolved,
                }),
            );
        }
    }

    new_modules.extend(initial_files);

    for import in import_results.iter() {
        for resolved in import.1.resolved_ids.iter() {
            if !new_modules.iter().any(|m| m.id == *resolved) {
                for module in new_modules.iter() {
                    println!("{:?} - {:?}", module.path, module.id);
                }
                panic!("symbol table not found {resolved:?}");
            }
        }
    }
    (import_results, new_modules)
}

fn resolve_file_imports(
    file: &EnderpyFile,
    execution_environment: &ruff_python_resolver::execution_environment::ExecutionEnvironment,
    import_config: &ruff_python_resolver::config::Config,
    host: &ruff_python_resolver::host::StaticHost,
) -> HashMap<ImportModuleDescriptor, ImportResult> {
    let mut imports = HashMap::new();
    debug!("resolving imports for file {:?}", file.path);
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
            // TODO: Cache non relative imports
            let resolved = match false {
                true => continue,
                false => resolver::resolve_import(
                    &file.path,
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

                let symbol_table = manager.get_symbol_table(&path);

                let result = format!("{}", symbol_table);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../test_data/output/");
                settings.set_description(content);
                settings.add_filter(r"/.*/typechecker", "[TYPECHECKER]");
                settings.add_filter(r"/.*/typeshed", "[TYPESHED]");
                settings.add_filter(
                    r"module_name: .*.typechecker.test_data.inputs.symbol_table..*.py",
                    "module_name: [REDACTED]",
                );
                settings.add_filter(r"Id\(\d+\)", "Id(REDACTED)");
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
