use std::{collections::HashMap, path::PathBuf};

use enderpy_python_parser::error::ParsingError;
use env_logger::Builder;
use log::info;

use crate::{
    build_source::BuildSource,
    diagnostic::Diagnostic,
    nodes::{EnderpyFile, ImportKinds},
    ruff_python_import_resolver as ruff_python_resolver,
    ruff_python_import_resolver::{
        config::Config, execution_environment, import_result::ImportResult,
        module_descriptor::ImportModuleDescriptor, resolver,
    },
    settings::Settings,
    state::State,
    type_check::checker::TypeChecker,
};

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
            builder.filter(None, log::LevelFilter::Debug).init();
        } else {
            builder.filter(None, log::LevelFilter::Warn);
        }

        log::debug!("Initialized build manager");
        log::debug!("build sources: {:?}", sources);
        log::debug!("options: {:?}", options);

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

    // Entry point to analyze the program
    pub fn build(&mut self) {
        self.populate_modules();
    }

    // Resolves imports in all files and then populates the symbol table
    fn populate_modules(&mut self) {
        for build_source in self.build_sources.iter() {
            let build_source: BuildSource = build_source.clone();
            let state = State::new(build_source.into());
            self.modules.insert(state.file.module_name(), state);
        }
        let (new_files, imports) = match self.options.follow_imports {
            crate::settings::FollowImports::All => {
                self.gather_files(self.build_sources.clone(), true)
            }
            crate::settings::FollowImports::Skip => {
                self.gather_files(self.build_sources.clone(), false)
            }
        };
        for build_source in new_files {
            let module = self.create_module(build_source);
            self.modules.insert(module.file.module_name(), module);
        }
        for module in self.modules.values_mut() {
            info!("file: {:#?}", module.file.module_name());
            module.populate_symbol_table(&imports);
        }
    }

    // Performs type checking passes over the code
    // This step happens after the binding phase
    pub fn type_check(&mut self) {
        self.build();
        // TODO: This is a hack to get all the symbol tables so we can resolve imports
        let mut all_symbol_tables = Vec::new();
        for module in self.modules.values() {
            all_symbol_tables.push(module.get_symbol_table());
        }

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
            let mut checker = TypeChecker::new(state.1, &self.options, all_symbol_tables.clone());
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

    // Given a list of files, this function will resolve the imports in the files
    // and add them to the modules.
    fn gather_files(
        &self,
        initial_files: Vec<BuildSource>,
        add_indirect_imports: bool,
    ) -> (
        Vec<BuildSource>,
        HashMap<ImportModuleDescriptor, ImportResult>,
    ) {
        let execution_environment = &execution_environment::ExecutionEnvironment {
            root: self.options.root.clone(),
            python_version: ruff_python_resolver::python_version::PythonVersion::Py312,
            python_platform: ruff_python_resolver::python_platform::PythonPlatform::Darwin,
            // Adding a blank path to the extra paths is a hack to make the resolver work
            extra_paths: vec![PathBuf::from("")],
        };
        let import_config = &Config {
            typeshed_path: self.options.import_discovery.typeshed_path.clone(),
            stub_path: None,
            venv_path: Some(self.options.root.clone()),
            venv: None,
        };

        let mut files_to_resolve: Vec<BuildSource> = vec![];
        files_to_resolve.extend(initial_files);
        let mut import_results = HashMap::new();
        let mut imported_sources = Vec::new();

        log::debug!("import options: {:?}", execution_environment);
        let mut new_imports: Vec<BuildSource> = vec![];
        let host = &ruff_python_resolver::host::StaticHost::new(vec![]);
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
                        match BuildSource::from_path(resolved_path.clone(), true) {
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

                    // TODO: not sure if this part is needed. Need to check when
                    // we have more tests on builder. This
                    // is supposed to add implicit imports to the build sources
                    // implicit import example: import foo.bar
                    // foo/bar/__init__.py
                    // In this case, we need to add foo/bar/__init__.py to the
                    // build sources for (name,
                    // implicit_import) in resolved.implicit_imports.iter() {
                    //     if self
                    //         .modules
                    //         .contains_key(&get_module_name(&implicit_import.
                    // path))     {
                    //         log::debug!(
                    //             "implicit import already exists: {}",
                    //             get_module_name(&implicit_import.path)
                    //         );
                    //         continue;
                    //     }
                    //     let source =
                    // std::fs::read_to_string(implicit_import.path.clone()).
                    // unwrap();     let build_source =
                    //         BuildSource::from_path(implicit_import.path.
                    // clone(), true).unwrap();
                    // self.build_sources.push(build_source);
                    // self.add_to_modules(&build_source);
                    // match self.modules.get(&build_source.module) {
                    //     Some(discovered_module) => {
                    //         if
                    // !self.modules.contains_key(&build_source.module) {
                    //             new_imports.push(discovered_module);
                    //         }
                    //     }
                    //     None => {
                    //         panic!("cannot find module: {}",
                    // build_source.module);     }
                    // }
                    // }
                }
            }
        }

        // if !add_indirect_imports {
        //     return;
        // }
        //
        // // Follow the import files and add them to the modules as well
        // while !new_imports.is_empty() {
        //     let mut next_imports = vec![];
        //     for state in new_imports {
        //         let resolved_imports = state.resolve_file_imports(
        //             execution_environment,
        //             import_config,
        //             host,
        //             &cached_imports,
        //         );
        //         // check if the resolved_imports are not in the current files and add
        // them to         // the new imports
        //         for (_, resolved_import) in resolved_imports {
        //             if !resolved_import.is_import_found {
        //                 continue;
        //             }
        //             for resolved_path in resolved_import.resolved_paths {
        //                 if
        // self.modules.contains_key(&get_module_name(&resolved_path)) {
        //                     log::debug!("imported file already in modules: {:?}",
        // resolved_path);                     continue;
        //                 }
        //                 let build_source = match
        // BuildSource::from_path(resolved_path, true) {
        // Ok(build_source) => build_source,                     Err(e) => {
        //                         log::warn!("cannot read file: {}", e);
        //                         continue;
        //                     }
        //                 };
        //                 match self.modules.get(&build_source.module) {
        //                     Some(state) => {
        //                         if !self.modules.contains_key(&build_source.module) {
        //                             next_imports.push(state);
        //                         }
        //                     }
        //                     None => {
        //                         panic!("cannot find module: {}",
        // build_source.module);                     }
        //                 }
        //             }
        //         }
        //     }
        //     new_imports = next_imports;
        // }
        (imported_sources, import_results)
    }

    // TODO: refactor to implement From/to trait
    fn create_module(&self, build_source: BuildSource) -> State {
        State::new(EnderpyFile::from(build_source))
    }

    fn resolve_file_imports(
        &self,
        file: EnderpyFile,
        execution_environment: &ruff_python_resolver::execution_environment::ExecutionEnvironment,
        import_config: &ruff_python_resolver::config::Config,
        host: &ruff_python_resolver::host::StaticHost,
        cached_imports: &HashMap<ImportModuleDescriptor, ImportResult>,
    ) -> HashMap<ImportModuleDescriptor, ImportResult> {
        log::debug!("resolving imports for file: {}", file.module_name());
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
            log::debug!("import descriptions: {:?}", import_descriptions);

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
                log::debug!(
                    "resolved import: {} -> {:?}",
                    import_desc.name(),
                    resolved.resolved_paths
                );
                imports.insert(import_desc, resolved.clone());
            }
        }

        imports.clone()
    }
}

// TODO: refactor type check tests to be like symbol table tests
#[cfg(test)]
mod tests {
    use std::fs;

    use insta::glob;

    use super::*;

    #[allow(dead_code)]
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

    macro_rules! snap_type {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let result = snapshot_type_check(contents);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.set_description(contents);
                settings.add_filter(
                    r"module_name: .*.typechecker.test_data.inputs.symbol_table..*.py",
                    "module_name: [REDACTED]",
                );
                settings.bind(|| {
                    insta::assert_snapshot!(result);
                });
            }
        };
    }

    snap_type!(test_type_check_var, "../testdata/inputs/type_check_var.py");
    snap_type!(
        test_type_check_call,
        "../testdata/inputs/type_check_call.py"
    );
    // snap_type!(
    //     test_type_check_list,
    //     "../testdata/inputs/type_check_list.py"
    // );

    snap_type!(
        test_type_check_undefined,
        "../testdata/inputs/type_check_undefined.py"
    );

    snap_type!(
        test_undefined_names,
        "../testdata/inputs/test_undefined_name.py"
    );

    #[test]
    fn test_symbol_table() {
        glob!("../test_data/inputs/", "symbol_table/*.py", |path| {
            let mut manager = BuildManager::new(
                vec![BuildSource::from_path(path.to_path_buf(), false).unwrap()],
                Settings::test_settings(),
            );
            manager.build();

            let module = manager.get_state(path.to_path_buf()).unwrap();

            let result = format!("{}", module.get_symbol_table());
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("../testdata/output/");
            settings.set_description(fs::read_to_string(path).unwrap());
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
        })
    }
}
