use std::path::Path;

mod ast_visitor;
mod nodes;
mod ruff_python_import_resolver;
mod symbol_table;

pub mod build;
pub mod checker;
pub mod diagnostic;
pub mod semantic_analyzer;
pub mod settings;
pub mod type_evaluator;
mod types;

pub(crate) mod builtins {
    pub const LIST_TYPE: &str = "list";
    pub const TUPLE_TYPE: &str = "tuple";
    pub const DICT_TYPE: &str = "dict";
    pub const SET_TYPE: &str = "set";
    pub const ITER_TYPE: &str = "Iterator";

    pub const ALL_BUILTINS: [&str; 9] = [
        LIST_TYPE, TUPLE_TYPE, DICT_TYPE, SET_TYPE, ITER_TYPE, "str", "int", "float", "bool",
    ];
}

const PROJECT_ROOT_MARKERS: [&str; 1] = ["pyproject.toml"];

pub fn find_project_root(path: &Path) -> &Path {
    let root = path
        .ancestors()
        .find(|p| PROJECT_ROOT_MARKERS.iter().any(|m| p.join(m).exists()));
    match root {
        Some(root) => root,
        None => {
            if path.is_dir() {
                path
            } else {
                path.parent().unwrap_or(path)
            }
        }
    }
}

pub fn get_module_name(path: &Path) -> String {
    path.to_str().unwrap().replace(['/', '\\'], ".")
}
