use std::path::Path;

pub mod ast_visitor;
pub mod file;
mod ruff_python_import_resolver;
pub mod symbol_table;

pub mod build;
pub mod checker;
pub mod diagnostic;
pub mod semantic_analyzer;
pub mod settings;
pub mod type_evaluator;
pub mod types;

pub(crate) mod builtins {
    pub const LIST_TYPE: &str = "list";
    pub const TUPLE_TYPE: &str = "tuple";
    pub const DICT_TYPE: &str = "dict";
    pub const SET_TYPE: &str = "set";
    pub const ITER_TYPE: &str = "Iterator";
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
    // First we strip .pyi and / or __init__.pyi from the end
    let mut s = path.to_str().unwrap();
    s = match s.strip_suffix("/__init__.pyi") {
        Some(new) => new,
        None => s
    };
    s = match s.strip_suffix(".pyi") {
        Some(new) => new,
        None => s
    };
    // And then we replace the slashes with .
    s.replace(['/', '\\'], ".")
}
