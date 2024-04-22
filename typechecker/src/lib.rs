mod ast_visitor;
mod error;
mod nodes;
mod ruff_python_import_resolver;
mod symbol_table;

pub mod build;
pub mod build_source;
pub mod checker;
pub mod diagnostic;
pub mod project;
mod rules;
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
