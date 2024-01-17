pub mod checker;
mod rules;
pub mod type_evaluator;
mod types;

pub(crate) mod builtins {
    pub const LIST_TYPE: &str = "list";
    pub const TUPLE_TYPE: &str = "tuple";
    pub const DICT_TYPE: &str = "dict";
    pub const SET_TYPE: &str = "set";
    pub const ITER_TYPE: &str = "Iterator";
}
