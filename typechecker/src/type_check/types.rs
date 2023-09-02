use std::fmt::Display;

use parser::ast;

#[allow(unused)]
pub enum Type {
    None,
    Unknown,
    Callable(Box<CallableType>),
    Bool,
    Int,
    Float,
    Str,
}

#[allow(unused)]
pub struct CallableType {
    name: String,
    arguments: ast::Arguments,
    return_type: Type,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let type_str = match self {
            Type::None => "None",
            Type::Bool => "Bool",
            Type::Int => "Int",
            Type::Float => "Float",
            Type::Str => "Str",
            Type::Unknown => "Unknown",
            Type::Callable(callable_type) => callable_type.name.as_str(),
        };

        write!(f, "{}", type_str)
    }
}
