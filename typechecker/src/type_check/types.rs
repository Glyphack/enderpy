use std::fmt::Display;

use parser::ast;

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Type {
    None,
    Unknown,
    Callable(Box<CallableType>),
    Bool,
    Int,
    Float,
    Str,
    Class(ClassType),
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct CallableType {
    name: String,
    arguments: ast::Arguments,
    return_type: Type,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct ClassType {
    pub name: String,
    // to represent types like `List[Int]`
    pub args: Vec<Type>,
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
            Type::Class(class_type) => class_type.name.as_str(),
        };

        write!(f, "{}", type_str)
    }
}
