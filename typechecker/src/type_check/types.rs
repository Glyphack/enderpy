use std::fmt::Display;

use parser::ast;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
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

impl PartialEq for CallableType {
    fn eq(&self, other: &Self) -> bool {
        // TODO: add check for args too. We need to check what should be the rule for two args to
        // be equal
        self.return_type == other.return_type
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct ClassType {
    pub name: String,
    // to represent types like `List[Int]`
    pub args: Vec<Type>,
}

impl PartialEq for ClassType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args
    }
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
            Type::Class(class_type) => {
                // show it like class[args]
                let args_str = class_type
                    .args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                let fmt = format!("{}[{}]", class_type.name, args_str);
                return write!(f, "{}", fmt);
            }
        };

        write!(f, "{}", type_str)
    }
}
