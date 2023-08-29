use std::fmt::Display;

pub enum Type {
    Any,
    None,
    Bool,
    Int,
    Float,
    Str,
    Unknown,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let type_str = match self {
            Type::Any => "Any",
            Type::None => "None",
            Type::Bool => "Bool",
            Type::Int => "Int",
            Type::Float => "Float",
            Type::Str => "Str",
            Type::Unknown => "Unknown",
        };

        write!(f, "{}", type_str)
    }
}
