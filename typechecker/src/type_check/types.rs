use std::fmt::Display;

use enderpy_python_parser::ast;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum PythonType {
    None,
    Unknown,
    /// representing that we know nothing about the value a node can contain.
    /// For example, if a file contains only the function def f(x): return x, the name x will have an Anyas its value within the function
    /// because there is no information to determine what value it can contain
    Any,
    /// representing a value with concrete type. 
    /// For example, if we define some variable foo to have type Literal[3], we are declaring that foo must be exactly equal to 3 and no other value.
    /// In type inference the values are not assumed to be literals unless they are explicitly declared as such.
    KnownValue(KnownValue),
    Callable(Box<CallableType>),
    Bool,
    Int,
    Float,
    Str,
    Class(ClassType),
    Never,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct CallableType {
    pub name: String,
    pub arguments: ast::Arguments,
    pub return_type: PythonType,
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
    pub args: Vec<PythonType>,
}

impl PartialEq for ClassType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KnownValue {
    pub literal_value: LiteralValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Bool(bool),
    Int(String),
    Float(String),
    Str(String),
    None,
}

impl Display for PythonType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let type_str = match self {
            PythonType::None => "None",
            PythonType::Any => "Any",
            PythonType::Bool => "Bool",
            PythonType::Int => "Int",
            PythonType::Float => "Float",
            PythonType::Str => "Str",
            PythonType::Unknown => "Unknown",
            PythonType::Callable(callable_type) => callable_type.name.as_str(),
            PythonType::Class(class_type) => {
                // show it like class[args]
                let args_str = class_type
                    .args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                let fmt = format!("{}[{}]", class_type.name, args_str);
                return write!(f, "{}", fmt);
            },
            PythonType::Never => "Never",
            PythonType::KnownValue(value) => {
                let value = match &value.literal_value {
                        LiteralValue::Bool(value) => value.to_string(),
                        LiteralValue::Int(value) => value.to_string(),
                        LiteralValue::Float(value) => value.to_string(),
                        LiteralValue::Str(value) => value.to_string(),
                        LiteralValue::None => "None".to_string(),
                };
                return write!(f, "Literal[{}]", value);
            },
        };

        write!(f, "{}", type_str)
    }
}
