use std::fmt::Display;

use enderpy_python_parser::ast;

use crate::symbol_table;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum PythonType {
    None,
    /// Unknown and Any type are similar but we are using Uknown when we cannot possibly know the type of a value.
    Unknown,
    /// representing that we know nothing about the value a node can contain.
    /// For example, if a file contains only the function def f(x): return x, the name x will have an Anyas its value within the function
    /// because there is no information to determine what value it can contain
    Any,
    /// representing a value with concrete type.
    /// For example, if we define some variable foo to have type Literal[3], we are declaring that foo must be exactly equal to 3 and no other value.
    /// In type inference the values are not assumed to be literals unless they are explicitly declared as such.
    KnownValue(KnownValue),
    /// Union type
    MultiValue(Vec<PythonType>),
    Callable(Box<CallableType>),
    Bool,
    Int,
    Float,
    Str,
    Class(ClassType),
    Never,
}

#[allow(unused)]
pub enum TypeFlags {
    /// This type refers to an instance of a class.
    Instance,

    /// This type refers to a class.
    Instantiable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Any {
    pub source: AnySource,
}

/// Describes the source of Any
#[allow(unused)]
#[derive(PartialEq, Clone, Debug)]
pub enum AnySource {
    /// The user wrote 'Any' in an annotation.
    Explicit,
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
    /// The class details from the symbol table
    pub details: symbol_table::Class,
    // to represent types like `List[Int]`
    pub type_parameters: Vec<PythonType>,
}

impl ClassType {
    pub fn new(details: symbol_table::Class, type_parameters: Vec<PythonType>) -> Self {
        Self {
            details,
            type_parameters,
        }
    }
}

impl PartialEq for ClassType {
    fn eq(&self, other: &Self) -> bool {
        self.details.name == other.details.name && self.type_parameters == other.type_parameters
    }
}

/// https://peps.python.org/pep-0586/
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
    Bytes(Vec<u8>),
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let value_str = match self {
            LiteralValue::Bool(b) => b.to_string(),
            LiteralValue::Int(i) => i.to_string(),
            LiteralValue::Float(f) => f.to_string(),
            LiteralValue::Str(s) => s.to_string(),
            LiteralValue::None => "None".to_string(),
            LiteralValue::Bytes(b) => {
                let bytes_str = b.iter().map(|b| format!("{:02x}", b)).collect::<String>();
                return write!(f, "b'{}'", bytes_str);
            }
        };

        write!(f, "{}", value_str)
    }
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
                    .type_parameters
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                let fmt = format!("{}[{}]", class_type.details.name, args_str);
                return write!(f, "{}", fmt);
            }
            PythonType::Never => "Never",
            PythonType::KnownValue(value) => {
                let value = format!("{}", value.literal_value);
                return write!(f, "Literal[{}]", value);
            }
            PythonType::MultiValue(m) => {
                let values = m
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                return write!(f, "Union[{}]", values);
            }
        };

        write!(f, "{}", type_str)
    }
}
