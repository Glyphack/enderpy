use is_macro::Is;
use std::fmt::Display;
use std::path::PathBuf;

use enderpy_python_parser::ast;

use crate::symbol_table::{self, Id};

#[derive(Debug, Clone, PartialEq, Eq, Is)]
pub enum PythonType {
    None,
    /// Unknown and Any type are similar but we are using Unknown when we cannot
    /// possibly know the type of a value.
    Unknown,
    /// representing that we know nothing about the value a node can contain.
    /// For example, if a file contains only the function def f(x): return x,
    /// the name x will have an Any as its value within the function
    /// because there is no information to determine what value it can contain
    Any,
    /// representing a value with concrete type.
    /// For example, if we define some variable foo to have type Literal[3], we
    /// are declaring that foo must be exactly equal to 3 and no other value.
    /// In type inference the values are not assumed to be literals unless they
    /// are explicitly declared as such.
    KnownValue(KnownValue),
    Module(ModuleRef),
    /// Union type
    MultiValue(Vec<PythonType>),
    Callable(Box<CallableType>),
    Class(ClassType),
    Optional(Box<PythonType>),
    Never,
    TypeVar(TypeVar),
}

impl PythonType {
    pub fn type_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (PythonType::None, PythonType::None) => true,
            (PythonType::Unknown, PythonType::Unknown) => true,
            (PythonType::Any, PythonType::Any) => true,
            (PythonType::Never, PythonType::Never) => true,
            (PythonType::KnownValue(v1), PythonType::KnownValue(v2)) => v1 == v2,
            (PythonType::MultiValue(m1), PythonType::MultiValue(m2)) => {
                if m1.len() != m2.len() {
                    return false;
                }

                for (t1, t2) in m1.iter().zip(m2.iter()) {
                    if !t1.type_equal(t2) {
                        return false;
                    }
                }

                true
            }
            (PythonType::Callable(c1), PythonType::Callable(c2)) => c1.type_equal(c2),
            (PythonType::Class(c1), PythonType::Class(c2)) => c1.type_equal(c2),
            _ => false,
        }
    }
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

impl Eq for CallableType {}

impl CallableType {
    pub fn type_equal(&self, other: &Self) -> bool {
        // TODO: add check for args too. We need to check what should be the rule for
        self.return_type.type_equal(&other.return_type)
    }
}

impl PartialEq for CallableType {
    fn eq(&self, other: &Self) -> bool {
        // TODO: add check for args too. We need to check what should be the rule for
        // two args to be equal
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
    // Whether this class is an instance or the class itself.
    // This is determined based on:
    // 1. If the type is inferred from annotation of a parameter or variable that is an instance
    // 2. If the type is inferred from a class node then it's an instance
    pub is_instance: bool,
}

impl ClassType {
    pub fn new(
        details: symbol_table::Class,
        type_parameters: Vec<PythonType>,
        is_instance: bool,
    ) -> Self {
        Self {
            details,
            type_parameters,
            is_instance,
        }
    }

    pub fn type_equal(&self, other: &Self) -> bool {
        self.details.name == other.details.name
            && self.type_parameters.len() == other.type_parameters.len()
            && self
                .type_parameters
                .iter()
                .zip(other.type_parameters.iter())
                .all(|(t1, t2)| t1.type_equal(t2))
    }
}

impl PartialEq for ClassType {
    fn eq(&self, other: &Self) -> bool {
        self.details.name == other.details.name && self.type_parameters == other.type_parameters
    }
}

impl Eq for ClassType {}

#[derive(Debug, Eq, Clone)]
pub struct TypeVar {
    pub name: String,
    pub bounds: Vec<PythonType>,
}

impl PartialEq for TypeVar {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.bounds == other.bounds
    }
}

/// https://peps.python.org/pep-0586/
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct KnownValue {
    pub literal_value: LiteralValue,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
                for byte in b {
                    write!(f, "{:02x}", byte)?;
                }
                return Ok(());
            }
        };

        write!(f, "{}", value_str)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ModuleRef {
    pub module_id: Id,
}

impl Display for PythonType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let type_str = match self {
            PythonType::None => "None",
            PythonType::Any => "Any",
            PythonType::Module(_) => "Module",
            PythonType::Unknown => "Unknown",
            PythonType::Callable(callable_type) => {
                let fmt = format!("(function) {}", callable_type.name.as_str());
                return write!(f, "{}", fmt);
            }
            PythonType::Class(class_type) => {
                // show it like class[args]
                let args_str = class_type
                    .type_parameters
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                let fmt = if args_str.is_empty() {
                    format!("(class) {}", class_type.details.name.clone())
                } else {
                    format!("(class) {}[{}]", class_type.details.qual_name, args_str)
                };
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
            PythonType::TypeVar(type_var) => {
                let bounds = type_var
                    .bounds
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                return write!(f, "TypeVar[{}, {}]", type_var.name, bounds);
            }
            PythonType::Optional(optional) => return write!(f, "Optional[{optional:}]"),
        };

        write!(f, "{}", type_str)
    }
}
