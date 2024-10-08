use crate::symbol_table::{self, Id};
use is_macro::Is;
use std::fmt::{self, Display};

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
    Coroutine(Box<CoroutineType>),
    Class(ClassType),
    Instance(InstanceType),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoneType {}

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
    pub signature: Vec<CallableArgs>,
    pub return_type: PythonType,
    pub is_async: bool,
}

impl Display for CallableType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let signature_str = self
            .signature
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let fmt = format!(
            "(function) Callable ({}): {}",
            signature_str, self.return_type
        );
        return write!(f, "{}", fmt);
    }
}

#[derive(Debug, Clone)]
pub enum CallableArgs {
    PositionalOnly(PythonType),
    Positional(PythonType),
    Keyword(PythonType),
    Args(PythonType),
    KwArgs(PythonType),
    WithDefault(PythonType),
}

impl fmt::Display for CallableArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CallableArgs::PositionalOnly(ty) => write!(f, "pos_only: {}", ty),
            CallableArgs::Positional(ty) => write!(f, "pos: {}", ty),
            CallableArgs::Keyword(ty) => write!(f, "kw_only: {}", ty),
            CallableArgs::Args(ty) => write!(f, "*args: {}", ty),
            CallableArgs::KwArgs(ty) => write!(f, "**kwargs: {}", ty),
            CallableArgs::WithDefault(ty) => write!(f, "kw_default: {} = ...", ty),
        }
    }
}

impl CallableArgs {
    pub fn get_type(&self) -> &PythonType {
        match &self {
            CallableArgs::Args(python_type) => python_type,
            CallableArgs::PositionalOnly(python_type) => python_type,
            CallableArgs::Positional(python_type) => python_type,
            CallableArgs::Keyword(python_type) => python_type,
            CallableArgs::KwArgs(python_type) => python_type,
            CallableArgs::WithDefault(python_type) => python_type,
        }
    }
}

impl Eq for CallableType {}

impl CallableType {
    pub fn new(
        name: String,
        signature: Vec<CallableArgs>,
        return_type: PythonType,
        is_async: bool,
    ) -> Self {
        CallableType {
            name,
            signature,
            return_type,
            is_async,
        }
    }
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CoroutineType {
    pub return_type: PythonType,
    // TODO(coroutine_annotation): Any Any are send and yield type that are not implemented yet
    // https://github.com/python/typing/issues/251
    pub send_type: PythonType,
    pub yield_type: PythonType,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct ClassType {
    /// The class details from the symbol table
    pub details: symbol_table::Class,
    // to represent types like `List[Int]`
    // NOTE: This can only be type var
    pub type_parameters: Vec<PythonType>,
    // If the parameters are specialized this filed is filled.
    // This can happen when creating a class with another type var
    pub specialized: Vec<PythonType>,
    // What types are allowed as base classes?
    pub base_classes: Vec<ClassType>,
}

impl ClassType {
    pub fn new(
        details: symbol_table::Class,
        type_parameters: Vec<PythonType>,
        base_classes: Vec<ClassType>,
        specialized: Vec<PythonType>,
    ) -> Self {
        Self {
            details,
            type_parameters,
            base_classes,
            specialized,
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

impl Display for ClassType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args_str = self
            .type_parameters
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let specialized = self
            .specialized
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let fmt = if args_str.is_empty() && specialized.is_empty() {
            format!("(class) {}", self.details.name.clone())
        } else {
            format!(
                "(class) {}[{}][{}]",
                self.details.qual_name, args_str, specialized
            )
        };
        write!(f, "{}", fmt)
    }
}

impl Eq for ClassType {}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceType {
    pub class_type: ClassType,
    // to represent types like `List[Int]`
    // TODO: Need to set unspecified type parameters to any
    pub specialized_type_parameters: Vec<PythonType>,
}

impl InstanceType {
    pub fn new(class: ClassType, type_parameters: Vec<PythonType>) -> Self {
        Self {
            class_type: class,
            specialized_type_parameters: type_parameters,
        }
    }
}

impl Display for InstanceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args_str = self
            .specialized_type_parameters
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let fmt = if args_str.is_empty() {
            format!("(instance) {}", self.class_type.details.name.clone())
        } else {
            format!(
                "(instance) {}[{}]",
                self.class_type.details.qual_name, args_str
            )
        };
        return write!(f, "{}", fmt);
    }
}

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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let type_str = match self {
            PythonType::None => "None",
            PythonType::Any => "Any",
            PythonType::Module(_) => "Module",
            PythonType::Unknown => "Unknown",
            PythonType::Callable(callable_type) => {
                return write!(f, "{}", callable_type);
            }
            PythonType::Coroutine(callable_type) => {
                let fmt = format!(
                    "Coroutine[{}, {}, {}]",
                    callable_type.send_type, callable_type.yield_type, callable_type.return_type
                );
                return write!(f, "{}", fmt);
            }
            PythonType::Class(class_type) => {
                return write!(f, "{class_type}");
            }
            PythonType::Instance(class_type) => {
                return write!(f, "{class_type}");
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
