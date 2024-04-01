use enderpy_python_parser::ast::{Alias, ImportFrom};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportModuleDescriptor {
    pub leading_dots: usize,
    pub name_parts: Vec<String>,
    pub imported_symbols: Vec<String>,
}

impl ImportModuleDescriptor {
    pub(crate) fn name(&self) -> String {
        format!(
            "{}{}",
            ".".repeat(self.leading_dots),
            &self.name_parts.join(".")
        )
    }
}

// Converts an import alias (e.g. impor a) to a module descriptor
// Since each import can have multiple aliases like import a, b, c
// we need to convert each alias to a module descriptor
impl From<&Alias> for ImportModuleDescriptor {
    fn from(alias: &Alias) -> Self {
        ImportModuleDescriptor {
            leading_dots: 0,
            name_parts: alias
                .name
                .split('.')
                .map(|s| s.trim())
                .map(std::string::ToString::to_string)
                .collect(),
            imported_symbols: vec![],
        }
    }
}

// Converts an import from (e.g. from a import b) to a module descriptor
impl From<&ImportFrom> for ImportModuleDescriptor {
    fn from(import_from: &ImportFrom) -> Self {
        ImportModuleDescriptor {
            leading_dots: import_from.level,
            name_parts: import_from
                .module
                .split('.')
                .map(|s| s.trim())
                .map(std::string::ToString::to_string)
                .collect(),
            imported_symbols: import_from.names.iter().map(|x| x.name.clone()).collect(),
        }
    }
}
