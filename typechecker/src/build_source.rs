use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
/// BuildSource is a struct that holds metadata about a source file
/// that is being analyzed.
/// It only contains information about the source file itself, not any
/// semantic information about the file. Like the AST, for example.
/// This struct is only used in the builder to keep track of the sources
/// And to add more sources to the builder.
pub struct BuildSource {
    pub path: PathBuf,
    pub source: String,
    pub module: String,
    // if this source is found by following an import
    pub followed: bool,
}

impl BuildSource {
    pub fn from_path(path: PathBuf, followed: bool) -> Result<Self, std::io::Error> {
        let source = std::fs::read_to_string(&path)?;
        let module = get_module_name(&path);
        Ok(BuildSource {
            path,
            module,
            source,
            followed,
        })
    }
}

fn get_module_name(path: &Path) -> String {
    path.to_str().unwrap_or_default().replace(['/', '\\'], ".")
}
