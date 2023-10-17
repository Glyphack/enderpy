use std::path::PathBuf;

#[derive(Debug)]
/// BuildSource is a struct that holds metadata about a source file
/// that is being analyzed.
/// It only contains information about the source file itself, not any
/// semantic information about the file. Like the AST, for example.
/// This struct is only used in the builder to keep track of the sources
/// And to add more sources to the builder.
pub struct BuildSource {
    pub path: PathBuf,
    pub source: String,
    // if this source is found by following an import
    pub followed: bool,
}
