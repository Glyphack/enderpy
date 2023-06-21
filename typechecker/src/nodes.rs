// This file holds nodes that represent the AST of a file.
// This holds the same information as the parser AST, but
// in a more convenient format for the type checker.
// NOTE: at this point we're not sure what nodes need to be
// here, so this has the minimum amount of nodes needed to
// get the type checker working. But can be expanded.

use std::path::PathBuf;

use parser::ast::{Import, ImportFrom, Module};

pub struct FileAst {
    path: PathBuf,
    ast: Module,
    from_imports: ImportFrom,
    imports: Import,
}
