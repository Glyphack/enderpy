use std::path::PathBuf;

use crate::{build::BuildManager, symbol_table::SymbolTable};

pub struct State<'a> {
    // pub manager: BuildManager<'a>,
    pub smybol_table: SymbolTable<'a>,
    pub path: PathBuf,
}
