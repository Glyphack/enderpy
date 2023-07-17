use parser::ast::Module;

use crate::{build::BuildManager, nodes::EnderpyFile};

pub struct State<'a> {
    pub manager: &'a BuildManager<'a>,
    pub file: EnderpyFile,
}

impl<'a> State<'a> {}
