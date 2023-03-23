use miette::{self, Diagnostic};
use thiserror::{self, Error};

#[derive(Debug, Error, Diagnostic)]
#[error("Syntax error")]
#[diagnostic()]
pub struct SyntaxError(pub &'static str);
