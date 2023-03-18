use crate::parser::ast::Node;
use miette::{self, Diagnostic};
use thiserror::{self, Error};

#[derive(Debug, Error, Diagnostic)]
#[error("Expect token")]
#[diagnostic()]
pub struct ExpectToken(
    pub &'static str,
    pub &'static str,
    #[label("Expect `{0}` here, but found `{1}`")] pub Node,
);

#[derive(Debug, Error, Diagnostic)]
#[error("Unexpected token")]
#[diagnostic()]
pub struct UnexpectedToken(
    pub &'static str,
    #[label("Unexpected token `{0}`")] pub Node,
);

#[derive(Debug, Error, Diagnostic)]
#[error("Expect token")]
#[diagnostic()]
pub struct UnknownStatement(pub &'static str, #[label("Unknown statement {0}")] pub Node);
