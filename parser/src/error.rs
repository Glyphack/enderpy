use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum ParsingError {
    #[error("Invalid syntax")]
    #[diagnostic(code(parser::invalid_syntax))]
    InvalidSyntax {
        msg: Box<str>,
        line: u32,
        #[source_code]
        input: String,
        #[help]
        advice: String,
        #[label("span")]
        span: (usize, usize),
    },
}
