use enderpy_python_parser::error::ParsingError;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum BuildError {
    #[error(transparent)]
    #[diagnostic(code(builder::io_error))]
    IoError(#[from] std::io::Error),

    #[error("Invalid syntax")]
    #[diagnostic(code(builder::invalid_syntax))]
    TypeError {
        path: String,
        msg: String,
        line: u32,
        #[help]
        advice: String,
        #[label("span")]
        span: SourceSpan,
    },

    #[error(transparent)]
    // Use `#[diagnostic(transparent)]` to wrap another [`Diagnostic`]. You won't see labels otherwise
    #[diagnostic(transparent)]
    ParsingError(#[from] ParsingError),
}
