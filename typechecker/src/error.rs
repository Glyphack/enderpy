use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum TypeCheckError {
    // /// An error occurred while analyzing the source code.
    // #[error(transparent)]
    // #[diagnostic(code(my_lib::io_error))]
    // AnalysisError(#[from] std::io::Error),
    #[error(transparent)]
    // Use `#[diagnostic(transparent)]` to wrap another [`Diagnostic`]. You won't see labels otherwise
    #[diagnostic(transparent)]
    AnalysisError(#[from] AnalysisError),
}

#[derive(Error, Diagnostic, Debug)]
#[error("")]
pub struct AnalysisError {
    #[label("here")]
    pub at: SourceSpan,
    pub message: String,
}
