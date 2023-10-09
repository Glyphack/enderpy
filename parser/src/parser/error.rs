use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum ParsingError {
    #[error(transparent)]
    #[diagnostic(code(gen_color::io_error))]
    IoError(#[from] std::io::Error),

    #[error("Invalid syntax")]
    #[diagnostic(code(gen_color::colors_and_steps_mismatch))]
    InvalidSyntax {
        path: Box<str>,
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
