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

    #[error("Invalid token")]
    InvalidToken {
        msg: Box<str>,
    },
}


#[derive(Error, Debug)]
pub enum LexError {
    #[error("String not terminated")]
    StringNotTerminated,
    #[error("Invalid char {0} in binary literal. Must be 0 or 1")]
    InvalidDigitInBinaryLiteral(char),
    #[error("Invalid char {0} in octal literal. Must be between 0 and 7")]
    InvalidDigitInOctalLiteral(char),
    #[error("Invalid char {0} in hexadecimal literal. Must be between 0 and 9, or between A and F")]
    InvalidDigitInHexadecimalLiteral(char),
    #[error("Invalid digit in decimal literal")]
    InvalidDigitInDecimalLiteral,
    #[error("Unindent does not match any outer indentation level")]
    UnindentDoesNotMatchAnyOuterIndentationLevel,
}
