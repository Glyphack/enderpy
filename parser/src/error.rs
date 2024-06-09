use miette::Diagnostic;
use thiserror::Error;

use crate::Parser;

#[derive(Error, Diagnostic, Debug, Clone)]
pub enum ParsingError {
    #[error("Invalid syntax")]
    #[diagnostic(code(parser::invalid_syntax))]
    InvalidSyntax {
        msg: String,
        #[source_code]
        input: String,
        #[help]
        advice: String,
        #[label("span")]
        span: (usize, usize),
    },
}

impl From<Parser<'_>> for ParsingError {
    fn from(err: Parser) -> Self {
        let token = err.cur_token();
        ParsingError::InvalidSyntax {
            msg: token.value.to_string(),
            input: err.curr_line_string(),
            advice: String::default(),
            span: err.get_span_on_line(token.start, token.end),
        }
    }
}

impl From<&mut Parser<'_>> for ParsingError {
    fn from(err: &mut Parser) -> Self {
        let token = err.cur_token();
        ParsingError::InvalidSyntax {
            msg: token.value.to_string(),
            input: err.curr_line_string(),
            advice: String::default(),
            span: err.get_span_on_line(token.start, token.end),
        }
    }
}

#[derive(Error, Debug)]
pub enum LexError {
    #[error("String not terminated")]
    StringNotTerminated,
    #[error("Invalid char {0} in binary literal. Must be 0 or 1")]
    InvalidDigitInBinaryLiteral(char),
    #[error("Invalid char {0} in octal literal. Must be between 0 and 7")]
    InvalidDigitInOctalLiteral(char),
    #[error(
        "Invalid char {0} in hexadecimal literal. Must be between 0 and 9, or between A and F"
    )]
    InvalidDigitInHexadecimalLiteral(char),
    #[error("Invalid digit in decimal literal")]
    InvalidDigitInDecimalLiteral,
    #[error("Unindent does not match any outer indentation level")]
    UnindentDoesNotMatchAnyOuterIndentationLevel,
}
