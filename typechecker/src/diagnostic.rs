use std::fmt::Display;

use enderpy_python_parser::error::ParsingError;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Diagnostic {
    /// The message body to display to the user, to explain the diagnostic.
    pub body: String,
    /// The message to display to the user, to explain the suggested fix.
    pub suggestion: Option<String>,
    pub range: Range,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(line: {}, character: {})", self.line, self.character)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CharacterSpan(pub usize, pub usize);

impl From<ParsingError> for Diagnostic {
    fn from(error: ParsingError) -> Self {
        match error {
            ParsingError::InvalidSyntax {
                msg,
                input: _,
                advice,
                span,
            } => Diagnostic {
                body: msg.to_string(),
                suggestion: Some(advice),
                range: Range {
                    start: Position {
                        line: 0,
                        character: span.0 as u32,
                    },
                    end: Position {
                        line: 0,
                        character: span.1 as u32,
                    },
                },
            },
        }
    }
}
