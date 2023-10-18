use enderpy_python_parser::error::ParsingError;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Diagnostic {
    /// The identifier of the diagnostic, used to align the diagnostic with a rule.
    pub name: String,
    /// The message body to display to the user, to explain the diagnostic.
    pub body: String,
    /// The message to display to the user, to explain the suggested fix.
    pub suggestion: Option<String>,
    pub range: Range,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

impl From<ParsingError> for Diagnostic{
    fn from(error: ParsingError) -> Self {
        match error {
            ParsingError::InvalidSyntax {
                msg,
                line,
                input: _,
                advice,
                span,
            } => Diagnostic{
                name: "invalid-syntax".to_string(),
                body: msg.to_string(),
                suggestion: Some(advice),
                range: Range {
                    start: Position {
                        line,
                        character: span.0 as u32,
                    },
                    end: Position {
                        line,
                        character: span.1 as u32,
                    },
                },
            },
        }
    }
}

