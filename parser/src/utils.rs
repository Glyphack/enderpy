use crate::{token::{Token, Kind}, error::ParsingError, Lexer};

// Helper function to lex a file until the end
pub fn lex(lexer: &mut Lexer) -> (Vec<Token>, Vec<ParsingError>) {
    let mut tokens = vec![];
    let mut errors = vec![];
    loop {
        match lexer.next_token() {
            Ok(token) => {
                if token.kind == Kind::Eof {
                    break;
                }
                tokens.push(token);
            }
            Err(err) => {
                errors.push(err);
            }
        }
    }
    (tokens, errors)
}
