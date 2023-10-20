use crate::{
    token::{Kind, Token},
    Lexer,
};

// Helper function to lex a file until the end
pub fn lex(lexer: &mut Lexer) -> Vec<Token> {
    let mut tokens = vec![];
    loop {
        let token = lexer.next_token();
        if token.kind == Kind::Eof {
            break;
        }
        tokens.push(token);
    }
    tokens
}
