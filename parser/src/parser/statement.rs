use crate::token::{Kind, Token};

// This is a duplication of logic, not sure if it's worth it
pub fn is_at_compound_statement(token: &Token) -> bool {
    let kind_is_statement = match token.kind {
        Kind::If
        | Kind::While
        | Kind::For
        | Kind::Try
        | Kind::With
        | Kind::Def
        | Kind::Class
        // Decorators
        | Kind::MatrixMul => true,
        _ => false,
    };
    if kind_is_statement {
        return true;
    }

    if Kind::Identifier == token.kind && token.value.to_string() == "match" {
        return true;
    }

    false
}
