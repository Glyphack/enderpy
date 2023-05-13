use crate::token::Kind;

// This is a duplication of logic, not sure if it's worth it
pub fn is_at_compound_statement(kind: &Kind) -> bool {
    match kind {
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
    }
}
