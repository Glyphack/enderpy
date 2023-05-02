use crate::token::Kind;

pub fn is_at_compound_statement(kind: &Kind) -> bool {
    match kind {
        Kind::If | Kind::While | Kind::For | Kind::Try | Kind::With | Kind::Def | Kind::Class => {
            true
        }
        _ => false,
    }
}
