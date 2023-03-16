use crate::parser::BooleanOperator;
use crate::token::Kind;

pub fn is_bool_op(kind: &Kind) -> bool {
    match kind {
        Kind::And | Kind::Or => true,
        _ => false,
    }
}

pub fn map_binary_operator(kind: &Kind) -> BooleanOperator {
    match kind {
        Kind::And => BooleanOperator::And,
        Kind::Or => BooleanOperator::Or,
        _ => panic!("Not a binary operator"),
    }
}
