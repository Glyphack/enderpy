use crate::parser::ast::{BooleanOperator, UnaryOperator};
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

pub fn is_unary_op(kind: &Kind) -> bool {
    match kind {
        Kind::Not | Kind::BitNot | Kind::Minus | Kind::Plus => true,
        _ => false,
    }
}

pub fn map_unary_operator(kind: &Kind) -> UnaryOperator {
    match kind {
        Kind::Not => UnaryOperator::Not,
        Kind::BitNot => UnaryOperator::Invert,
        Kind::Minus => UnaryOperator::USub,
        Kind::Plus => UnaryOperator::UAdd,
        _ => panic!("Not a unary operator"),
    }
}
