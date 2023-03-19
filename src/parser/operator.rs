use crate::parser::ast::{BinaryOperator, BooleanOperator, UnaryOperator};
use crate::token::Kind;

pub fn is_bool_op(kind: &Kind) -> bool {
    match kind {
        Kind::And | Kind::Or => true,
        _ => false,
    }
}

pub fn map_boolean_operator(kind: &Kind) -> BooleanOperator {
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

pub fn is_bin_op(kind: &Kind) -> bool {
    match kind {
        Kind::Plus
        | Kind::Minus
        | Kind::Mul
        | Kind::MulAssign
        | Kind::Div
        | Kind::Mod
        | Kind::Pow
        | Kind::IntDiv
        | Kind::BitAnd
        | Kind::BitOr
        | Kind::BitXor
        | Kind::LeftShift
        | Kind::RightShift => true,
        _ => false,
    }
}

pub fn map_binary_operator(kind: &Kind) -> BinaryOperator {
    match kind {
        Kind::Plus => BinaryOperator::Add,
        Kind::Minus => BinaryOperator::Sub,
        Kind::Mul => BinaryOperator::Mult,
        Kind::MulAssign => BinaryOperator::MatMult,
        Kind::Div => BinaryOperator::Div,
        Kind::Mod => BinaryOperator::Mod,
        Kind::Pow => BinaryOperator::Pow,
        Kind::IntDiv => BinaryOperator::FloorDiv,
        Kind::BitAnd => BinaryOperator::BitAnd,
        Kind::BitOr => BinaryOperator::BitOr,
        Kind::BitXor => BinaryOperator::BitXor,
        Kind::LeftShift => BinaryOperator::LShift,
        Kind::RightShift => BinaryOperator::RShift,
        _ => panic!("Not a binary operator"),
    }
}
