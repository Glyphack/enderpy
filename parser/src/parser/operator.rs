use crate::parser::ast::UnaryOperator;
use crate::token::Kind;

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

pub fn is_bin_arithmetic_op(kind: &Kind) -> bool {
    match kind {
        Kind::Plus
        | Kind::Minus
        | Kind::Mul
        | Kind::MatrixMul
        | Kind::Div
        | Kind::Mod
        | Kind::Pow
        | Kind::IntDiv => true,
        _ => false,
    }
}

pub fn is_comparison_operator(kind: &Kind) -> bool {
    match kind {
        Kind::Eq
        | Kind::NotEq
        | Kind::Less
        | Kind::LessEq
        | Kind::Greater
        | Kind::GreaterEq
        | Kind::Is
        | Kind::In
        // Not is not a comparison operator, but it is used in the
        // "not in" operator
        | Kind::Not => true,
        _ => false,
    }
}
