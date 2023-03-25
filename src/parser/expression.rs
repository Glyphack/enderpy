use crate::token::Kind;

use super::ast::Expression;

pub fn is_atom(kind: &Kind) -> bool {
    match kind {
        Kind::Identifier
        | Kind::StringLiteral
        | Kind::RawString
        | Kind::RawBytes
        | Kind::Bytes
        | Kind::FString
        | Kind::RawFString
        | Kind::Integer
        | Kind::True
        | Kind::False
        | Kind::Binary
        | Kind::Octal
        | Kind::Hexadecimal
        | Kind::PointFloat
        | Kind::ExponentFloat
        | Kind::ImaginaryInteger
        | Kind::ImaginaryPointFloat
        | Kind::ImaginaryExponentFloat
        | Kind::None => true,
        _ => false,
    }
}

pub fn is_iterable(expr: &Expression) -> bool {
    match expr {
        Expression::List { .. }
        | Expression::Tuple { .. }
        | Expression::Set { .. }
        | Expression::Name { .. } => true,
        _ => false,
    }
}
