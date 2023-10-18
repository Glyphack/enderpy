use crate::parser::ast::Expression;
use crate::token::Kind;

pub fn is_atom(kind: &Kind) -> bool {
    match kind {
        Kind::Identifier
        | Kind::StringLiteral
        | Kind::RawBytes
        | Kind::Bytes
        | Kind::FStringStart
        | Kind::RawFStringStart
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
        // These might start a enclosured expression
        // https://docs.python.org/3/reference/expressions.html#atoms
        | Kind::LeftParen
        | Kind::LeftBracket
        | Kind::LeftBrace
        | Kind::Yield
        | Kind::Ellipsis
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
