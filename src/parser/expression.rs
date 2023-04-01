use crate::parser::ast::Expression;
use crate::token::Kind;

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
        // These might start a enclosured expression
        // https://docs.python.org/3/reference/expressions.html#atoms
        | Kind::LeftParen
        | Kind::LeftBracket
        | Kind::LeftBrace
        | Kind::Yield
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

// https://docs.python.org/3/reference/expressions.html#primaries
pub fn is_primary(expr: &Expression) -> bool {
    match expr {
        Expression::Name(_)
        | Expression::Constant(_)
        | Expression::List(_)
        | Expression::Tuple(_)
        | Expression::Dict(_)
        | Expression::Set(_)
        | Expression::Yield(_)
        | Expression::Generator(_)
        | Expression::ListComp(_)
        | Expression::SetComp(_)
        | Expression::DictComp(_)
        | Expression::Attribute(_)
        | Expression::Subscript(_)
        | Expression::Slice(_)
        | Expression::Call(_) => true,
        _ => false,
    }
}
