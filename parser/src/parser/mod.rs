pub mod ast;
#[allow(clippy::module_inception)]
pub mod parser;
use crate::token::{Kind, Token};
use ast::{Node, UnaryOperator};
use miette::Result;

use crate::{
    error::ParsingError,
    parser::ast::{Expression, JoinedStr},
};
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
        | Kind::Async => true,
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

pub fn extract_string_inside(val: String) -> String {
    let delimiters = vec!["\"\"\"", "\"", "'''", "'"];
    let mut result = String::new();
    let is_raw = val.starts_with('r') || val.starts_with('R');
    // drop first char if raw and put in val
    let val = if is_raw {
        val.chars().skip(1).collect::<String>()
    } else {
        val
    };

    for delimiter in delimiters {
        // TODO: The string value data structure should be changed so we can be sure
        // that the string is enclosed with the delimiter and not check for it here
        if let Some(val) = val.strip_prefix(delimiter) {
            let message = format!("String must be enclosed with {}", delimiter);
            result = val.strip_suffix(delimiter).expect(&message).to_string();
            break;
        }
    }
    // add r back if raw ternary
    if is_raw {
        result = format!("r\"{}\"", result);
    }

    result
}

pub fn is_string(kind: &Kind) -> bool {
    matches!(
        kind,
        Kind::StringLiteral | Kind::RawBytes | Kind::Bytes | Kind::FStringStart
    )
}

pub fn concat_string_exprs(lhs: Expression, rhs: Expression) -> Result<Expression, ParsingError> {
    use crate::parser::ast::{Constant, ConstantValue};
    match (lhs, rhs) {
        (Expression::Constant(lhs), Expression::Constant(rhs)) => {
            let node = Node {
                start: lhs.node.start,
                end: rhs.node.end,
            };
            let concatnated_string = match (lhs.value, rhs.value) {
                (ConstantValue::Str(lhs_val), ConstantValue::Str(rhs_val)) => {
                    Expression::Constant(Box::new(Constant {
                        node,
                        value: ConstantValue::Str(lhs_val + &rhs_val),
                    }))
                }
                (ConstantValue::Bytes(mut lhs), ConstantValue::Bytes(rhs)) => {
                    lhs.append(&mut rhs.clone());
                    Expression::Constant(Box::new(Constant {
                        node,
                        value: ConstantValue::Bytes(lhs),
                    }))
                }
                (ConstantValue::Bytes(_lhs), _) => {
                    return Err(ParsingError::InvalidSyntax {
                        msg: "Cannot concat bytes and string".into(),
                        input: "test".into(),
                        advice: "test".into(),
                        span: (0, 0),
                    });
                }
                (_, ConstantValue::Bytes(_rhs)) => {
                    return Err(ParsingError::InvalidSyntax {
                        msg: "Can only concat bytes with other bytes".into(),
                        input: "test".into(),
                        advice: "test".into(),
                        span: (0, 0),
                    });
                }
                _ => panic!("Cannot concat string"),
            };
            Ok(concatnated_string)
        }
        (Expression::JoinedStr(fstring_lhs), Expression::JoinedStr(fstring_rhs)) => {
            let mut values = fstring_lhs.values;
            values.extend(fstring_rhs.values);
            Ok(Expression::JoinedStr(Box::new(JoinedStr {
                node: Node {
                    start: fstring_lhs.node.start,
                    end: fstring_rhs.node.end,
                },
                values,
            })))
        }
        (Expression::JoinedStr(fstring_lhs), Expression::Constant(const_rhs)) => {
            let mut values = fstring_lhs.values;
            match const_rhs.value {
                ConstantValue::Str(rhs_val) => {
                    values.push(Expression::Constant(Box::new(Constant {
                        node: const_rhs.node,
                        value: ConstantValue::Str(rhs_val),
                    })));
                }
                ConstantValue::Bytes(_) => {
                    return Err(ParsingError::InvalidSyntax {
                        msg: "Cannot concat string and bytes".into(),
                        input: "test".into(),
                        advice: "test".into(),
                        span: (0, 0),
                    });
                }
                _ => panic!("Cannot concat string"),
            }
            Ok(Expression::JoinedStr(Box::new(JoinedStr {
                node: Node {
                    start: fstring_lhs.node.start,
                    end: const_rhs.node.end,
                },
                values,
            })))
        }
        (Expression::Constant(const_lhs), Expression::JoinedStr(fstring_rhs)) => {
            let const_expr = match const_lhs.value {
                ConstantValue::Str(rhs_val) => Expression::Constant(Box::new(Constant {
                    node: const_lhs.node,
                    value: ConstantValue::Str(rhs_val),
                })),
                ConstantValue::Bytes(_) => {
                    return Err(ParsingError::InvalidSyntax {
                        msg: "Cannot concat string and bytes".into(),
                        input: "test".into(),
                        advice: "test".into(),
                        span: (0, 0),
                    });
                }
                _ => panic!("Cannot concat string"),
            };
            let mut values = vec![const_expr];
            values.extend(fstring_rhs.values);
            Ok(Expression::JoinedStr(Box::new(JoinedStr {
                node: Node {
                    start: const_lhs.node.start,
                    end: fstring_rhs.node.end,
                },
                values,
            })))
        }
        _ => panic!("Cannot concat string"),
    }
}

pub fn is_unary_op(kind: &Kind) -> bool {
    matches!(kind, Kind::Not | Kind::BitNot | Kind::Minus | Kind::Plus)
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
    matches!(
        kind,
        Kind::Plus
            | Kind::Minus
            | Kind::Mul
            | Kind::MatrixMul
            | Kind::Div
            | Kind::Mod
            | Kind::Pow
            | Kind::IntDiv
    )
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
    matches!(
        expr,
        Expression::List { .. }
            | Expression::Tuple { .. }
            | Expression::Set { .. }
            | Expression::Name { .. }
    )
}

/// Checks weather a token kind can start a bitwise operation
/// start of bitwise operation cannot be a await primary
#[allow(dead_code)]
pub fn is_bitwise_or_op(cur_kind: &Kind) -> bool {
    match cur_kind {
        Kind::Plus | Kind::Minus | Kind::BitNot | Kind::Await => return false,
        _ => (),
    }
    if is_atom(cur_kind) {
        return false;
    }
    true
}
