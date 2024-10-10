pub mod compat;
use crate::ast;
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
    match token.kind {
        Kind::If
        | Kind::While
        | Kind::For
        | Kind::Try
        | Kind::With
        | Kind::Def
        | Kind::Class
        | Kind::Type
        | Kind::Match
        // Decorator
        | Kind::MatrixMul
        | Kind::Async => true,
        _ => false,
    }
}

// TODO: performance
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

pub fn concat_string_exprs(lhs: Expression, rhs: Expression) -> Result<Expression, ParsingError> {
    use crate::parser::ast::{Constant, ConstantValue};
    match (lhs, rhs) {
        (Expression::Constant(lhs), Expression::Constant(rhs)) => {
            let node = Node {
                start: lhs.node.start,
                end: rhs.node.end,
            };
            let concatnated_string = match (lhs.value, rhs.value) {
                (ConstantValue::Str, ConstantValue::Str) => {
                    Expression::Constant(Box::new(Constant {
                        node,
                        value: ConstantValue::Str,
                    }))
                }
                (ConstantValue::Bytes, ConstantValue::Bytes) => {
                    Expression::Constant(Box::new(Constant {
                        node,
                        value: ConstantValue::Bytes,
                    }))
                }
                (ConstantValue::Bytes, _) => {
                    panic!("Cannot concat bytes and string");
                }
                (_, ConstantValue::Bytes) => {
                    panic!("Can only concat bytes with other bytes");
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
                ConstantValue::Str => {
                    values.push(Expression::Constant(Box::new(Constant {
                        node: const_rhs.node,
                        value: ConstantValue::Str,
                    })));
                }
                ConstantValue::Bytes => {
                    panic!("Cannot concat string and bytes");
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
                ConstantValue::Str => Expression::Constant(Box::new(Constant {
                    node: const_lhs.node,
                    value: ConstantValue::Str,
                })),
                ConstantValue::Bytes => {
                    panic!("Cannot concat string and bytes");
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

pub fn map_unary_operator(kind: &Kind) -> UnaryOperator {
    match kind {
        Kind::Not => UnaryOperator::Not,
        Kind::BitNot => UnaryOperator::Invert,
        Kind::Minus => UnaryOperator::USub,
        Kind::Plus => UnaryOperator::UAdd,
        _ => panic!("Not a unary operator"),
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
