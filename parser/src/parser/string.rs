use miette::Result;

use crate::parser::ast::Expression;
use crate::parser::ast::JoinedStr;
use crate::token::Kind;

use super::ast::Node;
use crate::error::ParsingError;
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
    match kind {
        Kind::StringLiteral | Kind::RawBytes | Kind::Bytes | Kind::FStringStart => true,
        _ => false,
    }
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
                    })
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
