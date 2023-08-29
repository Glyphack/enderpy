use miette::Result;

use crate::parser::ast::JoinedStr;
use crate::parser::{ast::Expression, diagnostics};
use crate::token::Kind;

use super::ast::Node;
pub fn extract_string_inside(val: String) -> String {
    if let Some(val) = val.strip_prefix("\"\"\"") {
        val.strip_suffix("\"\"\"")
            .expect("String must be enclosed with \"\"\"")
            .to_string()
    } else if let Some(val) = val.strip_prefix('\"') {
        val.strip_suffix('\"')
            .expect("String must be enclosed with \"")
            .to_string()
    } else if let Some(val) = val.strip_prefix("'''") {
        val.strip_suffix("'''")
            .expect("String must be enclosed with '''")
            .to_string()
    } else if let Some(val) = val.strip_prefix('\'') {
        val.strip_suffix('\'')
            .expect("String must be enclosed with '")
            .to_string()
    } else {
        panic!(
            "String must be enclosed in \"\"\", \"', ''' or ' but got {} ",
            val.starts_with('\'')
        );
    }
}

pub fn is_string(kind: &Kind) -> bool {
    match kind {
        Kind::StringLiteral
        | Kind::RawString
        | Kind::RawBytes
        | Kind::Bytes
        | Kind::FStringStart => true,
        _ => false,
    }
}

pub fn concat_string_exprs(lhs: Expression, rhs: Expression) -> Result<Expression> {
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
                    return Err(diagnostics::InvalidSyntax(
                        "Cannot concat bytes and string".to_string(),
                        node,
                    )
                    .into())
                }
                (_, ConstantValue::Bytes(_rhs)) => {
                    return Err(diagnostics::InvalidSyntax(
                        "Cannot concat string and bytes".to_string(),
                        node,
                    )
                    .into())
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
                    return Err(diagnostics::InvalidSyntax(
                        "Cannot concat string and bytes".to_string(),
                        const_rhs.node,
                    )
                    .into());
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
                    return Err(diagnostics::InvalidSyntax(
                        "Cannot concat string and bytes".to_string(),
                        const_lhs.node,
                    )
                    .into());
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
