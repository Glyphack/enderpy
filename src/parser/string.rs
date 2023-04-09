use miette::Result;

use crate::parser::{ast::Expression, diagnostics};
use crate::token::Kind;

use super::ast::{Constant, Node};
pub fn extract_string_inside(val: String) -> String {
    if let Some(val) = val.strip_prefix("\"\"\"") {
        val.strip_suffix("\"\"\"")
            .expect("String must be enclosed with \"\"\"")
            .to_string()
    } else if let Some(val) = val.strip_prefix("\"") {
        val.strip_suffix("\"")
            .expect("String must be enclosed with \"")
            .to_string()
    } else if let Some(val) = val.strip_prefix("'''") {
        val.strip_suffix("'''")
            .expect("String must be enclosed with '''")
            .to_string()
    } else if let Some(val) = val.strip_prefix("'") {
        val.strip_suffix("'")
            .expect("String must be enclosed with '")
            .to_string()
    } else {
        panic!("String must be enclosed in \"\"\", \"', ''' or '");
    }
}

pub fn is_string(kind: &Kind) -> bool {
    match kind {
        Kind::StringLiteral
        | Kind::RawString
        | Kind::RawBytes
        | Kind::Bytes
        | Kind::FString
        | Kind::RawFString => true,
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
                (ConstantValue::Bytes(lhs), _) => {
                    return Err(
                        diagnostics::InvalidSyntax("Cannot concat bytes and string", node).into(),
                    )
                }
                (_, ConstantValue::Bytes(rhs)) => {
                    return Err(
                        diagnostics::InvalidSyntax("Cannot concat string and bytes", node).into(),
                    )
                }
                _ => panic!("Cannot concat string"),
            };
            Ok(concatnated_string)
        }
        _ => panic!("Cannot concat string"),
    }
}
