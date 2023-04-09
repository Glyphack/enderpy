use crate::parser::ast::Expression;
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

pub fn concat_string_exprs(lhs: Expression, rhs: Expression) -> Expression {
    use crate::parser::ast::{Constant, ConstantValue};
    match (lhs, rhs) {
        (Expression::Constant(lhs), Expression::Constant(rhs)) => {
            let concatnated_string = match (lhs.value, rhs.value) {
                (ConstantValue::Str(lhs), ConstantValue::Str(rhs)) => lhs + &rhs,
                _ => panic!("Cannot concat string"),
            };
            let node_start = lhs.node.start;
            let node_end = rhs.node.end;
            Expression::Constant(Box::new(Constant {
                node: Node {
                    start: node_start,
                    end: node_end,
                },
                value: ConstantValue::Str(concatnated_string),
            }))
        }
        _ => panic!("Cannot concat string"),
    }
}
