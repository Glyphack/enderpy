use parser::ast::{ConstantValue, Expression};

use crate::symbol_table::NodeType;

pub fn get_target_name(assignment_target: &Expression) -> String {
    match assignment_target {
        Expression::Name(n) => n.id,
        _ => panic!("cannot defer name of the assignment target"),
    }
}

pub fn get_expr_type(expr: &Expression) -> NodeType {
    match expr {
        Expression::Constant(c) => get_constant_value_type(&c.value),
        _ => panic!("not implemented type"),
    }
}

pub fn get_constant_value_type(constant: &ConstantValue) -> NodeType {
    match constant {
        ConstantValue::Str(_) => NodeType::String,
        _ => panic!("not impl"),
    }
}
