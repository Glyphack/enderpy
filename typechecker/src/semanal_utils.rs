use parser::ast::{ConstantValue, Expression};

use crate::symbol_table::NodeType;

pub fn get_target_name(assignment_target: &Expression) -> String {
    match assignment_target {
        Expression::Name(n) => n.id.clone(),
        _ => panic!("cannot defer name of the assignment target"),
    }
}


pub fn get_constant_value_type(constant: &ConstantValue) -> NodeType {
    match constant {
        ConstantValue::Str(_) => NodeType::String,
        _ => panic!("not impl"),
    }
}
