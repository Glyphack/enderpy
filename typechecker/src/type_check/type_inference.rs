#![allow(dead_code)]
#![allow(unused_variables)]
use core::panic;

/// This module is resonsible for ineferring type from annotations or python expressions.
use enderpy_python_parser::ast::{self, BinaryOperator, Expression, Subscript, Name, Attribute};

use super::{
    builtins,
    types::{LiteralValue, PythonType},
};

const LITERAL_TYPE_PARAMETER_MSG: &str = "Type arguments for 'Literal' must be None, a literal value (int, bool, str, or bytes), or an enum value";
// TODO: this is not the right message there are other types like Dict that are allowed as parameters
const UNION_TYPE_PARAMETER_MSG: &str = "Type arguments for 'Union' must be names or literal values";

// This function tries to find the python type from an annotation expression
// If the annotation is invalid it returns uknown type
pub fn get_type_from_annotation(type_annotation: &ast::Expression) -> PythonType {
    log::debug!("Getting type from annotation: {:?}", type_annotation);
    let expr_type = match type_annotation {
        ast::Expression::Name(name) => match name.id.as_str() {
            "int" => PythonType::Int,
            "float" => PythonType::Float,
            "str" => PythonType::Str,
            "bool" => PythonType::Bool,
            "None" => PythonType::None,
            _ => PythonType::Unknown,
        },
        // Illegal type annotation
        Expression::Constant(c) => {
            if let ast::ConstantValue::None = c.value {
                PythonType::None
            } else {
                PythonType::Unknown
            }
        },
        Expression::Subscript(s) => {
            // This is a generic type
            let name = match *s.value.clone() {
                Expression::Constant(_) => todo!(),
                Expression::List(_) => todo!(),
                Expression::Tuple(_) => todo!(),
                Expression::Dict(_) => todo!(),
                Expression::Set(_) => todo!(),
                Expression::Name(n) => {
                    // TODO: handle builtins with enum
                    if is_literal(n.id.clone()) {
                        return handle_literal_type(s);
                    }
                    if is_union(n.id.clone()) {
                        match *s.slice.clone() {
                            Expression::Tuple(t) => {
                                return handle_union_type(t.elements);
                            },
                            expr@Expression::Name(_) | expr@Expression::Constant(_) | expr@Expression::Subscript(_) | expr@Expression::Slice(_) => {
                                return handle_union_type(vec![expr]);
                            },
                            _ => panic!("Union type must have a tuple as parameter"),
                        }
                    }
                    get_builtin_type(n.id)
                }
                Expression::BoolOp(_) => todo!(),
                Expression::UnaryOp(_) => todo!(),
                Expression::BinOp(_) => todo!(),
                Expression::NamedExpr(_) => todo!(),
                Expression::Yield(_) => todo!(),
                Expression::YieldFrom(_) => todo!(),
                Expression::Starred(_) => todo!(),
                Expression::Generator(_) => todo!(),
                Expression::ListComp(_) => todo!(),
                Expression::SetComp(_) => todo!(),
                Expression::DictComp(_) => todo!(),
                Expression::Attribute(_) => todo!(),
                Expression::Subscript(_) => todo!(),
                Expression::Slice(_) => todo!(),
                Expression::Call(_) => todo!(),
                Expression::Await(_) => todo!(),
                Expression::Compare(_) => todo!(),
                Expression::Lambda(_) => todo!(),
                Expression::IfExp(_) => todo!(),
                Expression::JoinedStr(_) => todo!(),
                Expression::FormattedValue(_) => todo!(),
            }
            .to_string();
            PythonType::Class(super::types::ClassType {
                name,
                args: vec![get_type_from_annotation(&s.slice)],
            })
        },
        Expression::BinOp(b) => {
            match b.op {
                BinaryOperator::BitOr => {
                    // flatten the bit or expression if the left and right are also bit or
                    let mut union_paramters = vec![];
                    // TODO handle when left and right are also binary operator
                    // Like a | b | c | d
                    union_paramters.push(*b.left.clone());
                    union_paramters.push(*b.right.clone());
                    handle_union_type(union_paramters)
                },
                _ => todo!(),
            }
        }

        _ => PythonType::Unknown,
    };

    expr_type
}


/// https://peps.python.org/pep-0484/#union-types
/// expressions are the parameters of the union type
/// in case of t1 | t2 | t3, expressions are [t1, t2, t3]
/// and in case of Union[t1, t2, t3], expressions are [t1, t2, t3]
fn handle_union_type(expressions: Vec<Expression>) -> PythonType {
    let mut types = vec![];
    for expr in expressions {
        let t =  get_type_from_annotation(&expr);
        if is_valid_union_parameter(&t) {
            log::debug!("Union type parameter: {:?}", t);
            types.push(t);
        }
    }

    // If we don't have any types in the union type, it means that all the parameters were invalid
    // So we return unknown type
    if types.is_empty() {
        return PythonType::Unknown;
    }

    PythonType::MultiValue(types)
}

/// TODO: Need to complete this when types are more complete
/// Check if a type can be used as a parameter for a union type
fn is_valid_union_parameter(python_type: &PythonType) -> bool {
    match python_type {
        _ => true,
    }
}

// https://peps.python.org/pep-0586
fn handle_literal_type(s: &Subscript) -> PythonType {
    // Only simple parameters are allowed for literal type:
    // https://peps.python.org/pep-0586/#legal-and-illegal-parameterizations
    let value = get_literal_value_from_param(&s.slice.clone());
    if value.len() > 1 {
        todo!("MultiValue literal type is not supported yet")
    }

    PythonType::KnownValue(super::types::KnownValue {
        literal_value: value.last().unwrap().clone(),
    })
}

/// Write a function that takes in an expression which is a parameter to a literal type and returns
/// the LiteralValue of the parameter.
/// Literal values might contain a tuple, that's why the return type is a vector.
pub fn get_literal_value_from_param(expr: &Expression) -> Vec<LiteralValue> {
    log::debug!("Getting literal value from param: {:?}", expr);
    let val = match expr {
        Expression::Constant(c) => {
            match c.value.clone() {
                ast::ConstantValue::Bool(b) => LiteralValue::Bool(b),
                ast::ConstantValue::Int(i) => LiteralValue::Int(i),
                ast::ConstantValue::Float(f) => LiteralValue::Float(f),
                ast::ConstantValue::Str(s) => LiteralValue::Str(s),
                ast::ConstantValue::Bytes(b) => LiteralValue::Bytes(b),
                ast::ConstantValue::None => LiteralValue::None,
                // Tuple is illegal if it has parantheses, otherwise it's allowed and the output a multiValued type
                // Currently even mypy does not supoort this, who am I to do it?
                // https://mypy-play.net/?mypy=latest&python=3.10&gist=0df0421d5c85f3b75f65a51cae8616ce
                ast::ConstantValue::Tuple(t) => {
                    if t.len() == 1 {
                        match t[0].value.clone() {
                            ast::ConstantValue::Bool(b) => LiteralValue::Bool(b),
                            ast::ConstantValue::Int(i) => LiteralValue::Int(i),
                            ast::ConstantValue::Float(f) => LiteralValue::Float(f),
                            ast::ConstantValue::Str(s) => LiteralValue::Str(s),
                            ast::ConstantValue::Bytes(b) => LiteralValue::Bytes(b),
                            ast::ConstantValue::None => LiteralValue::None,
                            _ => panic!("Tuple type with illegal parameter"),
                        }
                    } else {
                        let literal_values = t
                            .iter()
                            .map(|c| match c.value.clone() {
                                ast::ConstantValue::Bool(b) => LiteralValue::Bool(b),
                                ast::ConstantValue::Int(i) => LiteralValue::Int(i),
                                ast::ConstantValue::Float(f) => LiteralValue::Float(f),
                                ast::ConstantValue::Str(s) => LiteralValue::Str(s),
                                ast::ConstantValue::Bytes(b) => LiteralValue::Bytes(b),
                                ast::ConstantValue::None => LiteralValue::None,
                                _ => panic!("Tuple type with illegal parameter"),
                            })
                            .collect();
                        return literal_values;
                    }
                }
                // Illegal parameter
                ast::ConstantValue::Ellipsis => {
                    panic!("Literal type with ellipsis value is not supported")
                }
                ast::ConstantValue::Complex { real, imaginary } => {
                    panic!("Literal type with complex value is not supported")
                }
            }
        }
        // Only can be enum values
        Expression::Attribute(a) => {
            let value = match *a.value.clone() {
                Expression::Name(n) => n.id,
                _ => panic!("Literal type with attribute value can only be a name"),
            };
            LiteralValue::Str(value)
        }
        Expression::Subscript(s) => {
            match *s.value.clone() {
                Expression::Name(n) => {
                    if !is_literal(n.id.clone()) {
                        panic!("{}", LITERAL_TYPE_PARAMETER_MSG)
                    }
                    // When there is a literal inside a literal we flatten it
                    return get_literal_value_from_param(&s.slice);
                }
                _ => panic!("{}", LITERAL_TYPE_PARAMETER_MSG),
            };
        }
        // Illegal parameter
        _ => panic!("Literal type with illegal parameter, can only be a constant value or enum"),
    };

    vec![val]
}

pub fn type_equal(t1: &PythonType, t2: &PythonType) -> bool {
    match (t1, t2) {
        (PythonType::Int, PythonType::Int) => true,
        (PythonType::Float, PythonType::Float) => true,
        (PythonType::Str, PythonType::Str) => true,
        (PythonType::Bool, PythonType::Bool) => true,
        (PythonType::None, PythonType::None) => true,
        _ => false,
    }
}

pub fn type_check_bin_op(t1: &PythonType, t2: &PythonType, op: &BinaryOperator) -> bool {
    let check_table = match op {
        BinaryOperator::Add => vec![
            (PythonType::Int, PythonType::Int),
            (PythonType::Float, PythonType::Float),
        ],
        BinaryOperator::Sub => vec![
            (PythonType::Int, PythonType::Int),
            (PythonType::Float, PythonType::Float),
        ],
        BinaryOperator::Mult => vec![
            (PythonType::Int, PythonType::Int),
            (PythonType::Float, PythonType::Float),
            (PythonType::Str, PythonType::Int),
            (PythonType::Int, PythonType::Str),
        ],
        BinaryOperator::Div => vec![
            (PythonType::Int, PythonType::Int),
            (PythonType::Float, PythonType::Float),
        ],
        BinaryOperator::Mod => vec![
            (PythonType::Int, PythonType::Int),
            (PythonType::Float, PythonType::Float),
        ],
        BinaryOperator::Pow => vec![
            (PythonType::Int, PythonType::Int),
            (PythonType::Float, PythonType::Float),
        ],
        BinaryOperator::LShift => vec![(PythonType::Int, PythonType::Int)],
        BinaryOperator::RShift => vec![(PythonType::Int, PythonType::Int)],
        BinaryOperator::BitOr => vec![(PythonType::Int, PythonType::Int)],
        BinaryOperator::BitAnd => vec![(PythonType::Int, PythonType::Int)],
        BinaryOperator::BitXor => vec![(PythonType::Int, PythonType::Int)],
        BinaryOperator::FloorDiv => vec![
            (PythonType::Int, PythonType::Int),
            (PythonType::Float, PythonType::Float),
        ],
        BinaryOperator::MatMult => vec![
            (PythonType::Int, PythonType::Int),
            (PythonType::Float, PythonType::Float),
        ],
    };

    for (t1_, t2_) in check_table {
        if matches!(t1, PythonType::Unknown) || matches!(t2, PythonType::Unknown) {
            return true;
        }
        if type_equal(t1, &t1_) && type_equal(t2, &t2_) {
            return true;
        }
    }

    false
}

pub fn bin_op_result_type(t1: &PythonType, t2: &PythonType, op: &BinaryOperator) -> PythonType {
    if !type_check_bin_op(t1, t2, op) {
        return PythonType::Unknown;
    }

    match op {
        BinaryOperator::Add
        | BinaryOperator::Sub
        | BinaryOperator::Mult
        | BinaryOperator::MatMult
        | BinaryOperator::Div
        | BinaryOperator::Mod
        | BinaryOperator::Pow
        | BinaryOperator::LShift
        | BinaryOperator::RShift
        | BinaryOperator::BitOr
        | BinaryOperator::BitXor
        | BinaryOperator::BitAnd
        | BinaryOperator::FloorDiv => {
            if type_equal(t1, &PythonType::Float) || type_equal(t2, &PythonType::Float) {
                return PythonType::Float;
            }
            if type_equal(t1, &PythonType::Int) || type_equal(t2, &PythonType::Int) {
                return PythonType::Int;
            }
            match t1 {
                PythonType::Str => PythonType::Str,
                PythonType::None => PythonType::None,
                PythonType::Unknown => PythonType::Unknown,
                PythonType::Bool => PythonType::Bool,
                PythonType::Int => PythonType::Int,
                PythonType::Float => PythonType::Float,
                _ => PythonType::Unknown,
            }
        }
    }
}

pub fn get_builtin_type(name: String) -> String {
    match name.as_str() {
        "list" => builtins::LIST_TYPE.to_string(),
        _ => name.clone(),
    }
}

pub fn is_literal(name: String) -> bool {
    match name.as_str() {
        "Literal" => true,
        _ => false,
    }
}
fn is_union(clone: String) -> bool {
    match clone.as_str() {
        "Union" => true,
        _ => false,
    }
}

pub fn is_subscriptable(t: &PythonType) -> bool {
    match t {
        PythonType::Class(c) => match c.name.as_str() {
            builtins::LIST_TYPE => true,
            _ => false,
        },
        _ => todo!(),
    }
}
