#![allow(dead_code)]
#![allow(unused_variables)]
/// This module is resonsible for ineferring type from annotations or python expressions.

use enderpy_python_parser::ast::{self, BinaryOperator, Expression, Subscript};

use super::{builtins, types::{PythonType, LiteralValue}};

pub fn get_type_from_annotation(type_annotation: &ast::Expression) -> PythonType {
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
        Expression::Constant(c) => PythonType::Unknown,
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
                    if n.id == "Literal" {
                        return handle_literal_type(s);
                    }
                    get_builtin_type(n.id)
                },
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
        }

        _ => PythonType::Unknown,
    };

    expr_type
}

fn handle_literal_type(s: &Subscript) -> PythonType {
    // Only simple parameters are allowed for literal type:
    // https://peps.python.org/pep-0586/#legal-and-illegal-parameterizations
    
    let value = match *s.slice.clone() {
        Expression::Constant(c) => {
            match c.value {
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
                            ast::ConstantValue::Bool(b) => LiteralValue::Bool(b.clone()),
                            ast::ConstantValue::Int(i) => LiteralValue::Int(i),
                            ast::ConstantValue::Float(f) => LiteralValue::Float(f),
                            ast::ConstantValue::Str(s) => LiteralValue::Str(s),
                            ast::ConstantValue::Bytes(b) => LiteralValue::Bytes(b),
                            ast::ConstantValue::None => LiteralValue::None,
                            _ => panic!("Tuple type with illegal parameter"),
                        }
                    } else {
                    // TODO: this is a multiValued type
                    return PythonType::Unknown;
                    }
                },
                // Illegal parameter
                ast::ConstantValue::Ellipsis => panic!("Literal type with ellipsis value is not supported"),
                ast::ConstantValue::Complex { real, imaginary } => panic!("Literal type with complex value is not supported"),
            }
        },
        // Only can be enum values
        Expression::Attribute(a) => {
            let value = match *a.value.clone() {
                Expression::Name(n) => n.id,
                _ => panic!("Literal type with attribute value can only be a name"),
            };
            LiteralValue::Str(value)
        },
        // Illegal parameter
        _ => panic!("Literal type with illegal parameter, can only be a constant value or enum"),
    };

    PythonType::KnownValue(super::types::KnownValue { literal_value: value })
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

pub fn is_subscriptable(t: &PythonType) -> bool {
    match t {
        PythonType::Class(c) => match c.name.as_str() {
            builtins::LIST_TYPE => true,
            _ => false,
        },
        _ => todo!(),
    }
}
