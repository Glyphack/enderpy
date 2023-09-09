use parser::ast::{self, BinaryOperator, Expression};

use super::{builtins, types::Type};

pub fn get_type_from_annotation(type_annotation: &ast::Expression) -> Type {
    let expr_type = match type_annotation {
        ast::Expression::Name(name) => match name.id.as_str() {
            "int" => Type::Int,
            "float" => Type::Float,
            "str" => Type::Str,
            "bool" => Type::Bool,
            "None" => Type::None,
            _ => Type::Unknown,
        },
        Expression::Constant(c) => match c.value.clone() {
            ast::ConstantValue::Int(_) => Type::Int,
            ast::ConstantValue::Float(_) => Type::Float,
            ast::ConstantValue::Str(_) => Type::Str,
            ast::ConstantValue::Bool(_) => Type::Bool,
            ast::ConstantValue::None => Type::None,
            ast::ConstantValue::Bytes(_) => todo!(),
            ast::ConstantValue::Tuple(_) => todo!(),
            ast::ConstantValue::Complex { real, imaginary } => todo!(),
        },
        Expression::Subscript(s) => {
            // This is a generic type
            let name = match *s.value.clone() {
                Expression::Constant(_) => todo!(),
                Expression::List(_) => todo!(),
                Expression::Tuple(_) => todo!(),
                Expression::Dict(_) => todo!(),
                Expression::Set(_) => todo!(),
                Expression::Name(n) => get_builtin_type(n.id),
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
            Type::Class(super::types::ClassType {
                name,
                args: vec![get_type_from_annotation(&s.slice)],
            })
        }

        _ => Type::Unknown,
    };

    expr_type
}

pub fn type_equal(t1: &Type, t2: &Type) -> bool {
    match (t1, t2) {
        (Type::Int, Type::Int) => true,
        (Type::Float, Type::Float) => true,
        (Type::Str, Type::Str) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::None, Type::None) => true,
        _ => false,
    }
}

pub fn type_check_bin_op(t1: &Type, t2: &Type, op: &BinaryOperator) -> bool {
    let check_table = match op {
        BinaryOperator::Add => vec![(Type::Int, Type::Int), (Type::Float, Type::Float)],
        BinaryOperator::Sub => vec![(Type::Int, Type::Int), (Type::Float, Type::Float)],
        BinaryOperator::Mult => vec![
            (Type::Int, Type::Int),
            (Type::Float, Type::Float),
            (Type::Str, Type::Int),
            (Type::Int, Type::Str),
        ],
        BinaryOperator::Div => vec![(Type::Int, Type::Int), (Type::Float, Type::Float)],
        BinaryOperator::Mod => vec![(Type::Int, Type::Int), (Type::Float, Type::Float)],
        BinaryOperator::Pow => vec![(Type::Int, Type::Int), (Type::Float, Type::Float)],
        BinaryOperator::LShift => vec![(Type::Int, Type::Int)],
        BinaryOperator::RShift => vec![(Type::Int, Type::Int)],
        BinaryOperator::BitOr => vec![(Type::Int, Type::Int)],
        BinaryOperator::BitAnd => vec![(Type::Int, Type::Int)],
        BinaryOperator::BitXor => vec![(Type::Int, Type::Int)],
        BinaryOperator::FloorDiv => vec![(Type::Int, Type::Int), (Type::Float, Type::Float)],
        BinaryOperator::MatMult => vec![(Type::Int, Type::Int), (Type::Float, Type::Float)],
    };

    for (t1_, t2_) in check_table {
        if matches!(t1, Type::Unknown) || matches!(t2, Type::Unknown) {
            return true;
        }
        if type_equal(t1, &t1_) && type_equal(t2, &t2_) {
            return true;
        }
    }

    false
}

pub fn bin_op_result_type(t1: &Type, t2: &Type, op: &BinaryOperator) -> Type {
    if !type_check_bin_op(t1, t2, op) {
        return Type::Unknown;
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
            if type_equal(t1, &Type::Float) || type_equal(t2, &Type::Float) {
                return Type::Float;
            }
            if type_equal(t1, &Type::Int) || type_equal(t2, &Type::Int) {
                return Type::Int;
            }
            match t1 {
                Type::Str => Type::Str,
                Type::None => Type::None,
                Type::Unknown => Type::Unknown,
                Type::Bool => Type::Bool,
                Type::Int => Type::Int,
                Type::Float => Type::Float,
                _ => Type::Unknown,
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

pub fn is_subscriptable(t: &Type) -> bool {
    match t {
        Type::Class(c) => match c.name.as_str() {
            builtins::LIST_TYPE => true,
            _ => false,
        },
        _ => todo!(),
    }
}
