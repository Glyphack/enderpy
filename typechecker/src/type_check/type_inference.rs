use parser::ast;

use super::types::Type;

pub fn get_type_from_annotation(type_annotation: ast::Expression) -> Type {
    let expr_type = match type_annotation {
        ast::Expression::Name(name) => match name.id.as_str() {
            "int" => Type::Int,
            "float" => Type::Float,
            "str" => Type::Str,
            "bool" => Type::Bool,
            "None" => Type::None,
            _ => Type::Unknown,
        },
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
