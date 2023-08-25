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
