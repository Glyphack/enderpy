#![allow(dead_code)]
#![allow(unused_variables)]

use core::panic;
use std::collections::HashMap;

use enderpy_python_parser as parser;
use enderpy_python_parser::ast;
use log::debug;
use miette::{bail, miette, Result};
use parser::ast::{Expression, GetNode, Statement};

use super::{
    builtins,
    types::{CallableType, LiteralValue, PythonType},
};
use crate::{
    ast_visitor::TraversalVisitor,
    ast_visitor_generic::TraversalVisitorImmutGeneric,
    nodes::EnderpyFile,
    state::State,
    symbol_table::{self, Declaration, LookupSymbolRequest, SymbolTable, SymbolTableNode},
    type_check::types::ClassType,
};

const LITERAL_TYPE_PARAMETER_MSG: &str = "Type arguments for 'Literal' must be None, a literal value (int, bool, str, or bytes), or an enum value";
// TODO: this is not the right message there are other types like Dict that are
// allowed as parameters
const UNION_TYPE_PARAMETER_MSG: &str = "Type arguments for 'Union' must be names or literal values";

pub struct TypeEvaluator {
    // TODO: make this a reference to the symbol table in the checker
    pub symbol_table: SymbolTable,
    pub imported_symbol_tables: Vec<SymbolTable>,
}

pub struct TypeEvalError {
    pub message: String,
    pub position: usize,
}

/// Struct for evaluating the type of an expression
impl TypeEvaluator {
    /// Get the type of a symbol node based on declarations
    pub fn get_symbol_node_type(
        &self,
        symbol: &SymbolTableNode,
        position: Option<usize>,
    ) -> Result<PythonType> {
        let decl = match position {
            Some(position) => symbol.declaration_until_position(position),
            None => symbol.last_declaration(),
        };

        log::debug!("fetch symbol declaration: {:?}", decl);
        match decl {
            Some(decl) => self.get_type_from_declaration(decl),
            None => Ok(PythonType::Any),
        }
    }

    /// Entry point function to get type of an expression. The expression passed
    /// to this function must not be annotations, for example if you want to
    /// get the type of a variable declaration you should pass the value of
    /// the declaration to this function. To get the type of an annotation
    /// expression use get_type_from_annotation
    pub fn get_type(&self, expr: &ast::Expression) -> Result<PythonType> {
        log::debug!("get_type: {:?}", expr);
        match expr {
            ast::Expression::Constant(c) => {
                let typ = match &c.value {
                    // We should consider constants are not literals unless they are explicitly
                    // declared as such https://peps.python.org/pep-0586/#type-inference
                    ast::ConstantValue::Int(_) => PythonType::Int,
                    ast::ConstantValue::Float(_) => PythonType::Float,
                    ast::ConstantValue::Str(_) => PythonType::Str,
                    ast::ConstantValue::Bool(_) => PythonType::Bool,
                    ast::ConstantValue::None => PythonType::None,
                    _ => PythonType::Unknown,
                };
                Ok(typ)
            }
            ast::Expression::Name(n) => {
                self.infer_type_from_symbol_table(&n.id, Some(n.node.start))
            }
            ast::Expression::Call(call) => {
                let func = *call.func.clone();
                match func {
                    ast::Expression::Name(n) => {
                        // check if name is one of the builtins
                        if builtins::BUILTINS.contains(&n.id.as_str()) {
                            return Ok(PythonType::Unknown);
                        }
                        let f_type = self.infer_type_from_symbol_table(n.id.as_str(), None)?;
                        log::debug!("f_type: {:?}", f_type);
                        match f_type {
                            PythonType::Callable(callable_type) => Ok(callable_type.return_type),
                            PythonType::Never => Ok(PythonType::Never),
                            _ => Err(miette!("{} is not callable", n.id)),
                        }
                    }
                    ast::Expression::Attribute(_a) => Ok(PythonType::Unknown),
                    _ => {
                        debug!("infer type from call not implemented");
                        Ok(PythonType::Unknown)
                    }
                }
            }
            ast::Expression::BinOp(b) => Ok(self.bin_op_result_type(
                &self.get_type(&b.left)?,
                &self.get_type(&b.right)?,
                &b.op,
            )),
            ast::Expression::List(l) => {
                let final_elm_type = self.get_sequence_type_from_elements(&l.elements);
                let builtin_type = self.get_builtin_type(builtins::LIST_TYPE);
                if let Some(builtin_type) = builtin_type {
                    Ok(PythonType::Class(ClassType::new(
                        builtin_type,
                        vec![final_elm_type],
                    )))
                } else {
                    Ok(PythonType::Unknown)
                }
            }
            ast::Expression::Tuple(t) => {
                let elm_type = self.get_sequence_type_from_elements(&t.elements);
                let builtin_type = self.get_builtin_type(builtins::TUPLE_TYPE);
                if let Some(builtin_type) = builtin_type {
                    Ok(PythonType::Class(ClassType::new(
                        builtin_type,
                        vec![elm_type],
                    )))
                } else {
                    Ok(PythonType::Unknown)
                }
            }
            ast::Expression::Dict(d) => {
                let key_type = self.get_sequence_type_from_elements(&d.keys);
                let value_type = self.get_sequence_type_from_elements(&d.values);
                let builtin_type = self.get_builtin_type(builtins::DICT_TYPE);
                if let Some(builtin_type) = builtin_type {
                    Ok(PythonType::Class(ClassType::new(
                        builtin_type,
                        vec![key_type, value_type],
                    )))
                } else {
                    Ok(PythonType::Unknown)
                }
            }
            ast::Expression::Set(s) => {
                let elm_type = self.get_sequence_type_from_elements(&s.elements);
                let builtin_type = self.get_builtin_type(builtins::SET_TYPE);
                if let Some(builtin_type) = builtin_type {
                    Ok(PythonType::Class(ClassType::new(
                        builtin_type,
                        vec![elm_type],
                    )))
                } else {
                    Ok(PythonType::Unknown)
                }
            }
            ast::Expression::BoolOp(_) => Ok(PythonType::Bool),
            ast::Expression::UnaryOp(u) => match u.op {
                ast::UnaryOperator::Not => Ok(PythonType::Bool),
                ast::UnaryOperator::Invert => match self.get_type(&u.operand)? {
                    PythonType::Int => Ok(PythonType::Int),
                    _ => bail!(
                        "cannot invert type {}",
                        self.get_type(&u.operand)?.to_string()
                    ),
                },
                _ => self.get_type(&u.operand),
            },
            ast::Expression::NamedExpr(e) => self.get_type(&e.value),
            ast::Expression::Yield(a) => {
                let yield_type = match a.value {
                    Some(ref v) => self.get_type(v)?,
                    None => PythonType::None,
                };
                let builtin_type = self.get_builtin_type(builtins::ITER_TYPE);
                todo!()
            }
            ast::Expression::YieldFrom(yf) => {
                let yield_type = match *yf.value.clone() {
                    ast::Expression::List(l) => self.get_sequence_type_from_elements(&l.elements),
                    _ => panic!("TODO: infer type from yield from"),
                };
                todo!()
            }
            ast::Expression::Starred(s) => Ok(PythonType::Unknown),
            ast::Expression::Generator(g) => {
                // This is not correct
                // let mut comp_targets: HashMap<String, Type> = HashMap::new();
                // for gens in &g.generators {
                //     match *gens.target.clone() {
                //         ast::Expression::Name(n) => {
                //             comp_targets.insert(n.id, self.get_type(&gens.iter));
                //         }
                //         _ => panic!("comperhension target must be a name, or does it?"),
                //     }
                // }

                Ok(PythonType::Unknown)
            }
            ast::Expression::ListComp(_) => Ok(PythonType::Unknown),
            ast::Expression::SetComp(_) => Ok(PythonType::Unknown),
            ast::Expression::DictComp(_) => Ok(PythonType::Unknown),
            ast::Expression::Attribute(a) => Ok(PythonType::Unknown),
            ast::Expression::Subscript(s) => {
                let value_type = &self.get_type(&s.value)?;
                // This only handles container types and TODO
                Ok(value_type.clone())
            }
            ast::Expression::Slice(_) => Ok(PythonType::Unknown),
            ast::Expression::Await(_) => Ok(PythonType::Unknown),
            ast::Expression::Compare(_) => Ok(PythonType::Bool),
            ast::Expression::Lambda(_) => Ok(PythonType::Unknown),
            ast::Expression::IfExp(_) => Ok(PythonType::Unknown),
            ast::Expression::JoinedStr(_) => Ok(PythonType::Str),
            ast::Expression::FormattedValue(f) => self.get_type(&f.value),
        }
    }

    // This function tries to find the python type from an annotation expression
    // If the annotation is invalid it returns uknown type
    pub fn get_type_from_annotation(&self, type_annotation: &ast::Expression) -> PythonType {
        log::debug!("Getting type from annotation: {:?}", type_annotation);
        let expr_type = match type_annotation {
            Expression::Name(name) => match name.id.as_str() {
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
            }
            Expression::Subscript(s) => {
                let type_parameters = vec![self.get_type_from_annotation(&s.slice)];
                // This is a generic type
                let typ = self.get_class_declaration(*s.value.clone(), None);
                PythonType::Class(ClassType {
                    details: typ,
                    type_parameters,
                })
            }
            Expression::BinOp(b) => {
                match b.op {
                    ast::BinaryOperator::BitOr => {
                        // flatten the bit or expression if the left and right are also bit or
                        // TODO handle when left and right are also binary operator
                        // Like a | b | c | d
                        let union_paramters = self.flatten_bit_or(b);
                        self.handle_union_type(union_paramters)
                    }
                    _ => todo!(),
                }
            }

            _ => PythonType::Unknown,
        };

        expr_type
    }

    fn get_type_from_declaration(&self, declaration: &Declaration) -> Result<PythonType> {
        match declaration {
            Declaration::Variable(v) => {
                if let Some(type_annotation) = &v.type_annotation {
                    Ok(self.get_type_from_annotation(type_annotation))
                } else if let Some(source) = &v.inferred_type_source {
                    self.get_type(source)
                } else {
                    Ok(PythonType::Unknown)
                }
            }
            Declaration::Function(f) => {
                let annotated_return_type =
                    if let Some(type_annotation) = f.function_node.returns.clone() {
                        self.get_type_from_annotation(&type_annotation)
                    } else {
                        let inferred_return_type = self.infer_function_return_type(f);
                        log::debug!("infered_return_type: {:?}", inferred_return_type);
                        inferred_return_type
                    };

                let arguments = f.function_node.args.clone();
                let name = f.function_node.name.clone();

                Ok(PythonType::Callable(Box::new(CallableType {
                    name,
                    arguments,
                    return_type: annotated_return_type,
                })))
            }
            Declaration::Class(_) => Ok(PythonType::Unknown),
            Declaration::Parameter(p) => {
                if let Some(type_annotation) = &p.type_annotation {
                    Ok(self.get_type_from_annotation(type_annotation))
                } else if let Some(default) = &p.default_value {
                    self.get_type(default)
                } else {
                    Ok(PythonType::Any)
                }
            }
            Declaration::Alias(_) => Ok(PythonType::Unknown),
            Declaration::TypeParameter(_) => Ok(PythonType::Unknown),
            Declaration::TypeAlias(_) => Ok(PythonType::Unknown),
        }
    }

    fn infer_type_from_symbol_table(
        &self,
        name: &str,
        position: Option<usize>,
    ) -> Result<PythonType> {
        let lookup_request = LookupSymbolRequest {
            name: name.to_string(),
            position,
        };
        let result = match self.symbol_table.lookup_in_scope(lookup_request) {
            Some(symbol) => self.get_symbol_node_type(symbol, position),
            None => Ok(PythonType::Unknown),
        };
        log::debug!("infer_type_from_symbol_table: {:?} => {:?}", name, result);
        result
    }

    fn get_sequence_type_from_elements(&self, elements: &Vec<ast::Expression>) -> PythonType {
        let mut prev_elm_type = PythonType::Unknown;
        for elm in elements {
            let elm_type = self.get_type(elm).unwrap_or(PythonType::Unknown);
            if prev_elm_type == PythonType::Unknown {
                prev_elm_type = elm_type;
            } else if prev_elm_type != elm_type {
                prev_elm_type = PythonType::Unknown;
                break;
            }
        }
        prev_elm_type
    }

    fn infer_function_return_type(&self, f: &crate::symbol_table::Function) -> PythonType {
        if !f.is_abstract() && !f.raise_statements.is_empty() {
            return PythonType::Never;
        }
        if !f.yeild_statements.is_empty() {
            let mut yield_types = vec![];
            for yield_statement in &f.yeild_statements {
                if let Some(value) = &yield_statement.value {
                    yield_types.push(self.get_type(value).unwrap_or(PythonType::Unknown));
                }
            }
            if yield_types.len() == 1 {
                todo!()
                // return PythonType::Class(super::types::ClassType {
                //     name: builtins::ITER_TYPE.to_string(),
                //     args: vec![yield_types[0].clone()],
                // });
            } else {
                // TODO: Union type
                return PythonType::Unknown;
            }
        }
        if f.return_statements.is_empty() {
            PythonType::None
        } else {
            let mut return_types = vec![];
            for return_statement in &f.return_statements {
                if let Some(value) = &return_statement.value {
                    return_types.push(self.get_type(value).unwrap_or(PythonType::Unknown));
                }
            }
            if return_types.len() == 1 {
                return_types[0].clone()
            } else {
                // TODO: Union type
                PythonType::Unknown
            }
        }
    }

    /// Retrieves a pythoh type that is present in the builtin scope
    fn get_builtin_type(&self, name: &str) -> Option<symbol_table::Class> {
        let builtin_symbol = self.symbol_table.lookup_in_builtin_scope(name);
        let cls_declaration = match builtin_symbol {
            None => {
                log::debug!("builtin type {} not found", name);
                None
            }
            Some(node) => {
                // get the declaration with type class
                node.declarations.iter().find_map(|decl| match decl {
                    Declaration::Class(c) => Some(Declaration::Class(c.clone())),
                    _ => None,
                })
            }
        };

        match cls_declaration {
            None => None,
            Some(Declaration::Class(c)) => Some(c),
            _ => panic!("builtin type {} not found", name),
        }
    }

    /// This function flattens a chain of bit or expressions
    /// For example: a | b | c | d
    /// will be flattened to [a, b, c, d]
    fn flatten_bit_or(&self, b: &ast::BinOp) -> Vec<Expression> {
        let mut union_parameters = vec![];
        let mut current_expr = b.left.clone();

        while let Expression::BinOp(inner_binop) = *current_expr.clone() {
            if let ast::BinaryOperator::BitOr = inner_binop.op {
                union_parameters.push(*inner_binop.right.clone());
                current_expr = inner_binop.left;
            } else {
                union_parameters.push(*current_expr.clone());
                break;
            }
        }

        union_parameters.push(*current_expr.clone());

        current_expr = b.right.clone();

        while let Expression::BinOp(inner_binop) = *current_expr.clone() {
            if let ast::BinaryOperator::BitOr = inner_binop.op {
                union_parameters.push(*inner_binop.right.clone());
                current_expr = inner_binop.left;
            } else {
                union_parameters.push(*current_expr.clone());
                break;
            }
        }

        union_parameters.push(*current_expr.clone());
        union_parameters
    }

    /// https://peps.python.org/pep-0484/#union-types
    /// expressions are the parameters of the union type
    /// in case of t1 | t2 | t3, expressions are [t1, t2, t3]
    /// and in case of Union[t1, t2, t3], expressions are [t1, t2, t3]
    fn handle_union_type(&self, expressions: Vec<Expression>) -> PythonType {
        log::debug!("Handling union type with members: {:?}", expressions);
        let mut types = vec![];
        for expr in expressions {
            let t = self.get_type_from_annotation(&expr);
            if self.is_valid_union_parameter(&t) {
                log::debug!("Union type parameter: {:?}", t);
                types.push(t);
            }
        }

        // If we don't have any types in the union type, it means that all the
        // parameters were invalid So we return unknown type
        if types.is_empty() {
            return PythonType::Unknown;
        }

        PythonType::MultiValue(types)
    }

    /// TODO: Need to complete this when types are more complete
    /// Check if a type can be used as a parameter for a union type
    fn is_valid_union_parameter(&self, python_type: &PythonType) -> bool {
        true
    }

    // https://peps.python.org/pep-0586
    fn handle_literal_type(&self, s: &ast::Subscript) -> PythonType {
        // Only simple parameters are allowed for literal type:
        // https://peps.python.org/pep-0586/#legal-and-illegal-parameterizations
        let value = self.get_literal_value_from_param(&s.slice.clone());
        if value.len() > 1 {
            todo!("MultiValue literal type is not supported yet")
        }

        PythonType::KnownValue(super::types::KnownValue {
            literal_value: value.last().unwrap().clone(),
        })
    }

    /// Write a function that takes in an expression which is a parameter to a
    /// literal type and returns the LiteralValue of the parameter.
    /// Literal values might contain a tuple, that's why the return type is a
    /// vector.
    pub fn get_literal_value_from_param(&self, expr: &Expression) -> Vec<LiteralValue> {
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
                    // Tuple is illegal if it has parantheses, otherwise it's allowed and the output
                    // a multiValued type Currently even mypy does not supoort
                    // this, who am I to do it? https://mypy-play.net/?mypy=latest&python=3.10&gist=0df0421d5c85f3b75f65a51cae8616ce
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
                        if !self.is_literal(n.id.clone()) {
                            panic!("{}", LITERAL_TYPE_PARAMETER_MSG)
                        }
                        // When there is a literal inside a literal we flatten it
                        return self.get_literal_value_from_param(&s.slice);
                    }
                    _ => panic!("{}", LITERAL_TYPE_PARAMETER_MSG),
                };
            }
            // Illegal parameter
            _ => {
                panic!("Literal type with illegal parameter, can only be a constant value or enum")
            }
        };

        vec![val]
    }

    pub fn type_equal(&self, t1: &PythonType, t2: &PythonType) -> bool {
        t1.type_equal(t2)
    }

    pub fn type_check_bin_op(
        &self,
        t1: &PythonType,
        t2: &PythonType,
        op: &ast::BinaryOperator,
    ) -> bool {
        let check_table = match op {
            ast::BinaryOperator::Add => vec![
                (PythonType::Int, PythonType::Int),
                (PythonType::Float, PythonType::Float),
            ],
            ast::BinaryOperator::Sub => vec![
                (PythonType::Int, PythonType::Int),
                (PythonType::Float, PythonType::Float),
            ],
            ast::BinaryOperator::Mult => vec![
                (PythonType::Int, PythonType::Int),
                (PythonType::Float, PythonType::Float),
                (PythonType::Str, PythonType::Int),
                (PythonType::Int, PythonType::Str),
            ],
            ast::BinaryOperator::Div => vec![
                (PythonType::Int, PythonType::Int),
                (PythonType::Float, PythonType::Float),
            ],
            ast::BinaryOperator::Mod => vec![
                (PythonType::Int, PythonType::Int),
                (PythonType::Float, PythonType::Float),
            ],
            ast::BinaryOperator::Pow => vec![
                (PythonType::Int, PythonType::Int),
                (PythonType::Float, PythonType::Float),
            ],
            ast::BinaryOperator::LShift => vec![(PythonType::Int, PythonType::Int)],
            ast::BinaryOperator::RShift => vec![(PythonType::Int, PythonType::Int)],
            ast::BinaryOperator::BitOr => vec![(PythonType::Int, PythonType::Int)],
            ast::BinaryOperator::BitAnd => vec![(PythonType::Int, PythonType::Int)],
            ast::BinaryOperator::BitXor => vec![(PythonType::Int, PythonType::Int)],
            ast::BinaryOperator::FloorDiv => vec![
                (PythonType::Int, PythonType::Int),
                (PythonType::Float, PythonType::Float),
            ],
            ast::BinaryOperator::MatMult => vec![
                (PythonType::Int, PythonType::Int),
                (PythonType::Float, PythonType::Float),
            ],
        };

        for (t1_, t2_) in check_table {
            if matches!(t1, PythonType::Unknown) || matches!(t2, PythonType::Unknown) {
                return true;
            }
            if self.type_equal(t1, &t1_) && self.type_equal(t2, &t2_) {
                return true;
            }
        }

        false
    }

    pub fn bin_op_result_type(
        &self,
        t1: &PythonType,
        t2: &PythonType,
        op: &ast::BinaryOperator,
    ) -> PythonType {
        if !self.type_check_bin_op(t1, t2, op) {
            return PythonType::Unknown;
        }

        match op {
            ast::BinaryOperator::Add
            | ast::BinaryOperator::Sub
            | ast::BinaryOperator::Mult
            | ast::BinaryOperator::MatMult
            | ast::BinaryOperator::Div
            | ast::BinaryOperator::Mod
            | ast::BinaryOperator::Pow
            | ast::BinaryOperator::LShift
            | ast::BinaryOperator::RShift
            | ast::BinaryOperator::BitOr
            | ast::BinaryOperator::BitXor
            | ast::BinaryOperator::BitAnd
            | ast::BinaryOperator::FloorDiv => {
                if self.type_equal(t1, &PythonType::Float)
                    || self.type_equal(t2, &PythonType::Float)
                {
                    return PythonType::Float;
                }
                if self.type_equal(t1, &PythonType::Int) || self.type_equal(t2, &PythonType::Int) {
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

    pub fn is_literal(&self, name: String) -> bool {
        name.as_str() == "Literal"
    }

    fn is_union(&self, clone: String) -> bool {
        clone.as_str() == "Union"
    }

    pub fn is_subscriptable(&self, t: &PythonType) -> bool {
        if let PythonType::Class(c) = t {
            let class_name = c.details.name.as_str();
            return matches!(class_name, builtins::LIST_TYPE)
                || matches!(class_name, builtins::TUPLE_TYPE)
                || matches!(class_name, builtins::DICT_TYPE)
                || matches!(class_name, builtins::SET_TYPE);
        }

        false
    }

    fn get_class_declaration(&self, value: Expression, symbbol_table: Option<SymbolTable>) -> symbol_table::Class {
        let symbol_table = match symbbol_table {
            Some(s) => s,
            None => self.symbol_table.clone(),
        }; 
        match value {
            Expression::Constant(_) => todo!(),
            Expression::List(_) => todo!(),
            Expression::Tuple(_) => todo!(),
            Expression::Dict(_) => todo!(),
            Expression::Set(_) => todo!(),
            // This is a Generic type with a param: Container[type, ...]
            Expression::Name(n) => {
                if let Some(builtin_type) = self.get_builtin_type(n.id.as_str()) {
                    builtin_type
                // if it's not a builtin we want to get the class declaration form symbol table
                // and find where this class is defined
                } else {
                    let container_decl =
                        match symbol_table.lookup_in_scope(LookupSymbolRequest {
                            name: n.id.clone(),
                            position: Some(n.node.start),
                        }) {
                            Some(decl) => decl.last_declaration(),
                            None => panic!("Type {} has no declaration", n.id),
                        };
                    match container_decl {
                        Some(Declaration::Class(c)) => c.clone(),
                        Some(Declaration::Alias(a)) => self.resolve_alias(a),
                        _ => panic!("Type {} not found {:?}", n.id, container_decl),
                    }
                }
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
    }

    // Follows Alias declaration and resolves it to a class declaration
    // It searches through imported symbol tables for the module alias imports
    // and resolves the alias to the class declaration
    // TODO: refactor to resolve all aliases and not only classes
    fn resolve_alias(&self, a: &symbol_table::Alias) -> symbol_table::Class {
        let class_name = match a.symbol_name {
            Some(ref name) => name.clone(),
            None => panic!("Alias {:?} has no symbol name", a.import_node),
        };
        let imported_symbol_tables = self.imported_symbol_tables.clone();
        log::debug!(
            "Searching for alias {}",
            class_name,
        );
        for symbol_table in imported_symbol_tables {
            // TODO: only check the imports from typing for now.
            if symbol_table.module_name
                != ".Users.glyphack.Programming.enderpy.typeshed.stdlib.typing.pyi"
            {
                continue;
            }
            if let Some(decl) = symbol_table.lookup_in_scope(LookupSymbolRequest {
                name: class_name.clone(),
                position: None,
            }) {
                match decl.last_declaration() {
                    Some(c) => {
                        log::debug!("Found declaration: {:?}", c);
                        match c {
                            Declaration::Class(c) => return c.clone(),
                            Declaration::Alias(a) => return self.resolve_alias(a),
                            Declaration::Variable(v) => {
                                let type_annotation = v.type_annotation.clone();
                                log::debug!("Type annotation for class is {:?} now searching for {:?} in symbol table", type_annotation, type_annotation.clone());
                                match type_annotation {
                                    Some(t) => {
                                        log::debug!("Type annotation for class is {:?} now searching for {:?} in symbol table", t, t.clone());
                                        return self.get_class_declaration(t, Some(symbol_table));
                                    }
                                    None => continue,
                                }
                            }
                            _ => panic!("Cannot resolve alias to class"),
                        }
                    }
                    _ => panic!("Alias has no declaration"),
                }
            }
        }
        panic!("Alias {} not found", class_name);
    }
}

impl TraversalVisitorImmutGeneric<PythonType> for TypeEvaluator {
    fn visit_stmt(&self, s: &ast::Statement) -> PythonType {
        // map all statements and call visit
        match s {
            ast::Statement::ExpressionStatement(e) => self.visit_expr(e),
            ast::Statement::Import(i) => self.visit_import(i),
            ast::Statement::ImportFrom(i) => self.visit_import_from(i),
            ast::Statement::AssignStatement(a) => self.visit_assign(a),
            ast::Statement::AnnAssignStatement(a) => self.visit_ann_assign(a),
            ast::Statement::AugAssignStatement(a) => self.visit_aug_assign(a),
            ast::Statement::Assert(a) => self.visit_assert(a),
            ast::Statement::Pass(p) => self.visit_pass(p),
            ast::Statement::Delete(d) => self.visit_delete(d),
            ast::Statement::Return(r) => self.visit_return(r),
            ast::Statement::Raise(r) => self.visit_raise(r),
            ast::Statement::Break(b) => self.visit_break(b),
            ast::Statement::Continue(c) => self.visit_continue(c),
            ast::Statement::Global(g) => self.visit_global(g),
            ast::Statement::Nonlocal(n) => self.visit_nonlocal(n),
            ast::Statement::IfStatement(i) => self.visit_if(i),
            ast::Statement::WhileStatement(w) => self.visit_while(w),
            ast::Statement::ForStatement(f) => self.visit_for(f),
            ast::Statement::WithStatement(w) => self.visit_with(w),
            ast::Statement::TryStatement(t) => self.visit_try(t),
            ast::Statement::TryStarStatement(t) => self.visit_try_star(t),
            ast::Statement::FunctionDef(f) => self.visit_function_def(f),
            ast::Statement::ClassDef(c) => self.visit_class_def(c),
            ast::Statement::Match(m) => self.visit_match(m),
            Statement::AsyncForStatement(f) => self.visit_async_for(f),
            Statement::AsyncWithStatement(w) => self.visit_async_with(w),
            Statement::AsyncFunctionDef(f) => self.visit_async_function_def(f),
            Statement::TypeAlias(a) => self.visit_type_alias(a),
        }
    }

    fn visit_expr(&self, e: &ast::Expression) -> PythonType {
        match e {
            ast::Expression::Constant(c) => self.visit_constant(c),
            ast::Expression::List(l) => self.visit_list(l),
            ast::Expression::Tuple(t) => self.visit_tuple(t),
            ast::Expression::Dict(d) => self.visit_dict(d),
            ast::Expression::Set(s) => self.visit_set(s),
            ast::Expression::Name(n) => self.visit_name(n),
            ast::Expression::BoolOp(b) => self.visit_bool_op(b),
            ast::Expression::UnaryOp(u) => self.visit_unary_op(u),
            ast::Expression::BinOp(b) => self.visit_bin_op(b),
            ast::Expression::NamedExpr(n) => self.visit_named_expr(n),
            ast::Expression::Yield(y) => self.visit_yield(y),
            ast::Expression::YieldFrom(y) => self.visit_yield_from(y),
            ast::Expression::Starred(s) => self.visit_starred(s),
            ast::Expression::Generator(g) => self.visit_generator(g),
            ast::Expression::ListComp(l) => self.visit_list_comp(l),
            ast::Expression::SetComp(s) => self.visit_set_comp(s),
            ast::Expression::DictComp(d) => self.visit_dict_comp(d),
            ast::Expression::Attribute(a) => self.visit_attribute(a),
            ast::Expression::Subscript(s) => self.visit_subscript(s),
            ast::Expression::Slice(s) => self.visit_slice(s),
            ast::Expression::Call(c) => self.visit_call(c),
            ast::Expression::Await(a) => self.visit_await(a),
            ast::Expression::Compare(c) => self.visit_compare(c),
            ast::Expression::Lambda(l) => self.visit_lambda(l),
            ast::Expression::IfExp(i) => self.visit_if_exp(i),
            ast::Expression::JoinedStr(j) => self.visit_joined_str(j),
            ast::Expression::FormattedValue(f) => self.visit_formatted_value(f),
        }
    }

    fn visit_import(&self, _i: &ast::Import) -> PythonType {
        PythonType::Unknown
    }

    fn visit_import_from(&self, _i: &ast::ImportFrom) -> PythonType {
        PythonType::Unknown
    }

    fn visit_if(&self, i: &parser::ast::If) -> PythonType {
        PythonType::Unknown
    }

    fn visit_while(&self, w: &parser::ast::While) -> PythonType {
        PythonType::Unknown
    }

    fn visit_for(&self, f: &parser::ast::For) -> PythonType {
        PythonType::Unknown
    }

    fn visit_with(&self, w: &parser::ast::With) -> PythonType {
        PythonType::Unknown
    }

    fn visit_try(&self, t: &parser::ast::Try) -> PythonType {
        PythonType::Unknown
    }

    fn visit_try_star(&self, t: &parser::ast::TryStar) -> PythonType {
        PythonType::Unknown
    }

    fn visit_function_def(&self, f: &parser::ast::FunctionDef) -> PythonType {
        PythonType::Any
    }

    fn visit_class_def(&self, c: &parser::ast::ClassDef) -> PythonType {
        PythonType::Unknown
    }

    fn visit_match(&self, m: &parser::ast::Match) -> PythonType {
        PythonType::Unknown
    }

    fn visit_constant(&self, _c: &ast::Constant) -> PythonType {
        PythonType::Unknown
    }

    fn visit_list(&self, _l: &ast::List) -> PythonType {
        PythonType::Unknown
    }

    fn visit_tuple(&self, _t: &ast::Tuple) -> PythonType {
        PythonType::Unknown
    }

    fn visit_dict(&self, _d: &ast::Dict) -> PythonType {
        PythonType::Unknown
    }

    fn visit_set(&self, _s: &ast::Set) -> PythonType {
        PythonType::Unknown
    }

    fn visit_name(&self, _n: &ast::Name) -> PythonType {
        PythonType::Unknown
    }

    fn visit_bool_op(&self, _b: &ast::BoolOperation) -> PythonType {
        PythonType::Unknown
    }

    fn visit_unary_op(&self, _u: &ast::UnaryOperation) -> PythonType {
        PythonType::Unknown
    }

    fn visit_bin_op(&self, _b: &ast::BinOp) -> PythonType {
        PythonType::Unknown
    }

    fn visit_named_expr(&self, _n: &ast::NamedExpression) -> PythonType {
        PythonType::Unknown
    }

    fn visit_yield(&self, _y: &ast::Yield) -> PythonType {
        PythonType::Unknown
    }

    fn visit_yield_from(&self, _y: &ast::YieldFrom) -> PythonType {
        PythonType::Unknown
    }

    fn visit_starred(&self, _s: &ast::Starred) -> PythonType {
        PythonType::Unknown
    }

    fn visit_generator(&self, _g: &ast::Generator) -> PythonType {
        PythonType::Unknown
    }

    fn visit_list_comp(&self, _l: &ast::ListComp) -> PythonType {
        PythonType::Unknown
    }

    fn visit_set_comp(&self, _s: &ast::SetComp) -> PythonType {
        PythonType::Unknown
    }

    fn visit_dict_comp(&self, _d: &ast::DictComp) -> PythonType {
        PythonType::Unknown
    }

    fn visit_attribute(&self, _a: &ast::Attribute) -> PythonType {
        PythonType::Unknown
    }

    fn visit_subscript(&self, _s: &ast::Subscript) -> PythonType {
        PythonType::Unknown
    }

    fn visit_slice(&self, _s: &ast::Slice) -> PythonType {
        PythonType::Unknown
    }

    fn visit_call(&self, _c: &ast::Call) -> PythonType {
        PythonType::Unknown
    }

    fn visit_await(&self, _a: &ast::Await) -> PythonType {
        PythonType::Unknown
    }

    fn visit_compare(&self, _c: &ast::Compare) -> PythonType {
        PythonType::Unknown
    }

    fn visit_lambda(&self, _l: &ast::Lambda) -> PythonType {
        PythonType::Unknown
    }

    fn visit_if_exp(&self, _i: &ast::IfExp) -> PythonType {
        PythonType::Unknown
    }

    fn visit_joined_str(&self, _j: &ast::JoinedStr) -> PythonType {
        PythonType::Unknown
    }

    fn visit_formatted_value(&self, _f: &ast::FormattedValue) -> PythonType {
        PythonType::Unknown
    }

    fn visit_alias(&self, _a: &ast::Alias) -> PythonType {
        PythonType::Unknown
    }

    fn visit_assign(&self, _a: &ast::Assign) -> PythonType {
        PythonType::Unknown
    }

    fn visit_ann_assign(&self, _a: &ast::AnnAssign) -> PythonType {
        PythonType::Unknown
    }

    fn visit_aug_assign(&self, _a: &ast::AugAssign) -> PythonType {
        PythonType::Unknown
    }

    fn visit_assert(&self, _a: &ast::Assert) -> PythonType {
        PythonType::Unknown
    }

    fn visit_pass(&self, _p: &ast::Pass) -> PythonType {
        PythonType::Unknown
    }

    fn visit_delete(&self, _d: &ast::Delete) -> PythonType {
        PythonType::Unknown
    }

    fn visit_return(&self, _r: &ast::Return) -> PythonType {
        PythonType::Unknown
    }

    fn visit_raise(&self, _r: &ast::Raise) -> PythonType {
        PythonType::Unknown
    }

    fn visit_break(&self, _b: &ast::Break) -> PythonType {
        PythonType::Unknown
    }

    fn visit_continue(&self, _c: &ast::Continue) -> PythonType {
        PythonType::Unknown
    }

    fn visit_global(&self, _g: &ast::Global) -> PythonType {
        PythonType::Unknown
    }

    fn visit_nonlocal(&self, _n: &ast::Nonlocal) -> PythonType {
        PythonType::Unknown
    }
}

/// visits the ast and calls get_type on each expression and saves that type in
/// the types hashmap the key is the position of the expression in the source:
/// (line, start, end)
struct TypeEvalVisitor {
    pub type_eval: TypeEvaluator,
    pub types: HashMap<String, PythonType>,
    pub state: State,
}

impl TypeEvalVisitor {
    pub fn new(enderpy_file: EnderpyFile) -> Self {
        let mut state = State::new(enderpy_file);
        state.populate_symbol_table();
        let symbol_table = state.get_symbol_table();
        Self {
            types: HashMap::new(),
            type_eval: TypeEvaluator {
                symbol_table,
                imported_symbol_tables: vec![],
            },
            state,
        }
    }

    pub fn enderpy_file(&self) -> &EnderpyFile {
        &self.state.file
    }

    /// This function is called on every expression in the ast
    pub fn save_type(&mut self, expr: &ast::Expression) {
        let typ = self.type_eval.get_type(expr).unwrap_or(PythonType::Unknown);
        log::debug!("save_type: {:?} => {:?}", expr, typ);
        let start_pos = self.enderpy_file().get_position(expr.get_node().start);
        let end_pos = self.enderpy_file().get_position(expr.get_node().end);
        self.types.insert(format!("{}:{}", start_pos, end_pos), typ);
    }

    // TODO: move type annotation tests to its own file
    pub fn save_type_annotation(&mut self, expr: &ast::Expression) {
        let typ = self.type_eval.get_type_from_annotation(expr);
        log::debug!("save_type: {:?} => {:?}", expr, typ);
        let start_pos = self.enderpy_file().get_position(expr.get_node().start);
        let end_pos = self.enderpy_file().get_position(expr.get_node().end);
        self.types.insert(format!("{}:{}", start_pos, end_pos), typ);
    }

    fn visit_module(&mut self) {
        let body = self.enderpy_file().body.clone();
        for statement in body.iter() {
            self.visit_stmt(statement);
        }
    }
}

/// Traverse the ast and call call save_type on each expression
impl TraversalVisitor for TypeEvalVisitor {
    fn visit_stmt(&mut self, s: &ast::Statement) {
        // map all statements and call visit
        match s {
            ast::Statement::ExpressionStatement(e) => self.visit_expr(e),
            ast::Statement::Import(i) => {}
            ast::Statement::ImportFrom(i) => {}
            ast::Statement::AssignStatement(a) => {
                self.save_type(&a.value);
            }
            ast::Statement::AnnAssignStatement(a) => {
                if let Some(v) = a.value.as_ref() {
                    self.save_type(v);
                }

                self.save_type_annotation(&a.annotation)
            }
            ast::Statement::AugAssignStatement(a) => self.visit_aug_assign(a),
            ast::Statement::Assert(a) => self.visit_assert(a),
            ast::Statement::Pass(p) => self.visit_pass(p),
            ast::Statement::Delete(d) => self.visit_delete(d),
            ast::Statement::Return(r) => {
                if let Some(r) = r.value.as_ref() {
                    self.visit_expr(r);
                    self.save_type(r);
                }
            }
            ast::Statement::Raise(r) => {
                if let Some(r) = r.exc.as_ref() {
                    self.save_type(r);
                }
                if let Some(r) = r.cause.as_ref() {
                    self.save_type(r);
                }
            }
            ast::Statement::Break(b) => self.visit_break(b),
            ast::Statement::Continue(c) => self.visit_continue(c),
            ast::Statement::Global(g) => self.visit_global(g),
            ast::Statement::Nonlocal(n) => self.visit_nonlocal(n),
            ast::Statement::IfStatement(i) => self.visit_if(i),
            ast::Statement::WhileStatement(w) => self.visit_while(w),
            ast::Statement::ForStatement(f) => self.visit_for(f),
            ast::Statement::WithStatement(w) => self.visit_with(w),
            ast::Statement::TryStatement(t) => self.visit_try(t),
            ast::Statement::TryStarStatement(t) => self.visit_try_star(t),
            ast::Statement::FunctionDef(f) => {
                for stmt in &f.body {
                    self.visit_stmt(stmt);
                }
            }
            ast::Statement::ClassDef(c) => self.visit_class_def(c),
            ast::Statement::Match(m) => self.visit_match(m),
            Statement::AsyncForStatement(f) => self.visit_async_for(f),
            Statement::AsyncWithStatement(w) => self.visit_async_with(w),
            Statement::AsyncFunctionDef(f) => self.visit_async_function_def(f),
            Statement::TypeAlias(a) => self.visit_type_alias(a),
        }
    }

    fn visit_expr(&mut self, e: &ast::Expression) {
        match e {
            ast::Expression::Constant(c) => self.visit_constant(c),
            ast::Expression::List(l) => self.visit_list(l),
            ast::Expression::Tuple(t) => self.visit_tuple(t),
            ast::Expression::Dict(d) => self.visit_dict(d),
            ast::Expression::Set(s) => self.visit_set(s),
            ast::Expression::Name(n) => self.visit_name(n),
            ast::Expression::BoolOp(b) => self.visit_bool_op(b),
            ast::Expression::UnaryOp(u) => self.visit_unary_op(u),
            ast::Expression::BinOp(b) => {
                self.save_type(&b.left);
                self.save_type(&b.right);
            }
            ast::Expression::NamedExpr(n) => self.visit_named_expr(n),
            ast::Expression::Yield(y) => self.visit_yield(y),
            ast::Expression::YieldFrom(y) => self.visit_yield_from(y),
            ast::Expression::Starred(s) => self.visit_starred(s),
            ast::Expression::Generator(g) => self.visit_generator(g),
            ast::Expression::ListComp(l) => self.visit_list_comp(l),
            ast::Expression::SetComp(s) => self.visit_set_comp(s),
            ast::Expression::DictComp(d) => self.visit_dict_comp(d),
            ast::Expression::Attribute(a) => self.visit_attribute(a),
            ast::Expression::Subscript(s) => self.visit_subscript(s),
            ast::Expression::Slice(s) => self.visit_slice(s),
            ast::Expression::Call(c) => self.visit_call(c),
            ast::Expression::Await(a) => self.visit_await(a),
            ast::Expression::Compare(c) => self.visit_compare(c),
            ast::Expression::Lambda(l) => self.visit_lambda(l),
            ast::Expression::IfExp(i) => self.visit_if_exp(i),
            ast::Expression::JoinedStr(j) => self.visit_joined_str(j),
            ast::Expression::FormattedValue(f) => self.visit_formatted_value(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};

    use insta::glob;

    use super::*;
    use crate::build_source::BuildSource;

    fn snapshot_type_eval(source: &str) -> String {
        use enderpy_python_parser::Parser;

        let mut parser = Parser::new(source.to_string(), "".into());
        let ast_module = parser.parse();

        let enderpy_file = EnderpyFile::from(
            ast_module,
            Box::new(BuildSource {
                path: PathBuf::from(""),
                source: source.to_string(),
                module: "test".to_string(),
                followed: false,
            }),
            vec![],
        );

        let mut module = State::new(enderpy_file);
        module.populate_symbol_table();
        let symbol_table = module.get_symbol_table();

        let type_eval = TypeEvaluator {
            symbol_table,
            imported_symbol_tables: vec![],
        };

        let mut type_eval_visitor = TypeEvalVisitor::new(module.file);
        type_eval_visitor.visit_module();

        let result = type_eval_visitor.types;

        // sort result by key
        let mut result_sorted = result.clone().into_iter().collect::<Vec<_>>();
        result_sorted.sort_by(|a, b| a.0.cmp(&b.0));

        format!("{:#?}", result_sorted)
    }

    #[test]
    fn test_type_evaluator() {
        glob!("test_data/inputs/", "*.py", |path| {
            let contents = fs::read_to_string(path).unwrap();
            let result = snapshot_type_eval(&contents);
            let _ = env_logger::builder()
                .filter_level(log::LevelFilter::Debug)
                .is_test(true)
                .try_init();

            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("./test_data/output/");
            settings.set_description(fs::read_to_string(path).unwrap());
            settings.bind(|| {
                insta::assert_snapshot!(result);
            });
        })
    }
}
