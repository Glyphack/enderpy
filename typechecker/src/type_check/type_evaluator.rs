#![allow(dead_code)]
#![allow(unused_variables)]

use miette::{bail, miette, Result};
use enderpy_python_parser::ast;
use enderpy_python_parser as parser;
use parser::ast::Statement;

use crate::{
    ast_visitor_generic::TraversalVisitorImmutGeneric,
    symbol_table::{Declaration, SymbolTable, SymbolTableNode},
};

use super::{
    builtins, type_inference,
    types::{CallableType, PythonType},
};

pub struct TypeEvaluator {
    // TODO: make this a reference to the symbol table in the checker
    pub symbol_table: SymbolTable,
}

pub struct TypeEvalError {
    pub message: String,
    pub position: usize,
}

impl TypeEvaluator {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self { symbol_table }
    }

    pub fn get_symbol_node_type(&self, symbol: &SymbolTableNode, position: usize) -> Result<PythonType> {
        let decl = symbol
            .declaration_until_position(position)
            .ok_or_else(|| miette!("symbol {} is not defined", symbol.name))?;
        self
            .get_type_from_declaration(decl)
            .map_err(|e| miette!("cannot infer type for symbol {}: {}", symbol.name, e))
    }
    pub fn get_type(&self, expr: &ast::Expression) -> Result<PythonType> {
        match expr {
            ast::Expression::Constant(c) => {
                let typ = match c.value {
                            ast::ConstantValue::Int(_) => PythonType::Int,
                            ast::ConstantValue::Float(_) => PythonType::Float,
                            ast::ConstantValue::Str(_) => PythonType::Str,
                            ast::ConstantValue::Bool(_) => PythonType::Bool,
                            ast::ConstantValue::None => PythonType::None,
                            _ => PythonType::Unknown,
                        };
                Ok(typ)
            },
            ast::Expression::Name(n) => {
                self.infer_type_from_symbol_table(&n.id, n.node.start)
            },
            ast::Expression::Call(call) => {
                let func = *call.func.clone();
                match func {
                    ast::Expression::Name(n) => {
                        // check if name is one of the builtins
                        if builtins::BUILTINS.contains(&n.id.as_str()) {
                            return Ok(PythonType::Unknown);
                        }
                        let f_type = self.infer_type_from_symbol_table(n.id.as_str(), n.node.start)?;
                        match f_type {
                            PythonType::Callable(callable_type) => Ok(callable_type.return_type),
                            _ => bail!("{} is not callable", n.id)
                        }
                    }
                    ast::Expression::Attribute(_a) => panic!("TODO: infer type from attribute"),
                    _ => {
                        println!("infer type from call not implemented");
                        Ok(PythonType::Unknown)
                    },
                }
            }
            ast::Expression::BinOp(b) => Ok(type_inference::bin_op_result_type(
                &self.get_type(&b.left)?,
                &self.get_type(&b.right)?,
                &b.op,
            )),
            ast::Expression::List(l) => {
                let final_elm_type = self.get_sequence_type_from_elements(&l.elements);
                Ok(PythonType::Class(super::types::ClassType {
                    name: builtins::LIST_TYPE.to_string(),
                    args: vec![final_elm_type],
                }))
            }
            ast::Expression::Tuple(t) => {
                let elm_type = self.get_sequence_type_from_elements(&t.elements);
                Ok(PythonType::Class(super::types::ClassType {
                    name: builtins::TUPLE_TYPE.to_string(),
                    args: vec![elm_type],
                }))
            }
            ast::Expression::Dict(d) => {
                let key_type = self.get_sequence_type_from_elements(&d.keys);
                let value_type = self.get_sequence_type_from_elements(&d.values);
                Ok(PythonType::Class(super::types::ClassType {
                    name: builtins::DICT_TYPE.to_string(),
                    args: vec![key_type, value_type],
                }))
            }
            ast::Expression::Set(s) => {
                let elm_type = self.get_sequence_type_from_elements(&s.elements);
                Ok(PythonType::Class(super::types::ClassType {
                    name: builtins::SET_TYPE.to_string(),
                    args: vec![elm_type],
                }))
            }
            ast::Expression::BoolOp(_) => Ok(PythonType::Bool),
            ast::Expression::UnaryOp(u) => {
                match u.op {
                    ast::UnaryOperator::Not => Ok(PythonType::Bool),
                    ast::UnaryOperator::Invert => {
                        match self.get_type(&u.operand)? {
                            PythonType::Int => Ok(PythonType::Int),
                            _ => bail!("cannot invert type {}", self.get_type(&u.operand)?.to_string()),
                        }
                    }
                    _ => self.get_type(&u.operand),
                }
            }
            ast::Expression::NamedExpr(e) => self.get_type(&e.value),
            ast::Expression::Yield(a) => {
                let yield_type = match a.value {
                    Some(ref v) => self.get_type(v)?,
                    None => PythonType::None,
                };
                Ok(PythonType::Class(super::types::ClassType {
                    name: builtins::ITER_TYPE.to_string(),
                    args: vec![yield_type],
                }))
            }
            ast::Expression::YieldFrom(yf) => {
                let yield_type = match *yf.value.clone() {
                    ast::Expression::List(l) => self.get_sequence_type_from_elements(&l.elements),
                    _ => panic!("TODO: infer type from yield from"),
                };
                Ok(PythonType::Class(super::types::ClassType {
                    name: builtins::ITER_TYPE.to_string(),
                    args: vec![yield_type],
                }))
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
            ast::Expression::ListComp(_) => todo!(),
            ast::Expression::SetComp(_) => todo!(),
            ast::Expression::DictComp(_) => todo!(),
            ast::Expression::Attribute(a) => Ok(PythonType::Unknown),
            ast::Expression::Subscript(s) => {
                let value_type = &self.get_type(&s.value)?;
                // if the type of value is subscriptable, then return the type of the subscript

                // the type is subscriptable if it is a list, tuple, dict, or set
                Ok(match value_type {
                    PythonType::Class(class_type) => {
                        if let Some(args) = class_type.args.last() {
                            args.clone()
                        } else {
                            PythonType::Unknown
                        }
                    }
                    _ => PythonType::Unknown,
                })
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

    fn get_type_from_declaration(&self, declaration: &Declaration) -> Result<PythonType> {
        

        match declaration {
            Declaration::Variable(v) => {
                if let Some(type_annotation) = &v.type_annotation {
                    Ok(type_inference::get_type_from_annotation(type_annotation))
                } else if let Some(source) = &v.inferred_type_source {
                    self.get_type(source)
                } else {
                    bail!("var declaration must have a type annotation or inferred type")
                }
            }
            Declaration::Function(f) => {
                let return_type = if let Some(type_annotation) = f.function_node.returns.clone() {
                    type_inference::get_type_from_annotation(&type_annotation)
                } else {
                    println!("TODO: infer return type from function body");
                    PythonType::Unknown
                };

                let arguments = f.function_node.args.clone();
                let name = f.function_node.name.clone();

                Ok(PythonType::Callable(Box::new(CallableType {
                    name,
                    arguments,
                    return_type,
                })))
            }
            Declaration::Class(_) => Ok(PythonType::Unknown),
            Declaration::Parameter(_) => Ok(PythonType::Unknown),
        }
    }

    fn infer_type_from_symbol_table(&self, name: &str, position: usize) -> Result<PythonType> {
        match self.symbol_table.lookup_in_scope(name) {
            Some(symbol) => self.get_symbol_node_type(symbol, position),
            None => bail!("undefined name {}", name),
        }
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
        todo!();
    }

    fn visit_import_from(&self, _i: &ast::ImportFrom) -> PythonType {
        todo!();
    }

    fn visit_if(&self, i: &parser::ast::If) -> PythonType {
        todo!();
    }

    fn visit_while(&self, w: &parser::ast::While) -> PythonType {
        todo!();
    }

    fn visit_for(&self, f: &parser::ast::For) -> PythonType {
        todo!();
    }

    fn visit_with(&self, w: &parser::ast::With) -> PythonType {
        todo!();
    }

    fn visit_try(&self, t: &parser::ast::Try) -> PythonType {
        todo!();
    }

    fn visit_try_star(&self, t: &parser::ast::TryStar) -> PythonType {
        todo!();
    }

    fn visit_function_def(&self, f: &parser::ast::FunctionDef) -> PythonType {
        todo!();
    }

    fn visit_class_def(&self, c: &parser::ast::ClassDef) -> PythonType {
        todo!();
    }

    fn visit_match(&self, m: &parser::ast::Match) -> PythonType {
        todo!();
    }

    fn visit_constant(&self, _c: &ast::Constant) -> PythonType {
        todo!();
    }

    fn visit_list(&self, _l: &ast::List) -> PythonType {
        todo!()
    }

    fn visit_tuple(&self, _t: &ast::Tuple) -> PythonType {
        todo!()
    }

    fn visit_dict(&self, _d: &ast::Dict) -> PythonType {
        todo!()
    }

    fn visit_set(&self, _s: &ast::Set) -> PythonType {
        todo!()
    }

    fn visit_name(&self, _n: &ast::Name) -> PythonType {
        todo!()
    }

    fn visit_bool_op(&self, _b: &ast::BoolOperation) -> PythonType {
        todo!()
    }

    fn visit_unary_op(&self, _u: &ast::UnaryOperation) -> PythonType {
        todo!()
    }

    fn visit_bin_op(&self, _b: &ast::BinOp) -> PythonType {
        todo!()
    }

    fn visit_named_expr(&self, _n: &ast::NamedExpression) -> PythonType {
        todo!()
    }

    fn visit_yield(&self, _y: &ast::Yield) -> PythonType {
        todo!()
    }

    fn visit_yield_from(&self, _y: &ast::YieldFrom) -> PythonType {
        todo!()
    }

    fn visit_starred(&self, _s: &ast::Starred) -> PythonType {
        todo!()
    }

    fn visit_generator(&self, _g: &ast::Generator) -> PythonType {
        todo!()
    }

    fn visit_list_comp(&self, _l: &ast::ListComp) -> PythonType {
        todo!()
    }

    fn visit_set_comp(&self, _s: &ast::SetComp) -> PythonType {
        todo!()
    }

    fn visit_dict_comp(&self, _d: &ast::DictComp) -> PythonType {
        todo!()
    }

    fn visit_attribute(&self, _a: &ast::Attribute) -> PythonType {
        todo!()
    }

    fn visit_subscript(&self, _s: &ast::Subscript) -> PythonType {
        todo!()
    }

    fn visit_slice(&self, _s: &ast::Slice) -> PythonType {
        todo!()
    }

    fn visit_call(&self, _c: &ast::Call) -> PythonType {
        todo!()
    }

    fn visit_await(&self, _a: &ast::Await) -> PythonType {
        todo!()
    }

    fn visit_compare(&self, _c: &ast::Compare) -> PythonType {
        todo!()
    }

    fn visit_lambda(&self, _l: &ast::Lambda) -> PythonType {
        todo!()
    }

    fn visit_if_exp(&self, _i: &ast::IfExp) -> PythonType {
        todo!()
    }

    fn visit_joined_str(&self, _j: &ast::JoinedStr) -> PythonType {
        todo!()
    }

    fn visit_formatted_value(&self, _f: &ast::FormattedValue) -> PythonType {
        todo!()
    }

    fn visit_alias(&self, _a: &ast::Alias) -> PythonType {
        todo!()
    }

    fn visit_assign(&self, _a: &ast::Assign) -> PythonType {
        todo!()
    }

    fn visit_ann_assign(&self, _a: &ast::AnnAssign) -> PythonType {
        todo!()
    }

    fn visit_aug_assign(&self, _a: &ast::AugAssign) -> PythonType {
        todo!()
    }

    fn visit_assert(&self, _a: &ast::Assert) -> PythonType {
        todo!()
    }

    fn visit_pass(&self, _p: &ast::Pass) -> PythonType {
        todo!()
    }

    fn visit_delete(&self, _d: &ast::Delete) -> PythonType {
        todo!()
    }

    fn visit_return(&self, _r: &ast::Return) -> PythonType {
        todo!()
    }

    fn visit_raise(&self, _r: &ast::Raise) -> PythonType {
        todo!()
    }

    fn visit_break(&self, _b: &ast::Break) -> PythonType {
        todo!()
    }

    fn visit_continue(&self, _c: &ast::Continue) -> PythonType {
        todo!()
    }

    fn visit_global(&self, _g: &ast::Global) -> PythonType {
        todo!()
    }

    fn visit_nonlocal(&self, _n: &ast::Nonlocal) -> PythonType {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use std::path::PathBuf;
    // TODO: refactor and move the test to type check mod
    fn snapshot_type_eval(source: &str) -> String {
        use crate::nodes::EnderpyFile;
        use crate::state::State;
        use enderpy_python_parser::Parser;

        let mut parser = Parser::new(source.to_string());
        let ast_module = parser.parse();

        let enderpy_file = EnderpyFile::from(
            ast_module,
            "test".to_string(),
            "".to_string(),
            PathBuf::from("test.py"),
        );

        let mut module = State::new(Box::new(enderpy_file));
        module.populate_symbol_table();
        let symbol_table = module.get_symbol_table();

        let type_eval = TypeEvaluator::new(symbol_table);

        let mut result = HashMap::new();

        for stmt in module.file.body {
            match stmt {
                parser::ast::Statement::ExpressionStatement(e) => {
                    let t = type_eval.get_type(&e);
                    match e {
                        parser::ast::Expression::Name(n) => {
                            result.insert(n.id, t.unwrap_or(PythonType::Unknown).to_string());
                        }
                        _ => panic!("don't use this test for other expressions"),
                    }
                }
                parser::ast::Statement::AssignStatement(a) => {
                    let t = type_eval.get_type(&a.value);
                    match a.targets.first() {
                        Some(target) => match target {
                            parser::ast::Expression::Name(n) => {
                                result.insert(n.id.clone(), t.unwrap_or(PythonType::Unknown).to_string());
                            }
                            _ => panic!("don't use this test for other expressions"),
                        },
                        None => panic!("don't use this test for other expressions"),
                    }
                }
                _ => {}
            }
        }

        // sort result by key
        let mut result_sorted = result.clone().into_iter().collect::<Vec<_>>();
        result_sorted.sort_by(|a, b| a.0.cmp(&b.0));

        format!("{:#?}", result_sorted)
    }

    macro_rules! snap_type_eval {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let result = snapshot_type_eval(contents);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.set_description(contents);
                settings.bind(|| {
                    insta::assert_snapshot!(result);
                });
            }
        };
    }

    snap_type_eval!(test_type_eval_vars, "./testdata/inputs/type_eval_vars.py");
}
