use parser::ast;

use crate::{
    ast_visitor_generic::TraversalVisitorImmutGeneric,
    symbol_table::{Declaration, SymbolTable, SymbolTableNode},
};

use super::{
    builtins, type_inference,
    types::{CallableType, Type},
};

pub struct TypeEvaluator {
    // TODO: make this a reference to the symbol table in the checker
    pub symbol_table: SymbolTable,
}

impl TypeEvaluator {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self { symbol_table }
    }

    pub fn get_symbol_node_type(&self, symbol: &SymbolTableNode, position: usize) -> Type {
        match symbol.declaration_until_position(position) {
            Some(declaration) => self.get_type_from_declaration(declaration),
            None => Type::Unknown,
        }
    }

    pub fn get_type(&self, expr: &ast::Expression) -> Type {
        match expr {
            ast::Expression::Constant(c) => match c.value {
                ast::ConstantValue::Int(_) => Type::Int,
                ast::ConstantValue::Float(_) => Type::Float,
                ast::ConstantValue::Str(_) => Type::Str,
                ast::ConstantValue::Bool(_) => Type::Bool,
                ast::ConstantValue::None => Type::None,
                _ => Type::Unknown,
            },
            ast::Expression::Name(n) => self.infer_type_from_symbol_table(&n.id, n.node.start),
            ast::Expression::Call(call) => {
                let func = *call.func.clone();
                match func {
                    ast::Expression::Name(n) => {
                        let f_type = self.infer_type_from_symbol_table(n.id.as_str(), n.node.start);
                        // we know this must be a callable type otherwise raise error cannot call
                        match f_type {
                            Type::Callable(callable_type) => callable_type.return_type,
                            _ => Type::Unknown,
                        }
                    }
                    ast::Expression::Attribute(_a) => panic!("TODO: infer type from attribute"),
                    _ => panic!("TODO: infer type from call"),
                }
            }
            ast::Expression::BinOp(b) => type_inference::bin_op_result_type(
                &self.get_type(&b.left),
                &self.get_type(&b.right),
                &b.op,
            ),
            ast::Expression::List(l) => {
                let mut prev_elm_type = Type::Unknown;
                for elm in &l.elements {
                    let elm_type = self.get_type(elm);
                    if prev_elm_type == Type::Unknown {
                        prev_elm_type = elm_type;
                    } else if prev_elm_type != elm_type {
                        prev_elm_type = Type::Unknown;
                        break;
                    }
                }
                let final_elm_type = prev_elm_type;
                Type::Class(super::types::ClassType {
                    name: builtins::LIST_TYPE.to_string(),
                    args: vec![final_elm_type],
                })
            }
            ast::Expression::Tuple(_) => todo!(),
            ast::Expression::Dict(_) => todo!(),
            ast::Expression::Set(_) => todo!(),
            ast::Expression::BoolOp(_) => todo!(),
            ast::Expression::UnaryOp(_) => todo!(),
            ast::Expression::NamedExpr(_) => todo!(),
            ast::Expression::Yield(_) => todo!(),
            ast::Expression::YieldFrom(_) => todo!(),
            ast::Expression::Starred(_) => todo!(),
            ast::Expression::Generator(_) => todo!(),
            ast::Expression::ListComp(_) => todo!(),
            ast::Expression::SetComp(_) => todo!(),
            ast::Expression::DictComp(_) => todo!(),
            ast::Expression::Attribute(_) => todo!(),
            ast::Expression::Subscript(s) => {
                let value_type = &self.get_type(&s.value);
                // if the type of value is subscriptable, then return the type of the subscript

                // the type is subscriptable if it is a list, tuple, dict, or set
                match value_type {
                    Type::Class(class_type) => {
                        if let Some(args) = class_type.args.last() {
                            args.clone()
                        } else {
                            Type::Unknown
                        }
                    }
                    _ => Type::Unknown,
                }
            }
            ast::Expression::Slice(_) => todo!(),
            ast::Expression::Await(_) => todo!(),
            ast::Expression::Compare(_) => todo!(),
            ast::Expression::Lambda(_) => todo!(),
            ast::Expression::IfExp(_) => todo!(),
            ast::Expression::JoinedStr(_) => todo!(),
            ast::Expression::FormattedValue(_) => todo!(),
        }
    }

    fn get_type_from_declaration(&self, declaration: &Declaration) -> Type {
        match declaration {
            Declaration::Variable(v) => {
                if let Some(type_annotation) = &v.type_annotation {
                    type_inference::get_type_from_annotation(type_annotation)
                } else if let Some(source) = &v.inferred_type_source {
                    self.get_type(source)
                } else {
                    Type::Unknown
                }
            }
            Declaration::Function(f) => {
                let return_type = if let Some(type_annotation) = f.function_node.returns.clone() {
                    type_inference::get_type_from_annotation(&type_annotation)
                } else {
                    panic!("Infer the type based on the return statement")
                };

                let arguments = f.function_node.args.clone();
                let name = f.function_node.name.clone();

                Type::Callable(Box::new(CallableType {
                    name,
                    arguments,
                    return_type,
                }))
            }
            _ => panic!("TODO: infer type from declaration"),
        }
    }

    fn infer_type_from_symbol_table(&self, name: &str, position: usize) -> Type {
        match self.symbol_table.lookup_in_scope(name) {
            Some(symbol) => self.get_symbol_node_type(symbol, position),
            None => Type::Unknown,
        }
    }
}

impl TraversalVisitorImmutGeneric<Type> for TypeEvaluator {
    fn visit_stmt(&self, s: &ast::Statement) -> Type {
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
        }
    }

    fn visit_expr(&self, e: &ast::Expression) -> Type {
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

    fn visit_import(&self, _i: &ast::Import) -> Type {
        todo!();
    }

    fn visit_import_from(&self, _i: &ast::ImportFrom) -> Type {
        todo!();
    }

    fn visit_if(&self, i: &parser::ast::If) -> Type {
        todo!();
    }

    fn visit_while(&self, w: &parser::ast::While) -> Type {
        todo!();
    }

    fn visit_for(&self, f: &parser::ast::For) -> Type {
        todo!();
    }

    fn visit_with(&self, w: &parser::ast::With) -> Type {
        todo!();
    }

    fn visit_try(&self, t: &parser::ast::Try) -> Type {
        todo!();
    }

    fn visit_try_star(&self, t: &parser::ast::TryStar) -> Type {
        todo!();
    }

    fn visit_function_def(&self, f: &parser::ast::FunctionDef) -> Type {
        todo!();
    }

    fn visit_class_def(&self, c: &parser::ast::ClassDef) -> Type {
        todo!();
    }

    fn visit_match(&self, m: &parser::ast::Match) -> Type {
        todo!();
    }

    fn visit_constant(&self, _c: &ast::Constant) -> Type {
        todo!();
    }

    fn visit_list(&self, _l: &ast::List) -> Type {
        todo!()
    }

    fn visit_tuple(&self, _t: &ast::Tuple) -> Type {
        todo!()
    }

    fn visit_dict(&self, _d: &ast::Dict) -> Type {
        todo!()
    }

    fn visit_set(&self, _s: &ast::Set) -> Type {
        todo!()
    }

    fn visit_name(&self, _n: &ast::Name) -> Type {
        todo!()
    }

    fn visit_bool_op(&self, _b: &ast::BoolOperation) -> Type {
        todo!()
    }

    fn visit_unary_op(&self, _u: &ast::UnaryOperation) -> Type {
        todo!()
    }

    fn visit_bin_op(&self, _b: &ast::BinOp) -> Type {
        todo!()
    }

    fn visit_named_expr(&self, _n: &ast::NamedExpression) -> Type {
        todo!()
    }

    fn visit_yield(&self, _y: &ast::Yield) -> Type {
        todo!()
    }

    fn visit_yield_from(&self, _y: &ast::YieldFrom) -> Type {
        todo!()
    }

    fn visit_starred(&self, _s: &ast::Starred) -> Type {
        todo!()
    }

    fn visit_generator(&self, _g: &ast::Generator) -> Type {
        todo!()
    }

    fn visit_list_comp(&self, _l: &ast::ListComp) -> Type {
        todo!()
    }

    fn visit_set_comp(&self, _s: &ast::SetComp) -> Type {
        todo!()
    }

    fn visit_dict_comp(&self, _d: &ast::DictComp) -> Type {
        todo!()
    }

    fn visit_attribute(&self, _a: &ast::Attribute) -> Type {
        todo!()
    }

    fn visit_subscript(&self, _s: &ast::Subscript) -> Type {
        todo!()
    }

    fn visit_slice(&self, _s: &ast::Slice) -> Type {
        todo!()
    }

    fn visit_call(&self, _c: &ast::Call) -> Type {
        todo!()
    }

    fn visit_await(&self, _a: &ast::Await) -> Type {
        todo!()
    }

    fn visit_compare(&self, _c: &ast::Compare) -> Type {
        todo!()
    }

    fn visit_lambda(&self, _l: &ast::Lambda) -> Type {
        todo!()
    }

    fn visit_if_exp(&self, _i: &ast::IfExp) -> Type {
        todo!()
    }

    fn visit_joined_str(&self, _j: &ast::JoinedStr) -> Type {
        todo!()
    }

    fn visit_formatted_value(&self, _f: &ast::FormattedValue) -> Type {
        todo!()
    }

    fn visit_alias(&self, _a: &ast::Alias) -> Type {
        todo!()
    }

    fn visit_assign(&self, _a: &ast::Assign) -> Type {
        todo!()
    }

    fn visit_ann_assign(&self, _a: &ast::AnnAssign) -> Type {
        todo!()
    }

    fn visit_aug_assign(&self, _a: &ast::AugAssign) -> Type {
        todo!()
    }

    fn visit_assert(&self, _a: &ast::Assert) -> Type {
        todo!()
    }

    fn visit_pass(&self, _p: &ast::Pass) -> Type {
        todo!()
    }

    fn visit_delete(&self, _d: &ast::Delete) -> Type {
        todo!()
    }

    fn visit_return(&self, _r: &ast::Return) -> Type {
        todo!()
    }

    fn visit_raise(&self, _r: &ast::Raise) -> Type {
        todo!()
    }

    fn visit_break(&self, _b: &ast::Break) -> Type {
        todo!()
    }

    fn visit_continue(&self, _c: &ast::Continue) -> Type {
        todo!()
    }

    fn visit_global(&self, _g: &ast::Global) -> Type {
        todo!()
    }

    fn visit_nonlocal(&self, _n: &ast::Nonlocal) -> Type {
        todo!()
    }
}
