#![allow(dead_code)]
#![allow(unused_variables)]

use core::panic;
use std::path::PathBuf;

use dashmap::DashMap;
use enderpy_python_parser as parser;
use enderpy_python_parser::ast;

use log::debug;
use miette::{bail, Result};
use parser::ast::Expression;

use super::{
    builtins,
    types::{CallableType, LiteralValue, PythonType},
};
use crate::{
    semantic_analyzer::get_member_access_info,
    symbol_table::{Class, Declaration, Id, LookupSymbolRequest, SymbolTable, SymbolTableNode},
    types::{ClassType, ModuleRef, TypeVar},
};

const LITERAL_TYPE_PARAMETER_MSG: &str = "Type arguments for 'Literal' must be None, a literal value (int, bool, str, or bytes), or an enum value";
// TODO: this is not the right message there are other types like Dict that are
// allowed as parameters
const UNION_TYPE_PARAMETER_MSG: &str = "Type arguments for 'Union' must be names or literal values";

const SPECIAL_FORM: &str = "_SpecialForm";
#[derive(Clone, Debug)]
pub struct TypeEvaluator<'a> {
    // TODO: make this a reference to the symbol table in the checker
    pub symbol_table: SymbolTable,
    pub imported_symbol_tables: &'a DashMap<Id, SymbolTable>,
    pub ids: &'a DashMap<PathBuf, Id>,
}

/// Struct for evaluating the type of an expression
impl<'a> TypeEvaluator<'a> {
    pub fn new(
        symbol_table: SymbolTable,
        imported_symbol_tables: &'a DashMap<Id, SymbolTable>,
        ids: &'a DashMap<PathBuf, Id>,
    ) -> Self {
        TypeEvaluator {
            symbol_table,
            imported_symbol_tables,
            ids,
        }
    }
    /// Entry point function to get type of an expression. The expression passed
    /// to this function must not be annotations, for example if you want to
    /// get the type of a variable declaration you should pass the value of
    /// the declaration to this function. To get the type of an annotation
    /// expression use get_type_from_annotation
    pub fn get_type(
        &self,
        expr: &ast::Expression,
        symbol_table: Option<&SymbolTable>,
        symbol_table_scope: Option<u32>,
    ) -> Result<PythonType> {
        log::debug!(
            "Getting type for expression: {:?} in symbol table: {:?}",
            expr,
            symbol_table.map(|s| s.file_path.clone())
        );
        let msg = format!("{expr:?}");
        let symbol_table = match symbol_table {
            Some(s) => s,
            None => &self.symbol_table,
        };
        let r = match expr {
            ast::Expression::Constant(c) => {
                let typ = match &c.value {
                    // Constants are not literals unless they are explicitly
                    // typing.readthedocs.io/en/latest/spec/literal.html#backwards-compatibility
                    ast::ConstantValue::Int(_) => self.get_builtin_type("int"),
                    ast::ConstantValue::Float(_) => self.get_builtin_type("float"),
                    ast::ConstantValue::Str(_) => self.get_builtin_type("str"),
                    ast::ConstantValue::Bool(_) => self.get_builtin_type("bool"),
                    ast::ConstantValue::None => Some(PythonType::None),
                    ast::ConstantValue::Bytes(_) => self.get_builtin_type("bytes"),
                    ast::ConstantValue::Ellipsis => Some(PythonType::Any),
                    // TODO: implement
                    ast::ConstantValue::Tuple(_) => Some(PythonType::Unknown),
                    ast::ConstantValue::Complex { real, imaginary } => Some(PythonType::Unknown),
                };
                Ok(match typ {
                    Some(t) => t,
                    None => bail!("Unknown constant type"),
                })
            }
            ast::Expression::Name(n) => {
                if let Some(t) = self.get_builtin_type(&n.id) {
                    return Ok(t);
                }
                self.get_name_type(&n.id, Some(n.node.start), symbol_table, symbol_table_scope)
            }
            ast::Expression::Call(call) => {
                let called_function = &call.func;
                match called_function {
                    ast::Expression::Subscript(subscript) => {
                        todo!("initialized a class with type parameter type")
                    }
                    _ => {
                        let f_type = self.get_type(called_function, Some(symbol_table), None)?;
                        if let PythonType::Callable(c) = &f_type {
                            let return_type = self.get_return_type_of_callable(c, &call.args);
                            Ok(return_type)
                        } else if let PythonType::Class(c) = &f_type {
                            Ok(f_type)
                        } else if let PythonType::TypeVar(t) = &f_type {
                            let Some(first_arg) = call.args.first() else {
                                bail!("TypeVar must be called with a name");
                            };
                            let type_name = match first_arg {
                                ast::Expression::Constant(str) => match &str.value {
                                    ast::ConstantValue::Str(s) => s,
                                    _ => panic!("TypeVar first arg must be a string"),
                                },
                                _ => panic!("TypeVar must be called with at least one arg"),
                            };

                            let bounds: Vec<PythonType> = call
                                .args
                                .iter()
                                .skip(1)
                                .map(|arg| {
                                    self.get_type(arg, None, None)
                                        .unwrap_or(PythonType::Unknown)
                                })
                                .collect();

                            // Disallow specifying a single bound
                            if bounds.len() == 1 {
                                bail!("TypeVar must be called with at least two bounds");
                            }

                            // Disallow specifying a type var as a bound
                            if bounds.iter().any(|b| matches!(b, PythonType::TypeVar(_))) {
                                bail!("TypeVar cannot be used as a bound");
                            }

                            Ok(PythonType::TypeVar(TypeVar {
                                name: type_name.to_string(),
                                bounds,
                            }))
                        } else {
                            bail!("{f_type:?} is not callable");
                        }
                    }
                }
            }
            ast::Expression::List(l) => {
                let final_elm_type = self.get_sequence_type_from_elements(&l.elements);
                let class_type = self
                    .get_builtin_type(builtins::LIST_TYPE)
                    .expect("builtin list type not found");
                let Some(c) = class_type.as_class() else {
                    panic!("List type is not a class");
                };
                Ok(PythonType::Class(ClassType::new(
                    c.details.clone(),
                    vec![final_elm_type],
                    true,
                )))
            }
            ast::Expression::Tuple(t) => {
                let elm_type = self.get_sequence_type_from_elements(&t.elements);

                let class_type = self
                    .get_builtin_type(builtins::TUPLE_TYPE)
                    .expect("builtin tuple type not found");
                let Some(c) = class_type.as_class() else {
                    panic!("Tuple type is not a class");
                };
                Ok(PythonType::Class(ClassType::new(
                    c.details.clone(),
                    vec![elm_type],
                    true,
                )))
            }
            ast::Expression::Dict(d) => {
                let key_type = self.get_sequence_type_from_elements(&d.keys);
                let value_type = self.get_sequence_type_from_elements(&d.values);
                let class_type = self
                    .get_builtin_type(builtins::DICT_TYPE)
                    .expect("builtin dict type not found");
                let Some(c) = class_type.as_class() else {
                    panic!("Dict type is not a class but is {class_type:?}");
                };
                Ok(PythonType::Class(ClassType::new(
                    c.details.clone(),
                    vec![key_type, value_type],
                    true,
                )))
            }
            ast::Expression::Set(s) => {
                let elm_type = self.get_sequence_type_from_elements(&s.elements);
                let class_type = match self.get_builtin_type(builtins::SET_TYPE) {
                    Some(builtin_type) => match builtin_type {
                        PythonType::Class(c) => c.details,
                        _ => panic!("Dict type is not a class"),
                    },
                    _ => return Ok(PythonType::Unknown),
                };
                Ok(PythonType::Class(ClassType::new(
                    class_type,
                    vec![elm_type],
                    true,
                )))
            }
            ast::Expression::BoolOp(_) => Ok(self.get_builtin_type("bool").expect("typeshed")),
            ast::Expression::UnaryOp(u) => match u.op {
                ast::UnaryOperator::Not => Ok(self.get_builtin_type("bool").expect("typeshed")),
                ast::UnaryOperator::UAdd
                | ast::UnaryOperator::USub
                | ast::UnaryOperator::Invert => Ok(PythonType::Unknown),
            },
            ast::Expression::NamedExpr(e) => self.get_type(&e.value, None, None),
            ast::Expression::Yield(a) => {
                let yield_type = match a.value {
                    Some(ref v) => self.get_type(v, None, None)?,
                    None => PythonType::None,
                };
                let builtin_type = self.get_builtin_type(builtins::ITER_TYPE);
                todo!()
            }
            ast::Expression::YieldFrom(yf) => {
                let yield_type = match &yf.value {
                    ast::Expression::List(ref l) => {
                        self.get_sequence_type_from_elements(&l.elements)
                    }
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
                //         _ => panic!("comprehension target must be a name, or does it?"),
                //     }
                // }

                Ok(PythonType::Unknown)
            }
            ast::Expression::ListComp(_) => Ok(PythonType::Unknown),
            ast::Expression::SetComp(_) => Ok(PythonType::Unknown),
            ast::Expression::DictComp(_) => Ok(PythonType::Unknown),
            /*
            When attribute is accessed there are multilple cases:

            1. Accessing an attribute of a class inside the class through self
                ```
                class A:
                    def __init__(self):
                        self.a = 1
                    def get_a(self):
                        return self.a

                    # or a class method
                    @class_method
                    def class_method(cls):
                        return cls.a
                ```

            2. Accessing an attribute of a class outside the class
                ```
                class A:
                    def __init__(self):
                        self.a = 1
                a = A()
                print(a.a)
                ```
            */
            ast::Expression::Attribute(a) => {
                // TODO: sys is recursive import and not implemented
                if a.value.as_name().is_some_and(|n| n.id == "sys") {
                    return Ok(PythonType::Unknown);
                }
                // Case 1
                if get_member_access_info(symbol_table, &a.value).is_some() {
                    let enclosing_parent_class = symbol_table.get_enclosing_class_scope();
                    if let Some(enclosing_parent_class) = enclosing_parent_class {
                        let symbol_table_node =
                            symbol_table.lookup_attribute(&a.attr, enclosing_parent_class.id);
                        let res = match symbol_table_node {
                            Some(node) => self.get_symbol_type(node),
                            None => panic!("cannot find symbol table node for attribute access"),
                        };

                        return res;
                    }
                }

                // Case 2
                // First find the type of the attribute and then find the value in the scope of the attribute

                let value_type = match self.get_type(&a.value, None, None) {
                    Ok(t) => t,
                    Err(e) => {
                        return Ok(PythonType::Unknown);
                    }
                };
                match value_type {
                    PythonType::Class(c) => Ok(self
                        .lookup_on_class(symbol_table, &c, &a.attr)
                        .expect("attribute not found on type")),
                    PythonType::Module(module) => {
                        log::debug!("module: {:?}", module);
                        let module_sym_table =
                            self.imported_symbol_tables.get(&module.module_id).unwrap();
                        self.get_name_type(&a.attr, None, &module_sym_table, Some(0))
                    }
                    _ => Ok(PythonType::Unknown),
                }
            }
            ast::Expression::BinOp(b) => Ok(self.bin_op_result_type(
                &self.get_type(&b.left, None, None)?,
                &self.get_type(&b.right, None, None)?,
                &b.op,
            )),
            ast::Expression::Subscript(s) => {
                let value_type = self.get_type(&s.value, Some(symbol_table), None)?;

                let typ = match value_type {
                    PythonType::Class(ref c) => {
                        if !c.is_instance {
                            return Ok(value_type);
                        }
                        let lookup_on_class = self.lookup_on_class(symbol_table, c, "__getitem__");
                        match lookup_on_class {
                            Some(PythonType::Callable(callable)) => {
                                let ret_type = self.get_return_type_of_callable(&callable, &[]);
                                match ret_type {
                                    PythonType::TypeVar(ref tv) => {
                                        let type_var_type = c.type_parameters.first().unwrap();
                                        return Ok(type_var_type.clone());
                                    }
                                    _ => return Ok(ret_type),
                                }
                            }
                            _ => PythonType::Unknown,
                        }
                    }
                    _ => PythonType::Unknown,
                };

                if typ == PythonType::Unknown {
                    return Ok(value_type);
                }
                return Ok(typ);
            }
            ast::Expression::Slice(_) => Ok(PythonType::Unknown),
            ast::Expression::Await(_) => Ok(PythonType::Unknown),
            ast::Expression::Compare(_) => Ok(PythonType::Unknown),
            ast::Expression::Lambda(_) => Ok(PythonType::Unknown),
            ast::Expression::IfExp(_) => Ok(PythonType::Unknown),
            ast::Expression::JoinedStr(_) => Ok(self.get_builtin_type("str").expect("typeshed")),
            ast::Expression::FormattedValue(f) => self.get_type(&f.value, None, None),
        };

        r
    }

    // This function tries to find the python type from an annotation expression
    // If the annotation is invalid it returns unknown type
    pub fn get_annotation_type(
        &self,
        type_annotation: &ast::Expression,
        symbol_table: &SymbolTable,
        scope_id: Option<u32>,
    ) -> PythonType {
        log::debug!("Getting type from annotation: {:?}", type_annotation);
        let expr_type = match type_annotation {
            Expression::Name(name) => {
                if builtins::ALL_BUILTINS.contains(&name.id.as_str()) {
                    return self.get_builtin_type(&name.id).expect("typeshed");
                };
                let typ =
                    self.get_name_type(&name.id, Some(name.node.start), symbol_table, scope_id);
                match typ {
                    Ok(t) => t,
                    Err(e) => {
                        log::debug!("error getting type from annotation: {:?}", e);
                        PythonType::Unknown
                    }
                }
            }
            Expression::Constant(c) => {
                if let ast::ConstantValue::None = c.value {
                    PythonType::None
                // Illegal type annotation should report an error
                } else {
                    PythonType::Unknown
                }
            }
            Expression::Subscript(s) => {
                // This is a generic type
                let typ = self.get_type(&s.value, Some(&self.symbol_table), None);
                match typ {
                    Ok(typ) => {
                        let Some(class_type) = typ.as_class() else {
                            return PythonType::Unknown;
                        };
                        if class_type.details.special {
                            return match class_type.details.name.as_str() {
                                "Literal" => self.handle_literal_type(s),
                                "Union" => {
                                    // try to convert subscript value into tuple and send the tuple
                                    // items as parameters to union type
                                    let union_parameters = match &s.slice {
                                        Expression::Tuple(t) => &t.elements,
                                        _ => todo!(),
                                    };
                                    self.handle_union_type(union_parameters.to_vec())
                                }
                                "Optional" => {
                                    let inner_value =
                                        self.get_annotation_type(&s.slice, symbol_table, scope_id);
                                    PythonType::Optional(Box::new(inner_value))
                                }
                                _ => PythonType::Any,
                            };
                        }
                        let type_parameters =
                            vec![self.get_annotation_type(&s.slice, symbol_table, None)];
                        PythonType::Class(ClassType::new(
                            class_type.details.clone(),
                            type_parameters,
                            true,
                        ))
                    }
                    // Illegal type annotation? Trying to subscript a non class type
                    Err(e) => PythonType::Unknown,
                }
            }
            Expression::BinOp(b) => {
                match b.op {
                    // Union type
                    ast::BinaryOperator::BitOr => {
                        // flatten the bit or expression if the left and right are also bit or
                        let union_parameters = self.flatten_bit_or(b);
                        self.handle_union_type(union_parameters)
                    }
                    // TODO: check if other binary operators are allowed
                    _ => todo!(),
                }
            }
            _ => PythonType::Unknown,
        };

        expr_type
    }

    /// Get the python type of a name from the symbol table
    pub fn get_name_type(
        &self,
        name: &str,
        position: Option<u32>,
        symbol_table: &SymbolTable,
        scope_id: Option<u32>,
    ) -> Result<PythonType> {
        let lookup_request = LookupSymbolRequest {
            name,
            scope: scope_id,
        };
        log::debug!(
            "infer_type_from_symbol_table: symbol: {:?} symbol_table: {:?}",
            name,
            symbol_table.file_path,
        );

        let find_in_current_symbol_table = symbol_table.lookup_in_scope(&lookup_request);
        if let Some(f) = find_in_current_symbol_table {
            return self.get_symbol_type(f);
        };

        log::debug!(
            "did not find symbol {} in symbol table, checking star imports",
            name
        );
        // Check if there's any import * and try to find the symbol in those files
        for star_import in symbol_table.star_imports.iter() {
            log::debug!("checking star imports {:?}", star_import);
            for id in star_import.resolved_ids.iter() {
                let star_import_sym_table = self.imported_symbol_tables.get(id);
                let Some(sym_table) = star_import_sym_table else {
                    panic!("symbol table of star import not found at {:?}", id);
                };
                let res = sym_table.lookup_in_scope(&lookup_request);
                match res {
                    Some(res) => {
                        return self.get_symbol_type(res);
                    }
                    None => continue,
                };
            }
        }

        bail!("name {name} is not defined")
    }

    /// Get the type of a symbol node based on declarations
    fn get_symbol_type(&self, symbol: &SymbolTableNode) -> Result<PythonType> {
        log::debug!("get_symbol_node_type: {symbol:}");
        let decl = symbol.last_declaration();
        let decl_scope = decl.declaration_path().scope_id;
        let symbol_table = &self
            .imported_symbol_tables
            .get(&decl.declaration_path().symbol_table_id)
            .unwrap();
        let result = match decl {
            Declaration::Variable(v) => {
                if let Some(type_annotation) = &v.type_annotation {
                    let var_type =
                        self.get_annotation_type(type_annotation, symbol_table, Some(decl_scope));

                    if type_annotation
                        .as_name()
                        .is_some_and(|name| name.id == SPECIAL_FORM)
                    {
                        let class_symbol = Class::new_special(
                            symbol.name.clone(),
                            v.declaration_path.clone(),
                            decl_scope,
                        );
                        Ok(PythonType::Class(ClassType::new(
                            class_symbol,
                            vec![],
                            false,
                        )))
                    } else {
                        Ok(var_type)
                    }
                } else if let Some(source) = &v.inferred_type_source {
                    // TODO: Hacky way to resolve Dict, List, ... to something other than
                    // _Alias which is in the typeshed repo
                    let builtin_type = if source.as_call().is_some_and(|f| {
                        f.func.as_name().is_some_and(|n| n.id.as_str() == "_Alias")
                    }) {
                        let val = self.get_builtin_type(&symbol.name.to_lowercase());
                        debug!(
                            "found _alias {:?} resolved to {:?}",
                            &symbol.name.to_lowercase(),
                            val
                        );
                        val
                    } else {
                        None
                    };
                    if let Some(b_type) = builtin_type {
                        Ok(b_type)
                    } else {
                        self.get_type(source, Some(symbol_table), Some(decl_scope))
                    }
                } else {
                    Ok(PythonType::Unknown)
                }
            }
            Declaration::Function(f) => {
                let annotated_return_type =
                    if let Some(ref type_annotation) = f.function_node.returns {
                        self.get_annotation_type(type_annotation, symbol_table, Some(decl_scope))
                    } else {
                        // TODO: infer return type of function disabled because of recursive types
                        // self.infer_function_return_type(f)
                        PythonType::Any
                    };

                let arguments = f.function_node.args.clone();
                let name = f.function_node.name.clone();

                Ok(PythonType::Callable(Box::new(CallableType {
                    name,
                    arguments,
                    return_type: annotated_return_type,
                })))
            }
            Declaration::AsyncFunction(f) => {
                let annotated_return_type =
                    if let Some(ref type_annotation) = f.function_node.returns {
                        self.get_annotation_type(type_annotation, symbol_table, Some(decl_scope))
                    } else {
                        // TODO: infer return type of function disabled because of recursive types
                        // self.infer_function_return_type(f)
                        PythonType::Any
                    };

                let arguments = f.function_node.args.clone();
                let name = f.function_node.name.clone();

                Ok(PythonType::Callable(Box::new(CallableType {
                    name,
                    arguments,
                    return_type: annotated_return_type,
                })))
            }
            Declaration::Parameter(p) => {
                if let Some(type_annotation) = &p.type_annotation {
                    Ok(self.get_annotation_type(type_annotation, symbol_table, Some(decl_scope)))
                } else {
                    if p.is_first
                        && (p.parameter_node.arg == "self" || p.parameter_node.arg == "cls")
                    {
                        let class_scope = symbol_table
                            .get_enclosing_class_scope_of_scope(decl_scope)
                            .map(|s| symbol_table.get_scope_by_id(s.0).expect("not found"))
                            .expect("method must be in a class");
                        let parent_scope =
                            symbol_table.parent_scope(class_scope).expect("no parent");
                        let class_def = symbol_table
                            .lookup_in_scope(&LookupSymbolRequest {
                                name: &class_scope.name,
                                scope: Some(parent_scope.id),
                            })
                            .expect("class def not found");
                        return self.get_symbol_type(class_def);
                    }
                    Ok(PythonType::Unknown)
                }
            }
            Declaration::Alias(a) => {
                log::debug!("evaluating alias {:?}", a);
                // when symbol is an alias that is named to that symbol return Module type
                // e.g. from . import path as _path
                // then type of _path is Module(path)
                match &a.symbol_name {
                    Some(name) => {
                        log::debug!("finding alias with name {name:?}");
                        let import_result =
                            a.import_result.clone().expect("import result not found");
                        log::debug!("import result {:?}", import_result);
                        for id in import_result.resolved_ids.iter() {
                            log::debug!("checking path {:?}", id);
                            let Some(symbol_table_with_alias_def) =
                                self.imported_symbol_tables.get(id)
                            else {
                                panic!(
                                    " symbol table id {:?} with not found in import {:?}",
                                    id, import_result
                                );
                            };

                            if let Some(symbol_table_file_name) =
                                symbol_table_with_alias_def.file_path.file_stem()
                            {
                                if symbol_table_file_name
                                    .to_str()
                                    .is_some_and(|s| s == name.as_str())
                                {
                                    return Ok(PythonType::Module(ModuleRef {
                                        module_id: symbol_table_with_alias_def.id,
                                    }));
                                }
                            };

                            // sys/__init__.pyi imports sys itself don't know why
                            // If the resolved path is same as current symbol file path
                            // then it's cyclic and do not resolve
                            if symbol_table.id == *id {
                                log::debug!(
                                    "alias resolution skipped the import {:?}",
                                    import_result
                                );
                                continue;
                            }

                            let lookup = &LookupSymbolRequest { name, scope: None };
                            if let Some(current_symbol_lookup) =
                                symbol_table_with_alias_def.lookup_in_scope(lookup)
                            {
                                log::debug!("alias resolved to {:?}", current_symbol_lookup);
                                return self.get_symbol_type(current_symbol_lookup);
                            };

                            for star_import in symbol_table_with_alias_def.star_imports.iter() {
                                log::debug!("checking star imports {:?}", star_import);
                                for id in star_import.resolved_ids.iter() {
                                    log::debug!("checking path {:?}", id);
                                    let star_import_sym_table = self.imported_symbol_tables.get(id);
                                    let Some(sym_table) = star_import_sym_table else {
                                        panic!("symbol table of star import not found at {:?}", id);
                                    };
                                    let res = sym_table.lookup_in_scope(lookup);
                                    match res {
                                        Some(res) => {
                                            return self.get_symbol_type(res);
                                        }
                                        None => continue,
                                    };
                                }
                            }
                        }

                        Ok(PythonType::Unknown)
                    }
                    None => {
                        let Some(ref resolved_import) = a.import_result else {
                            return Ok(PythonType::Unknown);
                        };

                        let module_id = resolved_import.resolved_ids.first().unwrap();
                        return Ok(PythonType::Module(ModuleRef {
                            module_id: *module_id,
                        }));
                    }
                }
            }
            Declaration::TypeParameter(_) => Ok(PythonType::Unknown),
            Declaration::TypeAlias(_) => Ok(PythonType::Unknown),
            Declaration::Class(c) => self.get_class_declaration_type(c, symbol_table, decl_scope),
        };

        match result {
            Ok(ref t) => {
                log::debug!("evaluated type based on symbol: {} => {}", symbol, t);
            }
            Err(ref e) => {
                log::debug!("error evaluating type based on declaration: {}", e);
            }
        };
        result
    }

    fn get_class_declaration_type(
        &self,
        class_symbol: &Class,
        symbol_table: &SymbolTable,
        decl_scope: u32,
    ) -> Result<PythonType> {
        // TODO: typevar itself is a class but the rhs is typevar type
        if class_symbol.qual_name == "typing.TypeVar" {
            Ok(PythonType::TypeVar(TypeVar {
                name: "".to_string(),
                bounds: vec![],
            }))
        } else {
            let mut bases = vec![];
            match &class_symbol.class_node {
                Some(ref b) => {
                    for base in b.bases.iter() {
                        bases.push(base);
                    }
                }
                None => {}
            };
            // Bases can also add generic type parameters to the class
            // For example: class A(metaclass=Generic)
            match &class_symbol.class_node {
                Some(ref k) => {
                    for keyword in k.keywords.iter() {
                        bases.push(&keyword.value);
                    }
                }
                None => {}
            };

            let mut class_def_type_parameters = vec![];
            for base in bases {
                let base_type = self.get_type(base, Some(symbol_table), None);
                let Ok(PythonType::Class(c)) = base_type else {
                    continue;
                };
                let Some(possible_type_parameter) = base.as_subscript() else {
                    class_def_type_parameters.extend(c.type_parameters);
                    continue;
                };
                log::debug!("checking base: {:?} with type {c:?}", base);
                match &possible_type_parameter.slice {
                    ast::Expression::Name(type_parameter_name) => {
                        if class_symbol.name == type_parameter_name.id {
                            // TODO: if the class is type parameter itself.
                            // Then it causes a stack overflow.
                            // This only happened in stdlib str:
                            // class str(Sequence[str]):
                            continue;
                        }
                        let type_parameter = self.get_name_type(
                            &type_parameter_name.id,
                            Some(type_parameter_name.node.start),
                            symbol_table,
                            Some(decl_scope),
                        )?;
                        if class_def_type_parameters.contains(&type_parameter) {
                            continue;
                        }
                        class_def_type_parameters.push(type_parameter);
                    }
                    ast::Expression::Tuple(type_parameters) => {
                        let mut tuple_type_parameters = vec![];
                        for type_parameter in type_parameters.elements.iter() {
                            let type_parameter = self.get_type(
                                type_parameter,
                                Some(symbol_table),
                                Some(decl_scope),
                            )?;
                            if tuple_type_parameters.contains(&type_parameter) {
                                // TODO: Error type parameters must be unique
                                tuple_type_parameters = vec![PythonType::Unknown];
                                break;
                            }
                            tuple_type_parameters.push(type_parameter);
                        }
                        class_def_type_parameters.extend(tuple_type_parameters);
                    }
                    _ => {}
                };
            }

            Ok(PythonType::Class(ClassType::new(
                class_symbol.clone(),
                class_def_type_parameters,
                false,
            )))
        }
    }

    fn get_sequence_type_from_elements(&self, elements: &Vec<ast::Expression>) -> PythonType {
        let mut prev_elm_type = PythonType::Unknown;
        for elm in elements {
            let elm_type = self
                .get_type(elm, None, None)
                .unwrap_or(PythonType::Unknown);
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
        if !f.yield_statements.is_empty() {
            let mut yield_types = vec![];
            for yield_statement in &f.yield_statements {
                if let Some(value) = &yield_statement.value {
                    yield_types.push(
                        self.get_type(value, None, None)
                            .unwrap_or(PythonType::Unknown),
                    );
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
                    return_types.push(
                        self.get_type(value, None, None)
                            .unwrap_or(PythonType::Unknown),
                    );
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

    /// Retrieves a python type that is present in the builtin scope
    fn get_builtin_type(&self, name: &str) -> Option<PythonType> {
        // typeshed has a function class which is not supposed to be there.
        // https://github.com/python/typeshed/issues/2999
        // TODO: do something
        if name == "function" {
            return None;
        }
        let bulitins_symbol_table = &self
            .imported_symbol_tables
            .get(&Id(0))
            .expect("Builtins must exist");
        let builtin_symbol =
            bulitins_symbol_table.lookup_in_scope(&LookupSymbolRequest { name, scope: None })?;
        let decl = builtin_symbol.last_declaration();
        let found_declaration = match decl {
            Declaration::Class(c) => {
                let decl_scope = decl.declaration_path().scope_id;
                self.get_class_declaration_type(c, bulitins_symbol_table, decl_scope)
                    .unwrap_or_else(|_| {
                        panic!("Error getting type for builtin class: {:?}", c.class_node)
                    })
            }
            Declaration::Function(f) => {
                let arguments = f.function_node.args.clone();
                let name = f.function_node.name.clone();
                PythonType::Callable(Box::new(CallableType {
                    name,
                    arguments,
                    return_type: f.function_node.returns.clone().map_or(
                        PythonType::Unknown,
                        |type_annotation| {
                            self.get_annotation_type(&type_annotation, bulitins_symbol_table, None)
                        },
                    ),
                }))
            }
            _ => return None,
        };
        Some(found_declaration)
    }

    /// This function flattens a chain of bit or expressions
    /// For example: a | b | c | d
    /// will be flattened to [a, b, c, d]
    fn flatten_bit_or(&self, b: &ast::BinOp) -> Vec<Expression> {
        let mut union_parameters = vec![];
        let mut current_expr = b.left.clone();

        while let Expression::BinOp(ref inner_binop) = current_expr {
            if let ast::BinaryOperator::BitOr = inner_binop.op {
                union_parameters.push(inner_binop.right.clone());
                current_expr = inner_binop.left.clone();
            } else {
                union_parameters.push(current_expr.clone());
                break;
            }
        }

        union_parameters.push(current_expr.clone());

        current_expr.clone_from(&b.right);

        while let Expression::BinOp(ref inner_binop) = current_expr {
            if let ast::BinaryOperator::BitOr = inner_binop.op {
                union_parameters.push(inner_binop.right.clone());
                current_expr = inner_binop.left.clone();
            } else {
                union_parameters.push(current_expr.clone());
                break;
            }
        }

        union_parameters.push(current_expr);
        union_parameters
    }

    /// https://peps.python.org/pep-0484/#union-types
    /// expressions are the parameters of the union type
    /// in case of t1 | t2 | t3, expressions are [t1, t2, t3]
    /// and in case of Union[t1, t2, t3], expressions are [t1, t2, t3]
    fn handle_union_type(&self, expressions: Vec<Expression>) -> PythonType {
        let mut types = vec![];
        for expr in expressions {
            let t = self.get_annotation_type(&expr, &self.symbol_table, None);
            if self.is_valid_union_parameter(&t) {
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
        let val = match expr {
            Expression::Constant(c) => {
                match c.value.clone() {
                    ast::ConstantValue::Bool(b) => LiteralValue::Bool(b),
                    ast::ConstantValue::Int(i) => LiteralValue::Int(i),
                    ast::ConstantValue::Float(f) => LiteralValue::Float(f),
                    ast::ConstantValue::Str(s) => LiteralValue::Str(s),
                    ast::ConstantValue::Bytes(b) => LiteralValue::Bytes(b),
                    ast::ConstantValue::None => LiteralValue::None,
                    // Tuple is illegal if it has parentheses, otherwise it's allowed and the output
                    // a multiValued type Currently even mypy does not support
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
                let value = match &a.value {
                    Expression::Name(n) => &n.id,
                    _ => panic!("Literal type with attribute value can only be a name"),
                };
                LiteralValue::Str(value.to_string())
            }
            Expression::Subscript(s) => {
                match &s.value {
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

    pub fn bin_op_result_type(
        &self,
        t1: &PythonType,
        t2: &PythonType,
        op: &ast::BinaryOperator,
    ) -> PythonType {
        // Dummy
        t1.clone()
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

    // TODO: still not sure if this function should infer type of a type parameter
    fn get_return_type_of_callable(
        &self,
        f_type: &CallableType,
        args: &[Expression],
    ) -> PythonType {
        f_type.return_type.clone()
    }

    fn lookup_on_class(
        &self,
        symbol_table: &SymbolTable,
        c: &ClassType,
        method_name: &str,
    ) -> Option<PythonType> {
        let class_symbol_table_id = c.details.declaration_path.symbol_table_id;
        let class_symbol_table = self
            .imported_symbol_tables
            .get(&class_symbol_table_id)
            .unwrap();
        let class_scope = c.details.class_scope_id;
        let symbol_table_node = class_symbol_table.lookup_attribute(method_name, class_scope);

        symbol_table_node.map(|node| self.get_symbol_type(node).expect("Cannot infer type"))
    }
}
