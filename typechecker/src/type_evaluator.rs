#![allow(dead_code)]
#![allow(unused_variables)]

use core::panic;
use enderpy_python_parser::{self as parser, parser::parser::intern_lookup};
use parser::ast;
use parser::parser::parser::Parser;
use std::{
    cell::Cell,
    panic::{catch_unwind, AssertUnwindSafe},
    sync::Arc,
};
use tracing::{error, instrument, span, trace, Level};

use miette::{bail, Result};
use parser::ast::Expression;

use super::{
    builtins,
    types::{
        self, CallableType, ClassType, InstanceType, LiteralValue, ModuleRef, PythonType, TypeVar,
    },
};
use crate::{
    get_module_name,
    build::BuildManager,
    semantic_analyzer::get_member_access_info,
    symbol_table::{self, Class, Declaration, DeclarationPath, Id, SymbolTable, SymbolTableNode},
    types::CallableArgs,
};

const LITERAL_TYPE_PARAMETER_MSG: &str = "Type arguments for 'Literal' must be None, a literal value (int, bool, str, or bytes), or an enum value";
// TODO: this is not the right message there are other types like Dict that are
// allowed as parameters
const UNION_TYPE_PARAMETER_MSG: &str = "Type arguments for 'Union' must be names or literal values";

const SPECIAL_FORM: &str = "_SpecialForm";
#[derive(Clone, Debug)]
pub struct TypeEvaluator<'a> {
    build_manager: &'a BuildManager,
    flags: Cell<GetTypeFlags>,
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    // Defines how the type evaluation should be done at each moment.
    pub struct GetTypeFlags: u8 {
        /// When a annotation is in quotes then they are evaluated in deferred mode.
        /// Defer mode does two things:
        /// 1. Annotations can refer to symbols that are defined later in the program
        /// 2. Annotations are resolved in a different order. Usually symbol resolving
        ///    in python starts from the inner scope and if moves to outer scope if the
        ///    symbol is not found. But for deferred annotations This is the order:
        ///    - Start from the outer scope and move up until global
        ///    - If not found search the local scope
        ///    See more: https://peps.python.org/pep-0563/#backwards-compatibility
        ///    An example: https://github.com/python/typing/blob/main/conformance/tests/annotations_forward_refs.py#L78
        const DEFERRED = 1 << 0;
    }
}

fn class_type_to_instance_type(class_type: PythonType) -> PythonType {
    let PythonType::Class(c) = class_type else {
        return PythonType::Unknown;
    };
    PythonType::Instance(types::InstanceType::new(c.clone(), [].to_vec()))
}

/// Struct for evaluating the type of an expression
impl<'a> TypeEvaluator<'a> {
    pub fn new(build_manager: &'a BuildManager) -> Self {
        TypeEvaluator {
            build_manager,
            flags: Cell::new(GetTypeFlags::empty()),
        }
    }
    /// Entry point function to get type of an expression. The expression passed
    /// to this function must not be annotations, for example if you want to
    /// get the type of a variable declaration you should pass the value of
    /// the declaration to this function. To get the type of an annotation
    /// expression use get_type_from_annotation
    #[instrument(skip(self, symbol_table))]
    pub fn get_type(
        &self,
        expr: &ast::Expression,
        symbol_table: &SymbolTable,
        scope_id: u32,
    ) -> Result<PythonType> {
        let r = match expr {
            ast::Expression::Constant(c) => {
                let typ = match &c.value {
                    // Constants are not literals unless they are explicitly
                    // typing.readthedocs.io/en/latest/spec/literal.html#backwards-compatibility
                    ast::ConstantValue::Int => self.get_builtin_type("int").map(class_type_to_instance_type),
                    ast::ConstantValue::Float => self.get_builtin_type("float").map(class_type_to_instance_type),
                    ast::ConstantValue::Str(_) => self.get_builtin_type("str").map(class_type_to_instance_type),
                    ast::ConstantValue::Bool(_) => self.get_builtin_type("bool").map(class_type_to_instance_type),
                    ast::ConstantValue::None => Some(PythonType::None),
                    ast::ConstantValue::Bytes => self.get_builtin_type("bytes").map(class_type_to_instance_type),
                    ast::ConstantValue::Ellipsis => Some(PythonType::Any),
                    // TODO: implement
                    ast::ConstantValue::Tuple => Some(PythonType::Unknown),
                    ast::ConstantValue::Complex => Some(PythonType::Unknown),
                };
                Ok(match typ {
                    Some(t) => t,
                    None => bail!("Unknown constant type"),
                })
            }
            ast::Expression::Name(n) => {
                if n.id == "PathLike" {
                    dbg!(expr);
                }
                Ok(self.get_name_type(&n.id, Some(n.node.start), symbol_table, scope_id))
            }
            ast::Expression::Call(call) => {
                let called_function = &call.func;
                match called_function {
                    ast::Expression::Subscript(subscript) => {
                        todo!("initialized a class with type parameter type")
                    }
                    _ => {
                        let called_type = self.get_type(called_function, symbol_table, scope_id)?;
                        if let PythonType::Callable(c) = &called_type {
                            let return_type = self.get_return_type_of_callable(
                                c,
                                &call.args,
                                symbol_table,
                                scope_id,
                            );
                            Ok(return_type)
                        } else if let PythonType::Instance(i) = &called_type {
                            // This executes the __call__ method of the instance
                            let Some(PythonType::Callable(c)) = self.lookup_on_class(symbol_table, &i.class_type, "__call__") else {
                                bail!("If you call an instance, it must have a __call__ method");
                            };
                            let return_type = self.get_return_type_of_callable(
                                &c,
                                &call.args,
                                symbol_table,
                                scope_id,
                            );
                            Ok(return_type)
                        } else if let PythonType::Class(c) = &called_type {
                            // This instantiates the class
                            Ok(PythonType::Instance(types::InstanceType::new(c.clone(), [].to_vec())))
                        } else if let PythonType::TypeVar(t) = &called_type {
                            let Some(first_arg) = call.args.first() else {
                                bail!("TypeVar must be called with a name");
                            };
                            let type_name = match first_arg {
                                ast::Expression::Constant(ref str_const) => {
                                    match &str_const.value {
                                        ast::ConstantValue::Str(_) => {
                                            let decl_id = t.decl_id;
                                            let file = &self
                                                .build_manager
                                                .files
                                                .get(&symbol_table.id)
                                                .unwrap();
                                            str_const.get_value(&file.source).to_string()
                                        }
                                        _ => panic!("TypeVar first arg must be a string"),
                                    }
                                }
                                _ => panic!("TypeVar must be called with at least one arg"),
                            };

                            let bounds: Vec<PythonType> = call
                                .args
                                .iter()
                                .skip(1)
                                .map(|arg| {
                                    self.get_type(arg, symbol_table, scope_id)
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

                            if bounds
                                .iter()
                                .filter(|x| {
                                    let Some(class) = x.as_class() else {
                                        return false;
                                    };

                                    !class.specialized.is_empty()
                                })
                                .count()
                                != 0
                            {
                                trace!("type var bounds cannot contain other type vars");
                                bail!("type var bounds cannot contain other type vars");
                            }

                            Ok(PythonType::TypeVar(TypeVar {
                                name: type_name.to_string(),
                                bounds,
                                decl_id: t.decl_id,
                            }))
                        } else {
                            bail!("{called_type:?} is not callable");
                        }
                    }
                }
            }
            ast::Expression::List(l) => {
                let final_elm_type =
                    self.get_sequence_type_from_elements(&l.elements, symbol_table, scope_id);
                let class_type = self
                    .get_builtin_type(builtins::LIST_TYPE)
                    .expect("builtin list type not found");
                let Some(c) = class_type.class() else {
                    panic!("List type is not a class");
                };
                Ok(PythonType::Instance(InstanceType::new(
                    c,
                    vec![final_elm_type],
                )))
            }
            ast::Expression::Tuple(t) => {
                let elm_type =
                    self.get_sequence_type_from_elements(&t.elements, symbol_table, scope_id);

                let class_type = self
                    .get_builtin_type(builtins::TUPLE_TYPE)
                    .expect("builtin tuple type not found");
                let Some(c) = class_type.class() else {
                    panic!("Tuple type is not a class");
                };
                Ok(PythonType::Instance(InstanceType::new(c, vec![elm_type])))
            }
            ast::Expression::Dict(d) => {
                let key_type =
                    self.get_sequence_type_from_elements(&d.keys, symbol_table, scope_id);
                let value_type =
                    self.get_sequence_type_from_elements(&d.values, symbol_table, scope_id);
                let class_type = self
                    .get_builtin_type(builtins::DICT_TYPE)
                    .expect("builtin dict type not found");
                let Some(c) = class_type.class() else {
                    panic!("Dict type is not a class");
                };
                Ok(PythonType::Instance(InstanceType::new(
                    c,
                    vec![key_type, value_type],
                )))
            }
            ast::Expression::Set(s) => {
                let elm_type =
                    self.get_sequence_type_from_elements(&s.elements, symbol_table, scope_id);
                let class_type = match self.get_builtin_type(builtins::SET_TYPE) {
                    Some(builtin_type) => match builtin_type {
                        PythonType::Class(c) => c,
                        _ => panic!("Dict type is not a class"),
                    },
                    _ => return Ok(PythonType::Unknown),
                };
                Ok(PythonType::Instance(InstanceType::new(
                    class_type,
                    vec![elm_type],
                )))
            }
            ast::Expression::BoolOp(_) => Ok(self.get_builtin_type("bool").expect("typeshed")),
            ast::Expression::UnaryOp(u) => match u.op {
                ast::UnaryOperator::Not => Ok(self.get_builtin_type("bool").expect("typeshed")),
                ast::UnaryOperator::UAdd
                | ast::UnaryOperator::USub
                | ast::UnaryOperator::Invert => Ok(PythonType::Unknown),
            },
            ast::Expression::NamedExpr(e) => self.get_type(&e.value, symbol_table, scope_id),
            ast::Expression::Yield(a) => {
                let yield_type = match a.value {
                    Some(ref v) => self.get_type(v, symbol_table, scope_id)?,
                    None => PythonType::None,
                };
                let builtin_type = self.get_builtin_type(builtins::ITER_TYPE);
                todo!()
            }
            ast::Expression::YieldFrom(yf) => {
                let yield_type = match &yf.value {
                    ast::Expression::List(ref l) => {
                        self.get_sequence_type_from_elements(&l.elements, symbol_table, scope_id)
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
                // // TODO: sys is recursive import and not implemented
                // if a.value.as_name().is_some_and(|n| n.id == "sys") {
                //     return Ok(PythonType::Unknown);
                // }
                // Case 1
                // This is self or cls
                let file = &self.build_manager.files.get(&symbol_table.id).unwrap();
                if get_member_access_info(symbol_table, &a.value).is_some() {
                    let enclosing_parent_class = symbol_table.get_enclosing_class_scope();
                    if let Some(enclosing_parent_class) = enclosing_parent_class {
                        let symbol_table_node =
                            symbol_table.lookup_attribute(&a.attr, enclosing_parent_class.id);
                        let res = match symbol_table_node {
                            Some(node) => self.get_symbol_type(node, symbol_table, None),
                            None => panic!("cannot find symbol table node for attribute access"),
                        };

                        return Ok(res);
                    }
                }

                // Case 2
                // Check what is the attribute and then do a lookup on that
                let value_type = match self.get_type(&a.value, symbol_table, scope_id) {
                    Ok(t) => t,
                    Err(e) => {
                        return Ok(PythonType::Unknown);
                    }
                };
                match value_type {
                    PythonType::Class(ref c) => {
                        let attribute_on_c = self.lookup_on_class(symbol_table, c, &a.attr);
                        if let Some(attribute_on_c) = attribute_on_c {
                            Ok(attribute_on_c)
                        } else {
                            Ok(PythonType::Unknown)
                        }
                    }
                    PythonType::Module(module) => {
                        let module_sym_table = self.get_symbol_table(&module.module_id);
                        Ok(self.get_name_type(&a.attr, None, &module_sym_table, 0))
                    }
                    // Anything you perform a get attribute on should at least resolve using object
                    // builtin because everything is an object :)
                    _ => {
                        let get_builtin_type = self
                            .get_builtin_type("object")
                            .expect("object is a builtin");
                        let object_class = get_builtin_type.as_class().expect("object is a class");

                        let attribute_on_c =
                            self.lookup_on_class(symbol_table, object_class, &a.attr);
                        if let Some(attribute_on_c) = attribute_on_c {
                            Ok(attribute_on_c)
                        } else {
                            Ok(PythonType::Unknown)
                        }
                    }
                }
            }
            ast::Expression::BinOp(b) => Ok(self.bin_op_result_type(
                &self.get_type(&b.left, symbol_table, scope_id)?,
                &self.get_type(&b.right, symbol_table, scope_id)?,
                &b.op,
            )),
            ast::Expression::Subscript(s) => {
                let value_type = self.get_type(&s.value, symbol_table, scope_id)?;
                let span = span!(Level::TRACE, "Subscript", value_type = display(&value_type),);
                let _guard = span.enter();

                let typ = match value_type {
                    PythonType::Class(ref c) => {
                        // If type parameters are not set then this is setting them.
                        if c.specialized.is_empty() {
                            let mut initialized_type_parameters: Vec<PythonType> = vec![];
                            match &s.slice {
                                ast::Expression::Tuple(t) => {
                                    for e in t.elements.iter() {
                                        initialized_type_parameters.push(self.get_type(
                                            e,
                                            symbol_table,
                                            scope_id,
                                        )?)
                                    }
                                }
                                ast::Expression::Name(n) => {
                                    // TODO: str has a cyclic reference to itself in stdlib.
                                    if n.id == "str" && c.details.name == "Sequence" {
                                        initialized_type_parameters.push(PythonType::Unknown);
                                    } else {
                                        initialized_type_parameters.push(self.get_type(
                                            &s.slice,
                                            symbol_table,
                                            scope_id,
                                        )?)
                                    }
                                }
                                _ => initialized_type_parameters.push(self.get_type(
                                    &s.slice,
                                    symbol_table,
                                    scope_id,
                                )?),
                            }

                            let mut new_class = c.clone();
                            trace!("class {c} has no specialized params setting them {initialized_type_parameters:?}");
                            new_class.specialized = initialized_type_parameters;

                            PythonType::Class(new_class)
                            // If type parameters are set then this is accessing an attribute on
                            // class.
                        } else {
                            let lookup_on_class =
                                self.lookup_on_class(symbol_table, c, "__getitem__");
                            match lookup_on_class {
                                Some(PythonType::Callable(callable)) => {
                                    let ret_type = callable.return_type;

                                    match ret_type {
                                        PythonType::TypeVar(ref tv) => {
                                            // 1. Get the index of this type var in the class type
                                            //    parameters
                                            // 2. Get the value for this index in the
                                            //    initialized_type_parameters or Unknown if not initialized
                                            let Some(type_var_index) =
                                                c.type_parameters.iter().position(|tp| {
                                                    let Some(type_var) = tp.as_type_var() else {
                                                        return false;
                                                    };
                                                    type_var.name == tv.name
                                                })
                                            else {
                                                error!("did not find type var name {tv:?} in original type vars {:?}", c.type_parameters);
                                                return Ok(PythonType::Unknown);
                                            };

                                            let type_var_value = c
                                                .specialized
                                                .get(type_var_index)
                                                .unwrap_or(&PythonType::Any);
                                            type_var_value.clone()
                                        }
                                        _ => ret_type,
                                    }
                                }
                                _ => PythonType::Unknown,
                            }
                        }
                    }
                    PythonType::Instance(ref c) => {
                        let lookup_on_class =
                            self.lookup_on_class(symbol_table, &c.class_type, "__getitem__");

                        match lookup_on_class {
                            Some(PythonType::Callable(callable)) => {
                                let ret_type = self.resolve_generics(
                                    &callable.return_type,
                                    &c.class_type.type_parameters,
                                    &c.specialized_type_parameters,
                                );
                                match ret_type {
                                    PythonType::TypeVar(ref tv) => {
                                        // 1. Get the index of this type var in the class type
                                        //    parameters
                                        // 2. Get the value for this index in the
                                        //    initialized_type_parameters or Unknown if not initialized
                                        let Some(type_var_index) =
                                            c.class_type.type_parameters.iter().position(|tp| {
                                                let Some(type_var) = tp.as_type_var() else {
                                                    return false;
                                                };
                                                type_var.name == tv.name
                                            })
                                        else {
                                            return Ok(PythonType::Unknown);
                                        };

                                        let type_var_value = c
                                            .specialized_type_parameters
                                            .get(type_var_index)
                                            .unwrap_or(&PythonType::Any);
                                        type_var_value.clone()
                                    }
                                    _ => return Ok(ret_type),
                                }
                            }
                            _ => PythonType::Unknown,
                        }
                    }
                    _ => PythonType::Unknown,
                };

                // if typ == PythonType::Unknown {
                //     return Ok(value_type);
                // }
                return Ok(typ);
            }
            ast::Expression::Slice(_) => Ok(PythonType::Unknown),
            ast::Expression::Await(a) => {
                let awaited_type = self.get_type(&a.value, symbol_table, scope_id)?;
                let typ = match awaited_type {
                    PythonType::Coroutine(callable) => callable.return_type.clone(),
                    _ => unimplemented!("Can other things be awaited?"),
                };

                Ok(typ)
            }
            ast::Expression::Compare(_) => Ok(PythonType::Unknown),
            ast::Expression::Lambda(_) => Ok(PythonType::Unknown),
            ast::Expression::IfExp(_) => Ok(PythonType::Unknown),
            ast::Expression::JoinedStr(_) => Ok(self.get_builtin_type("str").expect("typeshed")),
            ast::Expression::FormattedValue(f) => self.get_type(&f.value, symbol_table, scope_id),
        };

        tracing::debug!("get type result: {:?}", r);
        r
    }

    // This function tries to find the python type from an annotation expression
    // If the annotation is invalid it returns unknown type
    pub fn get_annotation_type(
        &self,
        type_annotation: &ast::Expression,
        symbol_table: &SymbolTable,
        scope_id: u32,
    ) -> PythonType {
        let span = span!(Level::INFO, "Get type of annotation", annotation =?type_annotation, file_path =? symbol_table.file_path);
        let _guard = span.enter();
        let expr_type = match type_annotation {
            Expression::Name(name) => {
                // TODO: Reject this type if the name refers to a variable.
                return class_type_to_instance_type(self.get_name_type(&name.id, Some(name.node.start), symbol_table, scope_id));
            }
            Expression::Constant(ref c) => match c.value {
                ast::ConstantValue::None => PythonType::None,
                // TODO: (forward_refs) Forward annotations are not
                // completely supported.
                // 1. Cyclic references not detected
                // 2. Module is preferred over local scope so we first check module scope and
                //    then local scope.
                //    https://peps.python.org/pep-0563/#backwards-compatibility
                ast::ConstantValue::Str(_) => {
                    let source = self.build_manager.files.get(&symbol_table.id).unwrap();
                    let value = c.get_value(&source.source);
                    let mut parser = Parser::new(&value);
                    // Wrap the parsing logic inside a `catch_unwind` block
                    let parse_result = catch_unwind(AssertUnwindSafe(|| parser.parse()));

                    let module = match parse_result {
                        Ok(Ok(module)) => module,
                        Ok(Err(_)) => {
                            trace!("parsing annotation failed");
                            return PythonType::Unknown;
                        }
                        Err(_) => {
                            log::error!("panic occurred during parsing");
                            return PythonType::Unknown;
                        }
                    };
                    if module.body.len() != 1 {
                        log::error!("expected 1 statement in annotation");
                        return PythonType::Unknown;
                    }
                    let stmt = module.body.first().unwrap();
                    let ast::Statement::ExpressionStatement(expr) = stmt else {
                        panic!("expected expression in annotation");
                    };
                    let flags = self.flags.get();
                    let new_flags = flags | GetTypeFlags::DEFERRED;
                    self.flags.set(new_flags);
                    let annotation_type = self.get_annotation_type(expr, symbol_table, scope_id);
                    self.flags.set(flags);
                    annotation_type
                }
                _ => self
                    .get_type(type_annotation, symbol_table, scope_id)
                    .unwrap_or(PythonType::Unknown),
            },
            Expression::Subscript(s) => {
                // This is a generic type
                let typ = self
                    .get_type(&s.value, symbol_table, scope_id)
                    .unwrap_or_else(|_| PythonType::Unknown);
                let Some(class_type) = typ.as_class() else {
                    error!("subscript value is unknown {}", typ,);
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
                // TODO: Here we need to specify any for any of the unspecialised parameters
                let mut initialized_type_parameters: Vec<PythonType> = vec![];
                match &s.slice {
                    ast::Expression::Tuple(t) => {
                        for e in t.elements.iter() {
                            initialized_type_parameters.push(
                                self.get_type(e, symbol_table, scope_id)
                                    .unwrap_or_else(|x| {
                                        error!("Cannot get type for type parameter {e:?}");
                                        PythonType::Unknown
                                    }),
                            )
                        }
                    }
                    ast::Expression::Name(n) => {
                        // TODO: str has a cyclic reference to itself in stdlib.
                        if n.id == "str" && class_type.details.name == "Sequence" {
                            initialized_type_parameters.push(PythonType::Unknown);
                        } else {
                            initialized_type_parameters.push(
                                self.get_type(&s.slice, symbol_table, scope_id)
                                    .unwrap_or_else(|x| {
                                        error!("Cannot get type for type parameter {n:?}");
                                        PythonType::Unknown
                                    }),
                            )
                        }
                    }
                    other => initialized_type_parameters.push(self.get_annotation_type(
                        other,
                        symbol_table,
                        scope_id,
                    )),
                }
                let mut new_class_type = class_type.clone();
                new_class_type.specialized = initialized_type_parameters;
                PythonType::Class(new_class_type)
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
            },
            Expression::Attribute(a) => {
                match &a.value {
                    Expression::Name(n) => {
                        let Some(typ) = self.lookup_on_module(symbol_table, scope_id, &n.id, &a.attr) else {
                            return PythonType::Unknown;
                        };
                        return class_type_to_instance_type(typ);
                    },
                    _ => todo!(),
                };
            },
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
        scope_id: u32,
    ) -> PythonType {
        let span = span!(
            Level::DEBUG,
            "Search for name",
            name = name,
            position =? position,
            symbol_table=?symbol_table.file_path,
            scope_id =? scope_id,
        );
        let _guard = span.enter();
        trace!(
            "infer_type_from_symbol_table: symbol: {:?} symbol_table: {:?}",
            name,
            symbol_table.file_path,
        );

        let find_in_current_symbol_table = symbol_table.lookup_in_scope(name, scope_id);
        if let Some(f) = find_in_current_symbol_table {
            return self.get_symbol_type(f, symbol_table, position);
        };

        trace!(
            "did not find symbol {} in symbol table, checking star imports",
            name
        );
        // Check if there's any import * and try to find the symbol in those files
        for star_import in symbol_table.star_imports.iter() {
            trace!("checking star imports {:?}", star_import);
            for id in star_import.resolved_ids.iter() {
                let star_import_sym_table = self.get_symbol_table(id);
                // In the star import we can only lookup the global scope
                let res = star_import_sym_table.lookup_in_scope(name, 0);
                match res {
                    Some(res) => {
                        return self.get_symbol_type(res, symbol_table, position);
                    }
                    None => continue,
                };
            }
        }

        if let Some(t) = self.get_builtin_type(name) {
            t
        } else {
            error!("Cannot find type for name {}", name);
            PythonType::Unknown
        }
    }

    /// Get the type of a symbol node based on declarations
    fn get_symbol_type(
        &self,
        symbol: &SymbolTableNode,
        symbol_table: &SymbolTable,
        position: Option<u32>,
    ) -> PythonType {
        // Complex thing: here we are trying to find which declaration for this symbol can be used.
        // We use combination of stuff.
        // 1. If the deferred flag is set then the symbol can be defined after it's used.
        // 2. The declaration should be before the usage. But there is an exception when we are in
        //    a pyi file.
        // 3. If no information about position is specified then the last declaration is taken.
        let decl = {
            let maybe_decl = if let Some(position) = position {
                // TODO : filter declarations to what has been defined based on the position.
                // Current problem is that only using the position is not correct because a
                // symbol might be defined in another file and comparing positions will not help.
                if self.flags.get().intersects(GetTypeFlags::DEFERRED)
                    || (symbol_table.is_pyi_file())
                {
                    Some(symbol.last_declaration())
                } else {
                    symbol.get_declaration_until_pos(position)
                }
            } else {
                Some(symbol.last_declaration())
            };

            let Some(decl) = maybe_decl else {
                error!(
                    "Cannot find declaration for symbol {:?}, pos: {:?}, flags: {:?}",
                    symbol, position, self.flags
                );
                return PythonType::Unknown;
            };
            decl
        };
        let decl_scope = decl.declaration_path().scope_id;
        let symbol_table = self.get_dec_symbol_table(decl.declaration_path());
        let result = match decl {
            Declaration::Variable(v) => {
                if let Some(type_annotation) = &v.type_annotation {
                    let var_type =
                        self.get_annotation_type(type_annotation, &symbol_table, decl_scope);

                    if type_annotation
                        .as_name()
                        .is_some_and(|name| name.id == SPECIAL_FORM)
                    {
                        let class_symbol = Class::new_special(
                            symbol.name.clone(),
                            v.declaration_path.clone(),
                            decl_scope,
                        );
                        PythonType::Class(ClassType::new(class_symbol, vec![], vec![], vec![]))
                    } else {
                        var_type
                    }
                } else if let Some(source) = &v.inferred_type_source {
                    // TODO: Hacky way to resolve Dict, List, ... to something other than
                    // _Alias which is in the typeshed repo
                    let builtin_type = if source.as_call().is_some_and(|f| {
                        f.func.as_name().is_some_and(|n| n.id.as_str() == "_Alias")
                    }) {
                        let val = self.get_builtin_type(&symbol.name.to_lowercase());
                        trace!(
                            "found _alias {:?} resolved to {:?}",
                            &symbol.name.to_lowercase(),
                            val
                        );
                        val
                    } else {
                        None
                    };
                    if let Some(b_type) = builtin_type {
                        b_type
                    } else {
                        self.get_type(source, &symbol_table, decl_scope)
                            .unwrap_or(PythonType::Unknown)
                    }
                // If the variable was created using a for statement e.g. `a` in: for a in []:
                } else if let Some(for_stmt) = &v.for_target {
                    if let Some(position) = position {
                        if for_stmt.node.end < position {
                            error!("for loop variable used after for loop {}", &symbol.name);
                            return PythonType::Unknown;
                        }
                    }
                    let iter_type = self
                        .get_type(&for_stmt.iter, &symbol_table, decl_scope)
                        .unwrap_or_else(|_| panic!("iterating over unknown {:?}", for_stmt));
                    match iter_type {
                        PythonType::Instance(instance_type) => {
                            let iter_method = match self.lookup_on_class(
                                &symbol_table,
                                &instance_type.class_type,
                                "__iter__",
                            ) {
                                Some(PythonType::Callable(c)) => c,
                                Some(other) => panic!("iter method was not callable: {}", other),
                                None => panic!("next method not found"),
                            };

                            let Some(iter_method_type) = self
                                .resolve_generics(
                                    &iter_method.return_type,
                                    &instance_type.class_type.type_parameters,
                                    &instance_type.specialized_type_parameters,
                                )
                                .class()
                            else {
                                panic!("iter method return type is not class");
                            };

                            let next_method = match self.lookup_on_class(
                                &symbol_table,
                                &iter_method_type,
                                "__next__",
                            ) {
                                Some(PythonType::Callable(c)) => c,
                                Some(other) => panic!("next method was not callable: {}", other),
                                None => panic!("next method not found"),
                            };

                            self.resolve_generics(
                                &next_method.return_type,
                                &iter_method_type.type_parameters,
                                &iter_method_type.specialized,
                            )
                        },
                        PythonType::Class(class_type) => {
                            let iter_method = match self.lookup_on_class(
                                &symbol_table,
                                &class_type,
                                "__iter__",
                            ) {
                                Some(PythonType::Callable(c)) => c,
                                Some(other) => panic!("iter method was not callable: {}", other),
                                None => panic!("next method not found"),
                            };
                            let Some(iter_method_type) = &iter_method.return_type.class()
                            else {
                                panic!("iter method return type is not class");
                            };
                            let next_method = match self.lookup_on_class(
                                &symbol_table,
                                &iter_method_type,
                                "__next__",
                            ) {
                                Some(PythonType::Callable(c)) => c,
                                Some(other) => panic!("next method was not callable: {}", other),
                                None => panic!("next method not found"),
                            };
                            self.resolve_generics(
                               &next_method.return_type,
                               &iter_method_type.type_parameters,
                               &iter_method_type.specialized,
                            )
                            // PythonType::Unknown
                        },
                        _ => {
                            error!("iterating over a {:?} is not defined", iter_type);
                            PythonType::Unknown
                        }
                    }
                } else {
                    PythonType::Unknown
                }
            }
            Declaration::Function(f) => {
                self.get_function_type(
                    &symbol_table,
                    f,
                    // to be able to get the function signature correctly we use the scope id of the
                    // function. Since parameters are defined in that scope.
                    symbol_table.get_scope(f.function_node.node.start),
                )
            }
            Declaration::AsyncFunction(f) => self.get_async_function_type(
                &symbol_table,
                f,
                symbol_table.get_scope(f.function_node.node.start),
            ),
            Declaration::Parameter(p) => {
                if let Some(type_annotation) = &p.type_annotation {
                    let annotated_type =
                        self.get_annotation_type(type_annotation, &symbol_table, decl_scope);
                    if let PythonType::Class(ref c) = annotated_type {
                        let instance_type = InstanceType::new(c.clone(), c.specialized.clone());
                        PythonType::Instance(instance_type)
                    } else {
                        annotated_type
                    }
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
                            .lookup_in_scope(&class_scope.name, parent_scope.id)
                            .expect("class def not found");
                        return self.get_symbol_type(class_def, &symbol_table, position);
                    }
                    PythonType::Unknown
                }
            }
            Declaration::Alias(a) => {
                trace!("evaluating alias {:?}", a);
                // when symbol is an alias that is named to that symbol return Module type
                // e.g. from . import path as _path
                // then type of _path is Module(path)
                match &a.symbol_name {
                    Some(name) => {
                        trace!("finding alias with name {name:?}");
                        let import_result =
                            a.import_result.clone().expect("import result not found");
                        trace!("import result {:?}", import_result);
                        for id in import_result.resolved_ids.iter() {
                            trace!("checking path {:?}", id);
                            let symbol_table_with_alias_def = self.get_symbol_table(id);

                            if let Some(symbol_table_file_name) =
                                symbol_table_with_alias_def.file_path.file_stem()
                            {
                                // if what is imported is the whole file.
                                // e.g.
                                // pkg/
                                //  mod1.py
                                //  __init__.py
                                //
                                // from pkg import mod1
                                if symbol_table_file_name
                                    .to_str()
                                    .is_some_and(|s| s == name.as_str())
                                {
                                    return PythonType::Module(ModuleRef {
                                        module_id: symbol_table_with_alias_def.id,
                                    });
                                }
                            };

                            // sys/__init__.pyi imports sys itself don't know why
                            // If the resolved path is same as current symbol file path
                            // then it's cyclic and do not resolve
                            if symbol_table.id == *id {
                                trace!("alias resolution skipped the import {:?}", import_result);
                                continue;
                            }

                            let alias_symbol_table_name =
                                symbol_table_with_alias_def.get_file_name();
                            if let Some(current_symbol_lookup) =
                                symbol_table_with_alias_def.lookup_in_scope(name, 0)
                            {
                                trace!("alias resolved to {:?}", current_symbol_lookup);
                                return self.get_symbol_type(
                                    current_symbol_lookup,
                                    &symbol_table_with_alias_def,
                                    None,
                                );
                            };

                            for star_import in symbol_table_with_alias_def.star_imports.iter() {
                                trace!("checking star imports {:?}", star_import);
                                for id in star_import.resolved_ids.iter() {
                                    trace!("checking path {:?}", id);
                                    let star_import_sym_table = self.get_symbol_table(id);
                                    let res = star_import_sym_table.lookup_in_scope(name, 0);
                                    // TODO: if an import in the other module imports the previous
                                    // module again as * import then don't come back to the module
                                    // that started the import. Don't know the correct way to
                                    // handle this.
                                    match res {
                                        Some(res) => {
                                            return self.get_symbol_type(
                                                res,
                                                &star_import_sym_table,
                                                None,
                                            );
                                        }
                                        None => continue,
                                    };
                                }
                            }
                        }

                        PythonType::Unknown
                    }
                    None => {
                        match &a.import_node {
                            Some(i) => {
                                let module_name = &i.names[0].name;
                                let Some(module_symbol_table) = self.get_symbol_table_for_module(&a, module_name) else {
                                    return PythonType::Unknown;
                                };
                                return PythonType::Module(ModuleRef {
                                    module_id: module_symbol_table.id,
                                });
                            },
                            None => {
                                return PythonType::Unknown;
                            }
                        }
                    }
                }
            }
            Declaration::TypeParameter(_) => PythonType::Unknown,
            Declaration::TypeAlias(_) => PythonType::Unknown,
            Declaration::Class(c) => self
                .get_class_declaration_type(c, &symbol_table, decl_scope)
                .unwrap_or(PythonType::Unknown),
        };

        result
    }

    fn get_class_declaration_type(
        &self,
        class_symbol: &Class,
        symbol_table: &SymbolTable,
        class_decl_scope: u32,
    ) -> Result<PythonType> {
        // TODO: typevar itself is a class but the rhs is typevar type
        if class_symbol.qual_name == "typing.TypeVar" {
            return Ok(PythonType::TypeVar(TypeVar {
                name: "".to_string(),
                bounds: vec![],
                decl_id: symbol_table.id,
            }));
        }
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
        let mut base_classes = vec![];
        let mut specialized_type_parameters = vec![];
        for base_class in bases {
            let base_type = self.get_type(base_class, symbol_table, class_decl_scope);
            let Ok(PythonType::Class(c)) = base_type else {
                continue;
            };
            let Some(possible_type_parameter) = base_class.as_subscript() else {
                class_def_type_parameters.extend(c.type_parameters);
                continue;
            };
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
                        class_decl_scope,
                    );
                    if class_def_type_parameters.contains(&type_parameter) {
                        continue;
                    }
                    // If a type parameter wa
                    if class_def_type_parameters.contains(&type_parameter) {
                        continue;
                    }
                    class_def_type_parameters.push(type_parameter);
                }
                ast::Expression::Tuple(type_parameters) => {
                    let mut tuple_type_parameters = vec![];
                    for type_parameter in type_parameters.elements.iter() {
                        let type_parameter =
                            self.get_type(type_parameter, symbol_table, class_decl_scope)?;
                        if tuple_type_parameters.contains(&type_parameter) {
                            // TODO: Error type parameters must be unique
                            tuple_type_parameters = vec![PythonType::Unknown];
                            break;
                        }
                        if class_def_type_parameters.contains(&type_parameter) {
                            continue;
                        }
                        tuple_type_parameters.push(type_parameter);
                    }
                    class_def_type_parameters.extend(tuple_type_parameters);
                }
                // TODO: if the type parameters are not specialized in the class definition then
                // they should be any. So here we should check if these type parameters are
                // specified in the class definition.
                _ => {
                    for ele in c.type_parameters.iter() {
                        specialized_type_parameters.push(PythonType::Any);
                    }
                }
            };
            base_classes.push(c);
        }

        Ok(PythonType::Class(ClassType::new(
            class_symbol.clone(),
            class_def_type_parameters,
            base_classes,
            specialized_type_parameters,
        )))
    }

    fn get_sequence_type_from_elements(
        &self,
        elements: &Vec<ast::Expression>,
        symbol_table: &SymbolTable,
        scope_id: u32,
    ) -> PythonType {
        let mut prev_elm_type = PythonType::Unknown;
        for elm in elements {
            let elm_type = self
                .get_type(elm, symbol_table, scope_id)
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

    /// Retrieves a python type that is present in the builtin scope
    fn get_builtin_type(&self, name: &str) -> Option<PythonType> {
        // typeshed has a function class which is not supposed to be there.
        // https://github.com/python/typeshed/issues/2999
        // TODO: do something
        if name == "function" {
            return None;
        }
        let builtins_symbol_table = self.get_symbol_table(&Id(0));
        let builtin_symbol = builtins_symbol_table.lookup_in_scope(name, 0)?;
        let decl = builtin_symbol.last_declaration();
        let found_declaration = match decl {
            Declaration::Class(c) => {
                let decl_scope = decl.declaration_path().scope_id;
                self.get_class_declaration_type(c, &builtins_symbol_table, decl_scope)
                    .unwrap_or_else(|_| {
                        panic!("Error getting type for builtin class: {:?}", c.class_node)
                    })
            }
            Declaration::Function(f) => {
                self.get_function_type(&builtins_symbol_table, f, decl.declaration_path().scope_id)
            }
            Declaration::AsyncFunction(f) => self.get_async_function_type(
                &builtins_symbol_table,
                f,
                decl.declaration_path().scope_id,
            ),
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
        PythonType::Unknown
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

        PythonType::LiteralValue(super::types::KnownValue {
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
                    ast::ConstantValue::Bool(_) => LiteralValue::Bool,
                    ast::ConstantValue::Int => LiteralValue::Int,
                    ast::ConstantValue::Float => LiteralValue::Float,
                    ast::ConstantValue::Str(_) => LiteralValue::Str,
                    ast::ConstantValue::Bytes => LiteralValue::Bytes,
                    ast::ConstantValue::None => LiteralValue::None,
                    // Tuple is illegal if it has parentheses, otherwise it's allowed and the output
                    // a multiValued type Currently even mypy does not support
                    // this, who am I to do it? https://mypy-play.net/?mypy=latest&python=3.10&gist=0df0421d5c85f3b75f65a51cae8616ce
                    ast::ConstantValue::Tuple => {
                        LiteralValue::Int
                        // if t.len() == 1 {
                        //     match t[0].value.clone() {
                        //         ast::ConstantValue::Bool => LiteralValue::Bool,
                        //         ast::ConstantValue::Int => LiteralValue::Int,
                        //         ast::ConstantValue::Float => LiteralValue::Float,
                        //         ast::ConstantValue::Str => LiteralValue::Str,
                        //         ast::ConstantValue::Bytes => LiteralValue::Bytes,
                        //         ast::ConstantValue::None => LiteralValue::None,
                        //         _ => panic!("Tuple type with illegal parameter"),
                        //     }
                        // } else {
                        //     let literal_values = t
                        //         .iter()
                        //         .map(|c| match c.value {
                        //             ast::ConstantValue::Bool => LiteralValue::Bool,
                        //             ast::ConstantValue::Int => LiteralValue::Int,
                        //             ast::ConstantValue::Float => LiteralValue::Float,
                        //             ast::ConstantValue::Str => LiteralValue::Str,
                        //             ast::ConstantValue::Bytes => LiteralValue::Bytes,
                        //             ast::ConstantValue::None => LiteralValue::None,
                        //             _ => panic!("Tuple type with illegal parameter"),
                        //         })
                        //         .collect();
                        //     return literal_values;
                        // }
                    }
                    // Illegal parameter
                    ast::ConstantValue::Ellipsis => {
                        panic!("Literal type with ellipsis value is not supported")
                    }
                    ast::ConstantValue::Complex => {
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
                LiteralValue::Str
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

    // TODO: If the return type of type parameter then use the passed args to determine it's type
    // TODO: The any type can be assumed for the type parameters that are not set but we are not
    // correctly checking if it's set or not.
    fn get_return_type_of_callable(
        &self,
        f_type: &CallableType,
        args: &Vec<ast::Expression>,
        symbol_table: &SymbolTable,
        scope_id: u32,
    ) -> PythonType {
        let ret_type = f_type.return_type.clone();

        let PythonType::TypeVar(ref type_var) = ret_type else {
            return ret_type;
        };

        // If the return type is type var, then check all the passed arguments and find the
        // argument that was passed in the place of that type var and then use the type of passed
        // argument here.
        // If the type var is repeated then make sure all the passed arguments for that type var
        // have the same type. Otherwise return Typing::Unknown
        let target_name = &type_var.name;

        let mut values_matching_type_param: Option<PythonType> = None;

        for (index, callable_arg) in f_type.signature.iter().enumerate() {
            trace!("checking if arg is contains the type var. arg: {callable_arg}, type var: {target_name}");
            match callable_arg.get_type() {
                PythonType::TypeVar(arg_type_var) => {
                    if arg_type_var.name != *target_name {
                        continue;
                    }
                    let passed_arg = args.get(index).expect("arg not found");
                    let passed_arg_type = self
                        .get_type(passed_arg, symbol_table, scope_id)
                        .expect("cannot get type for parameter");
                    match values_matching_type_param {
                        Some(ref v) => {
                            // TODO: This should not be an equality check but assignable check
                            // Also the condition in block below.
                            // https://github.com/python/typing/blob/main/conformance/tests/generics_basic.py#L68
                            if *v != passed_arg_type {
                                error!("Two different types were passed for one type parameter first: {v} second: {passed_arg_type}");
                                return PythonType::Unknown;
                            }
                        }
                        None => values_matching_type_param = Some(passed_arg_type),
                    };
                }
                PythonType::Class(class_type) => {
                    for (type_param_index, type_parameter) in
                        class_type.type_parameters.iter().enumerate()
                    {
                        let Some(type_var) = type_parameter.as_type_var() else {
                            continue;
                        };
                        if type_var.name != *target_name {
                            continue;
                        }
                        // The instance that is passed in place of this class has the type of that
                        // type var
                        let passed_arg = args.get(index).expect("arg not found");
                        let passed_arg_type = self
                            .get_type(passed_arg, symbol_table, scope_id)
                            .expect("cannot get type for parameter");

                        let Some(passed_arg_instance) = passed_arg_type.as_instance() else {
                            error!("expected an instance to be passed for a class type {class_type} but got {passed_arg_type}");
                            continue;
                        };

                        let final_type = passed_arg_instance
                            .specialized_type_parameters
                            .get(type_param_index)
                            .unwrap_or(&PythonType::Any)
                            .clone();

                        match values_matching_type_param {
                            Some(ref v) => {
                                if *v != final_type {
                                    error!("Two different types were passed for one type parameter first: {v} second: {passed_arg_type}");
                                    return PythonType::Unknown;
                                }
                            }
                            None => values_matching_type_param = Some(final_type),
                        };
                    }

                    for (type_param_index, type_parameter) in
                        class_type.specialized.iter().enumerate()
                    {
                        let Some(type_var) = type_parameter.as_type_var() else {
                            continue;
                        };
                        if type_var.name != *target_name {
                            continue;
                        }
                        // The instance that is passed in place of this class has the type of that
                        // type var
                        let passed_arg = args.get(index).expect("arg not found");
                        let passed_arg_type = self
                            .get_type(passed_arg, symbol_table, scope_id)
                            .expect("cannot get type for parameter");

                        let Some(passed_arg_instance) = passed_arg_type.as_instance() else {
                            continue;
                        };

                        let final_type = passed_arg_instance
                            .specialized_type_parameters
                            .get(type_param_index)
                            .unwrap_or(&PythonType::Any)
                            .clone();

                        match values_matching_type_param {
                            Some(ref v) => {
                                if *v != final_type {
                                    error!("Two different types were passed for one type parameter first: {v} second: {passed_arg_type}");
                                    return PythonType::Unknown;
                                }
                            }
                            None => values_matching_type_param = Some(final_type),
                        };
                    }
                }
                _ => continue,
            }
        }
        if values_matching_type_param.is_some() {
            return values_matching_type_param
                .take()
                .expect("type parameter was not in passed args");
        }

        error!("cannot find the type var that function returns in the passed args. args: {args:?} type var: {target_name:?}");

        ret_type
    }

    pub fn lookup_on_class(
        &self,
        symbol_table: &SymbolTable,
        c: &ClassType,
        method_name: &str,
    ) -> Option<PythonType> {
        let class_symbol_table = self.get_dec_symbol_table(&c.details.declaration_path);
        let class_scope = c.details.class_scope_id;
        // TODO: Probably should implement mro here
        // Try to find on the class and it's base classes.
        let bases = self.get_base_classes(c);
        let symbol = class_symbol_table.lookup_attribute(method_name, class_scope);
        if symbol.is_none() {
            for base in bases {
                let base_class = base.expect_class();
                let class_symbol_table =
                    self.get_dec_symbol_table(&base_class.details.declaration_path);
                if let Some(attribute_on_base) = class_symbol_table
                    .lookup_attribute(method_name, base_class.details.class_scope_id)
                {
                    return Some(self.get_symbol_type(
                        attribute_on_base,
                        &class_symbol_table,
                        None,
                    ));
                }
            }
        }

        symbol.map(|node| self.get_symbol_type(node, symbol_table, None))
    }

    /// Find a type inside a Python module
    fn lookup_on_module(
        &self,
        symbol_table: &SymbolTable,
        scope_id: u32,
        module_name: &str,
        attr: &str,
    ) -> Option<PythonType> {
        // See if the module is in the symbol table
        let symbol_table_entry = symbol_table.lookup_in_scope(module_name, scope_id)?;
        match symbol_table_entry.last_declaration() {
            Declaration::Alias(a) => {
                let module_symbol_table = self.get_symbol_table_for_module(&a, module_name)?;
                return Some(self.get_name_type(attr, None, &module_symbol_table, 0));
            }
            _ => {}
        };
        None
    }

    fn get_symbol_table_for_module(&self, alias: &symbol_table::Alias, module_name: &str) -> Option<Arc<SymbolTable>> {
        let Some(ref resolved_import) = alias.import_result else {
            return None;
        };
        for id in resolved_import.resolved_ids.iter() {
            let module_symbol_table = self.get_symbol_table(id);
            if module_name == get_module_name(module_symbol_table.file_path.as_path()) {
                return Some(module_symbol_table);
            }
        }
        return None;
    }

    fn get_function_signature(
        &self,
        arguments: &ast::Arguments,
        symbol_table: &symbol_table::SymbolTable,
        scope_id: u32,
    ) -> Vec<CallableArgs> {
        let mut signature = Vec::with_capacity(arguments.len());
        for argument in arguments.posonlyargs.iter() {
            if let Some(type_annotation) = &argument.annotation {
                signature.push(CallableArgs::PositionalOnly(self.get_annotation_type(
                    type_annotation,
                    symbol_table,
                    scope_id,
                )));
            } else {
                signature.push(CallableArgs::PositionalOnly(self.get_name_type(
                    &argument.arg,
                    Some(argument.node.end),
                    symbol_table,
                    scope_id,
                )));
            }
        }
        for positional in arguments.args.iter() {
            if let Some(type_annotation) = &positional.annotation {
                signature.push(CallableArgs::Positional(self.get_annotation_type(
                    type_annotation,
                    symbol_table,
                    scope_id,
                )));
            } else {
                signature.push(CallableArgs::Positional(self.get_name_type(
                    &positional.arg,
                    Some(positional.node.end),
                    symbol_table,
                    scope_id,
                )));
            }
        }
        for argument in arguments.kwonlyargs.iter() {
            if let Some(type_annotation) = &argument.annotation {
                signature.push(CallableArgs::Keyword(self.get_annotation_type(
                    type_annotation,
                    symbol_table,
                    scope_id,
                )));
            } else {
                signature.push(CallableArgs::Keyword(PythonType::Unknown));
            }
        }

        if let Some(vararg) = &arguments.vararg {
            if let Some(type_annotation) = &vararg.annotation {
                signature.push(CallableArgs::Args(self.get_annotation_type(
                    type_annotation,
                    symbol_table,
                    scope_id,
                )));
            } else {
                signature.push(CallableArgs::Args(PythonType::Unknown));
            }
        }
        if let Some(kwarg) = &arguments.kwarg {
            if let Some(type_annotation) = &kwarg.annotation {
                signature.push(CallableArgs::KwArgs(self.get_annotation_type(
                    type_annotation,
                    symbol_table,
                    scope_id,
                )));
            } else {
                signature.push(CallableArgs::KwArgs(PythonType::Unknown));
            }
        }

        signature
    }

    // TODO(coroutine_annotation): These two are very similar. Maybe should be presented in another
    // way. Async version only needs the return type to be a coroutine.
    fn get_function_type(
        &self,
        symbol_table: &symbol_table::SymbolTable,
        f: &symbol_table::Function,
        arguments_scope_id: u32,
    ) -> PythonType {
        // TODO: handle default values

        let name = f.function_node.name;
        let signature =
            self.get_function_signature(&f.function_node.args, symbol_table, arguments_scope_id);
        let return_type =
            f.function_node
                .returns
                .clone()
                .map_or(PythonType::Unknown, |type_annotation| {
                    self.get_annotation_type(&type_annotation, symbol_table, arguments_scope_id)
                });
        let file = self
            .build_manager
            .files
            .get(&f.declaration_path.symbol_table_id)
            .unwrap();
        PythonType::Callable(Box::new(CallableType::new(
            intern_lookup(name).to_string(),
            signature,
            return_type,
            false,
        )))
    }

    fn get_async_function_type(
        &self,
        symbol_table: &symbol_table::SymbolTable,
        f: &symbol_table::AsyncFunction,
        scope_id: u32,
    ) -> PythonType {
        let arguments = f.function_node.args.clone();
        let name = f.function_node.name;
        let signature = self.get_function_signature(&f.function_node.args, symbol_table, scope_id);
        let return_type = f
            .function_node
            .returns
            .clone()
            .map_or(PythonType::Unknown, |type_annotation| {
                self.get_annotation_type(&type_annotation, symbol_table, scope_id)
            });

        let file = self
            .build_manager
            .files
            .get(&f.declaration_path.symbol_table_id)
            .unwrap();
        PythonType::Callable(Box::new(CallableType::new(
            intern_lookup(name).to_string(),
            signature,
            PythonType::Coroutine(Box::new(types::CoroutineType {
                return_type,
                send_type: PythonType::Any,
                yield_type: PythonType::Any,
            })),
            true,
        )))
    }

    fn get_base_classes(&self, c: &ClassType) -> Vec<PythonType> {
        let mut super_classes = vec![];
        super_classes.push(PythonType::Class(c.clone()));
        for super_class in &c.base_classes {
            super_classes.push(PythonType::Class(super_class.clone()));
        }
        let object_class = self.get_builtin_type("object").expect("object not found");

        super_classes.push(object_class.clone());

        super_classes
    }

    // Resolving all type parameters in a python type based on the given type parameters dict
    fn resolve_generics(
        &self,
        python_type: &PythonType,
        type_parameters: &Vec<PythonType>,
        specialized_types: &Vec<PythonType>,
    ) -> PythonType {
        match &python_type {
            PythonType::None => todo!(),
            PythonType::Unknown => todo!(),
            PythonType::Any => todo!(),
            PythonType::LiteralValue(known_value) => todo!(),
            PythonType::Module(module_ref) => todo!(),
            PythonType::MultiValue(vec) => todo!(),
            PythonType::Callable(callable_type) => todo!(),
            PythonType::Coroutine(coroutine_type) => todo!(),
            PythonType::Class(class_type) => {
                let mut resolved = vec![];
                for tp in type_parameters.iter() {
                    let specialized_type =
                        self.resolve_generics(tp, type_parameters, specialized_types);
                    resolved.push(specialized_type);
                }

                let mut new_class = class_type.clone();
                new_class.specialized = resolved;
                PythonType::Class(new_class)
            }
            PythonType::Instance(instance_type) => todo!(),
            PythonType::Optional(python_type) => todo!(),
            PythonType::TypeVar(type_var) => {
                let name = type_var.name.as_str();
                let mut index: Option<usize> = None;
                for (i, tp) in type_parameters.iter().enumerate() {
                    let PythonType::TypeVar(ref tp) = tp else {
                        continue;
                    };
                    if tp.name == name {
                        index = Some(i);
                    }
                }

                if let Some(pos) = index {
                    return specialized_types[pos].clone();
                }

                error!("Did not find type parameter {name}");
                python_type.clone()
            }
        }
    }

    fn get_dec_symbol_table(&self, decl_path: &DeclarationPath) -> Arc<SymbolTable> {
        let table_id = decl_path.symbol_table_id;
        return self.build_manager.get_symbol_table_by_id(&table_id);
    }

    fn get_symbol_table(&self, id: &Id) -> Arc<SymbolTable> {
        return self.build_manager.get_symbol_table_by_id(id);
    }
}
