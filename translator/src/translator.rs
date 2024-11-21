use std::sync::Arc;
use enderpy_python_parser::ast::{self, *};
use std::error::Error;
use std::fmt::Write;
use log::warn;

use enderpy_python_type_checker::{ast_visitor::TraversalVisitor, file::EnderpyFile, checker::TypeChecker, types::PythonType};

#[derive(Clone, Debug)]
pub struct CppTranslator<'a> {
    pub output: String,
    indent_level: usize,
    checker: Arc<TypeChecker<'a>>,
    file: &'a EnderpyFile,
}

impl<'a> CppTranslator<'a> {
    pub fn new(checker: Arc<TypeChecker<'a>>, file: &'a EnderpyFile) -> Self {
        CppTranslator {
            output: "".to_string(),
            indent_level: 0,
            checker: checker,
            file: file,
        }
    }

    pub fn translate(&mut self) {
        for stmt in self.file.tree.body.iter() {
            self.visit_stmt(stmt);
        }
    }

    pub fn write_indent(&mut self) {
        write!(self.output, "{}", "  ".repeat(self.indent_level));
    }
}

impl<'a> TraversalVisitor for CppTranslator<'a> {
    fn visit_stmt(&mut self, s: &ast::Statement) {
        self.write_indent();
        match s {
            Statement::ExpressionStatement(e) => self.visit_expr(e),
            Statement::Import(i) => self.visit_import(i),
            Statement::ImportFrom(i) => self.visit_import_from(i),
            Statement::AssignStatement(a) => self.visit_assign(a),
            Statement::AnnAssignStatement(a) => self.visit_ann_assign(a),
            Statement::AugAssignStatement(a) => self.visit_aug_assign(a),
            Statement::Assert(a) => self.visit_assert(a),
            Statement::Pass(p) => self.visit_pass(p),
            Statement::Delete(d) => self.visit_delete(d),
            Statement::ReturnStmt(r) => self.visit_return(r),
            Statement::Raise(r) => self.visit_raise(r),
            Statement::BreakStmt(b) => self.visit_break(b),
            Statement::ContinueStmt(c) => self.visit_continue(c),
            Statement::Global(g) => self.visit_global(g),
            Statement::Nonlocal(n) => self.visit_nonlocal(n),
            Statement::IfStatement(i) => self.visit_if(i),
            Statement::WhileStatement(w) => self.visit_while(w),
            Statement::ForStatement(f) => self.visit_for(f),
            Statement::WithStatement(w) => self.visit_with(w),
            Statement::TryStatement(t) => self.visit_try(t),
            Statement::TryStarStatement(t) => self.visit_try_star(t),
            Statement::FunctionDef(f) => self.visit_function_def(f),
            Statement::ClassDef(c) => self.visit_class_def(c),
            Statement::MatchStmt(m) => self.visit_match(m),
            Statement::AsyncForStatement(f) => self.visit_async_for(f),
            Statement::AsyncWithStatement(w) => self.visit_async_with(w),
            Statement::AsyncFunctionDef(f) => self.visit_async_function_def(f),
            Statement::TypeAlias(a) => self.visit_type_alias(a),
        }
    }

    fn visit_expr(&mut self, e: &Expression) {
        match e {
            Expression::Constant(c) => self.visit_constant(c),
            Expression::List(l) => self.visit_list(l),
            Expression::Tuple(t) => self.visit_tuple(t),
            Expression::Dict(d) => self.visit_dict(d),
            Expression::Set(s) => self.visit_set(s),
            Expression::Name(n) => self.visit_name(n),
            Expression::BoolOp(b) => self.visit_bool_op(b),
            Expression::UnaryOp(u) => self.visit_unary_op(u),
            Expression::BinOp(b) => self.visit_bin_op(b),
            Expression::NamedExpr(n) => self.visit_named_expr(n),
            Expression::Yield(y) => self.visit_yield(y),
            Expression::YieldFrom(y) => self.visit_yield_from(y),
            Expression::Starred(s) => self.visit_starred(s),
            Expression::Generator(g) => self.visit_generator(g),
            Expression::ListComp(l) => self.visit_list_comp(l),
            Expression::SetComp(s) => self.visit_set_comp(s),
            Expression::DictComp(d) => self.visit_dict_comp(d),
            Expression::Attribute(a) => self.visit_attribute(a),
            Expression::Subscript(s) => self.visit_subscript(s),
            Expression::Slice(s) => self.visit_slice(s),
            Expression::Call(c) => self.visit_call(c),
            Expression::Await(a) => self.visit_await(a),
            Expression::Compare(c) => self.visit_compare(c),
            Expression::Lambda(l) => self.visit_lambda(l),
            Expression::IfExp(i) => self.visit_if_exp(i),
            Expression::JoinedStr(j) => self.visit_joined_str(j),
            Expression::FormattedValue(f) => self.visit_formatted_value(f),
        }
    }

    fn visit_constant(&mut self, constant: &Constant) {
        match constant.value {
            ConstantValue::None => write!(self.output, "None"),
            ConstantValue::Ellipsis => write!(self.output, "..."),
            ConstantValue::Bool(_) => write!(self.output, "bool"),
            ConstantValue::Str(_) => write!(self.output, "{}", constant.get_value(&self.file.source).to_string()),
            ConstantValue::Bytes => write!(self.output, "bytes"),
            ConstantValue::Tuple => write!(self.output, "tuple"),
            ConstantValue::Int => write!(self.output, "int"),
            ConstantValue::Float => write!(self.output, "float"),
            ConstantValue::Complex => write!(self.output, "complex"),
            /*
            Constant::Tuple(elements) => {
                let tuple_elements: Vec<String> = elements
                    .iter()
                    .map(|elem| self.translate_constant(elem))
                    .collect::<Result<Vec<String>, _>>()?;
                Ok(format!("({})", tuple_elements.join(", ")))
            },
            */
        };
    }

    fn visit_import(&mut self, import: &Import) {
        for name in import.names.iter() {
            if name.name == "torch" {
                writeln!(self.output, "#include <torch/torch.h>");
            }
        }
    }

    fn visit_assign(&mut self, a: &Assign) {
        for target in &a.targets {
            // let type = self.checker.types.
            match target {
                Expression::Name(n) => {
                    println!("XXX {}", n.node.start);
                    // This loop should only iterate once
                    for t in self.checker.types.find(n.node.start, n.node.end) {
                        write!(self.output, "{} ", python_type_to_cpp(&t.val));
                    }
                    self.visit_name(n);
                },
                _ => {
                    self.visit_expr(target);
                }
            }
        }
        write!(self.output, " = ");
        self.visit_expr(&a.value);
    }

    fn visit_name(&mut self, name: &Name) {
        write!(self.output, "{}", name.id);
    }

    fn visit_call(&mut self, c: &Call) {
        self.visit_expr(&c.func);
        write!(self.output, "(");
        for arg in &c.args {
            self.visit_expr(arg);
        }
        for keyword in &c.keywords {
            self.visit_expr(&keyword.value);
        }
        write!(self.output, ")");
    }

    fn visit_attribute(&mut self, attribute: &Attribute) {
        self.visit_expr(&attribute.value);
        write!(self.output, "::{}", attribute.attr);
    }
}

fn python_type_to_cpp(python_type: &PythonType) -> String {
    match python_type {
        PythonType::Class(c) => {
            c.details.name.clone()
        },
        _ => String::from("<unknown type>")
    }
}

/*
impl CppTranslator {
    fn new() -> Self {
        CppTranslator::default()
    }

    fn translate_ast(&mut self, ast: &ast::Mod) -> Result<String, Box<dyn Error>> {
        match ast {
	    ast::Mod::Module(ast::ModModule { body, .. }) => {
	        for stmt in body.iter() {
		    self.translate_stmt(stmt)?;
		}
	    },
	    ast::Mod::Interactive(_) => {
	    },
	    ast::Mod::FunctionType(_) => {
	    },
	    ast::Mod::Expression(_) => {
	    },
	}
        
        Ok(self.output.clone())
    }

    fn translate_constant(&mut self, constant: &ast::Constant) -> Result<String, Box<dyn Error>> {
        match constant {
            ast::Constant::Int(n) => Ok(n.to_string()),
            ast::Constant::Float(f) => Ok(f.to_string()),
            ast::Constant::Complex { real, imag } => Ok(format!("{}+{}j", real, imag)),
            ast::Constant::Str(s) => Ok(format!("\"{}\"", s)),
            ast::Constant::Bool(b) => Ok(b.to_string()),
            ast::Constant::None => Ok("None".to_string()),
            ast::Constant::Tuple(elements) => {
                let tuple_elements: Vec<String> = elements
                    .iter()
                    .map(|elem| self.translate_constant(elem))
                    .collect::<Result<Vec<String>, _>>()?;
                Ok(format!("({})", tuple_elements.join(", ")))
            },
            ast::Constant::Ellipsis => Ok("...".to_string()),
            ast::Constant::Bytes(bytes) => Ok(format!("b\"{}\"", String::from_utf8_lossy(bytes))),
        }
    }

    fn translate_expr(&mut self, expr: &ast::Expr) -> Result<String, Box<dyn Error>> {
        match expr {
            ast::Expr::BoolOp(_) => {},
            ast::Expr::NamedExpr(_) => {},
            ast::Expr::BinOp(_) => {},
            ast::Expr::UnaryOp(_) => {},
            ast::Expr::Lambda(_) => {},
            ast::Expr::IfExp(_) => {},
            ast::Expr::Dict(_) => {},
            ast::Expr::Set(_) => {},
            ast::Expr::ListComp(_) => {},
            ast::Expr::SetComp(_) => {},
            ast::Expr::DictComp(_) => {},
            ast::Expr::GeneratorExp(_) => {},
            ast::Expr::Await(_) => {},
            ast::Expr::Yield(_) => {},
            ast::Expr::YieldFrom(_) => {},
            ast::Expr::Compare(_) => {},
            ast::Expr::Call(_) => {},
            ast::Expr::FormattedValue(_) => {},
            ast::Expr::JoinedStr(_) => {},
            ast::Expr::Constant(c) => {
	        let s = self.translate_constant(&c.value)?;
	        write!(self.output, "{}", s);
	    },
            ast::Expr::Attribute(_) => {},
            ast::Expr::Subscript(_) => {},
            ast::Expr::Starred(_) => {},
            ast::Expr::Name(name) => {
	        write!(self.output, "{}", name.id);
	    },
            ast::Expr::List(_) => {},
            ast::Expr::Tuple(_) => {},
            ast::Expr::Slice(_) => {},
	}
	Ok(self.output.clone())
    }

    fn translate_stmt(&mut self, stmt: &ast::Stmt) -> Result<String, Box<dyn Error>> {
        match stmt {
	    ast::Stmt::Assign(assign) => {
	        self.write_indent()?;
	        self.translate_expr(&assign.targets[0])?;
	        write!(self.output, " = ");
		self.translate_expr(&assign.value)?;
		write!(self.output, "\n");
	    },
	    ast::Stmt::FunctionDef(function) => {
	        writeln!(self.output, "void {}() {{", function.name.to_string());
		self.indent_level += 1;
		for stmt in function.body.iter() {
                    self.translate_stmt(stmt)?;
                }
		self.indent_level -= 1;
		writeln!(self.output, "}}");
	    },
	    ast::Stmt::AsyncFunctionDef(_) => {},
    	    ast::Stmt::ClassDef(_) => {},
    	    ast::Stmt::Return(_) => {},
    	    ast::Stmt::Delete(_) => {},
    	    ast::Stmt::TypeAlias(_) => {},
    	    ast::Stmt::AugAssign(_) => {},
    	    ast::Stmt::AnnAssign(_) => {},
    	    ast::Stmt::For(_) => {},
    	    ast::Stmt::AsyncFor(_) => {},
    	    ast::Stmt::While(_) => {},
    	    ast::Stmt::If(_) => {},
    	    ast::Stmt::With(_) => {},
    	    ast::Stmt::AsyncWith(_) => {},
    	    ast::Stmt::Match(_) => {},
    	    ast::Stmt::Raise(_) => {},
    	    ast::Stmt::Try(_) => {},
    	    ast::Stmt::TryStar(_) => {},
    	    ast::Stmt::Assert(_) => {},
    	    ast::Stmt::Import(imp) => {
	        if imp.names[0].name.to_string() == "torch" {
		    writeln!(self.output, "#include <torch/torch.h>");
		}
	    },
    	    ast::Stmt::ImportFrom(_) => {},
    	    ast::Stmt::Global(_) => {},
    	    ast::Stmt::Nonlocal(_) => {},
    	    ast::Stmt::Expr(_) => {},
    	    ast::Stmt::Pass(_) => {},
    	    ast::Stmt::Break(_) => {},
    	    ast::Stmt::Continue(_) => {},
	}
	Ok(self.output.clone())
    }

    fn write_indent(&mut self) -> Result<(), Box<dyn Error>> {
        write!(self.output, "{}", "  ".repeat(self.indent_level))?;
        Ok(())
    }
}

pub fn python_to_cpp(python_ast: &ast::Mod) -> Result<String, Box<dyn Error>> {
    let mut translator = CppTranslator::new();
    translator.translate_ast(python_ast)
}

*/
