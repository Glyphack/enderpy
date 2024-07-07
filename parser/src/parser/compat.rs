use std::io::Write;
use std::convert::From;
// use serde::{Deserialize, Serialize};
use miette::{bail, IntoDiagnostic, Result};
use serde_json::{json, Value};
// use enderpy_python_ast::*;
// use enderpy_python_ast::visitor::TraversalVisitor;

use crate::Parser;
use crate::ast::*;
use crate::runpython::{default_python_path, spawn_python_script_command};

type ToRowColFn = dyn Fn(u32) -> (u32, u32);

fn parse_python_source(source: &str) -> Result<Value> {
    let mut process = spawn_python_script_command(
        "parser/ast_python.py",
        vec!["--stdin"],
        default_python_path()?,
    )?;

    // Get process stdin and write the input string.
    if let Some(mut stdin) = process.stdin.take() {
        stdin.write_all(source.as_bytes()).into_diagnostic()?;
    } else {
        bail!("Failed to open stdin when running `parser/ast_python.py`");
    }
    // Get process stdout and parse result.
    let output = process.wait_with_output().into_diagnostic()?;
    let ast: Value = serde_json::from_str(String::from_utf8_lossy(&output.stdout).as_ref()).into_diagnostic()?;
    Ok(ast)
}

fn parse_enderpy_source(source: &str) -> Result<Value> {
    let mut parser = Parser::new(source, "string");
    let typed_ast = parser.parse().into_diagnostic()?;
    todo!();
}

trait IntoPythonCompat {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value;
}

trait IntoNullablePythonCompat<T: IntoPythonCompat> {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value;
}

impl<T: IntoPythonCompat> IntoNullablePythonCompat<T> for Option<T> {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        if let Some(v) = self {
            v.into_python_compat(to_row_col)
        } else {
            json!(null)
        }
    }
}

macro_rules! json_python_compat_node {
    ($name:literal, $instance:ident, $to_row_col:ident, $other_fields:tt) => {
        {
            let mut node = json!($other_fields);
            let (start_row, start_col) = $to_row_col($instance.node.start);
            let (end_row, end_col) = $to_row_col($instance.node.end);
            node["_type"] = json!($name);
            node["lineno"] = json!(start_row);
            node["col_offset"] = json!(start_col);
            node["end_lineno"] = json!(end_row);
            node["end_col_offset"] = json!(end_col);
            node
        }
    };
}

impl IntoPythonCompat for Module {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json!({
            "_type": "Module",
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for Statement {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        match self {
            Statement::ExpressionStatement(e) => e.into_python_compat(to_row_col),
            Statement::Import(i) => i.into_python_compat(to_row_col),
            Statement::ImportFrom(i) => i.into_python_compat(to_row_col),
            Statement::AssignStatement(a) => a.into_python_compat(to_row_col),
            Statement::AnnAssignStatement(a) => a.into_python_compat(to_row_col),
            Statement::AugAssignStatement(a) => a.into_python_compat(to_row_col),
            Statement::Assert(a) => a.into_python_compat(to_row_col),
            Statement::Pass(p) => p.into_python_compat(to_row_col),
            Statement::Delete(d) => d.into_python_compat(to_row_col),
            Statement::Return(r) => r.into_python_compat(to_row_col),
            Statement::Raise(r) => r.into_python_compat(to_row_col),
            Statement::Break(b) => b.into_python_compat(to_row_col),
            Statement::Continue(c) => c.into_python_compat(to_row_col),
            Statement::Global(g) => g.into_python_compat(to_row_col),
            Statement::Nonlocal(n) => n.into_python_compat(to_row_col),
            Statement::IfStatement(i) => i.into_python_compat(to_row_col),
            Statement::WhileStatement(w) => w.into_python_compat(to_row_col),
            Statement::ForStatement(f) => f.into_python_compat(to_row_col),
            Statement::WithStatement(w) => w.into_python_compat(to_row_col),
            Statement::TryStatement(t) => t.into_python_compat(to_row_col),
            Statement::TryStarStatement(t) => t.into_python_compat(to_row_col),
            Statement::FunctionDef(f) => f.into_python_compat(to_row_col),
            Statement::ClassDef(c) => c.into_python_compat(to_row_col),
            Statement::Match(m) => m.into_python_compat(to_row_col),
            Statement::AsyncForStatement(f) => f.into_python_compat(to_row_col),
            Statement::AsyncWithStatement(w) => w.into_python_compat(to_row_col),
            Statement::AsyncFunctionDef(f) => f.into_python_compat(to_row_col),
            Statement::TypeAlias(t) => t.into_python_compat(to_row_col),
        }
    }
}

impl IntoPythonCompat for Assign {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Assign", self, to_row_col, {
            "targets": self.targets.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for AnnAssign {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("AnnAssign", self, to_row_col, {
            "target": self.target.into_python_compat(to_row_col),
            "annotation": self.annotation.into_python_compat(to_row_col),
            "value": self.value.into_python_compat(to_row_col),
            "simple": self.simple,
        })
    }
}

impl IntoPythonCompat for AugAssign {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("AugAssign", self, to_row_col, {
            "target": self.target.into_python_compat(to_row_col),
            "op": self.op.into_python_compat(to_row_col),
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for AugAssignOp {
    fn into_python_compat(&self, _: &ToRowColFn) -> Value {
        match self {
            AugAssignOp::Add => json!({"_type": "Add"}),
            AugAssignOp::Sub => json!({"_type": "Sub"}),
            AugAssignOp::Mult => json!({"_type": "Mult"}),
            AugAssignOp::MatMult => json!({"_type": "MatMult"}),
            AugAssignOp::Div => json!({"_type": "Div"}),
            AugAssignOp::Mod => json!({"_type": "Mod"}),
            AugAssignOp::Pow => json!({"_type": "Pow"}),
            AugAssignOp::LShift => json!({"_type": "LShift"}),
            AugAssignOp::RShift => json!({"_type": "RShift"}),
            AugAssignOp::BitOr => json!({"_type": "BitOr"}),
            AugAssignOp::BitXor => json!({"_type": "BitXor"}),
            AugAssignOp::BitAnd => json!({"_type": "BitAnd"}),
            AugAssignOp::FloorDiv => json!({"_type": "FloorDiv"}),
        }
    }
}

impl IntoPythonCompat for Assert {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Assert", self, to_row_col, {
            "test": self.test.into_python_compat(to_row_col),
            "msg": self.msg.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Pass {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Pass", self, to_row_col, {})
    }
}

impl IntoPythonCompat for Delete {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Delete", self, to_row_col, {
            "targets": self.targets.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for Return {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Return", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Raise {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Raise", self, to_row_col, {
            "exc": self.exc.into_python_compat(to_row_col),
            "cause": self.cause.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Break {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Break", self, to_row_col, {})
    }
}

impl IntoPythonCompat for Continue {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Continue", self, to_row_col, {})
    }
}

impl IntoPythonCompat for Import {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Import", self, to_row_col, {
            "names": self.names.iter().map(|alias| alias.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for Alias {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Alias", self, to_row_col, {
            "name": self.name,
            "asname": self.asname,
        })
    }
}

impl IntoPythonCompat for ImportFrom {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("ImportFrom", self, to_row_col, {
            "module": self.module,
            "names": self.names.iter().map(|alias| alias.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "level": self.level,
        })
    }
}

impl IntoPythonCompat for Global {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Global", self, to_row_col, {
            "names": self.names,
        })
    }
}

impl IntoPythonCompat for Nonlocal {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Nonlocal", self, to_row_col, {
            "names": self.names,
        })
    }
}

impl IntoPythonCompat for Expression {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        match self {
            Expression::Constant(c) => c.into_python_compat(to_row_col),
            Expression::List(l) => l.into_python_compat(to_row_col),
            Expression::Tuple(t) => t.into_python_compat(to_row_col),
            Expression::Dict(d) => d.into_python_compat(to_row_col),
            Expression::Set(s) => s.into_python_compat(to_row_col),
            Expression::Name(n) => n.into_python_compat(to_row_col),
            Expression::BoolOp(b) => b.into_python_compat(to_row_col),
            Expression::UnaryOp(u) => u.into_python_compat(to_row_col),
            Expression::BinOp(b) => b.into_python_compat(to_row_col),
            Expression::NamedExpr(n) => n.into_python_compat(to_row_col),
            Expression::Yield(y) => y.into_python_compat(to_row_col),
            Expression::YieldFrom(y) => y.into_python_compat(to_row_col),
            Expression::Starred(s) => s.into_python_compat(to_row_col),
            Expression::Generator(g) => g.into_python_compat(to_row_col),
            Expression::ListComp(l) => l.into_python_compat(to_row_col),
            Expression::SetComp(s) => s.into_python_compat(to_row_col),
            Expression::DictComp(d) => d.into_python_compat(to_row_col),
            Expression::Attribute(a) => a.into_python_compat(to_row_col),
            Expression::Subscript(s) => s.into_python_compat(to_row_col),
            Expression::Slice(s) => s.into_python_compat(to_row_col),
            Expression::Call(c) => c.into_python_compat(to_row_col),
            Expression::Await(a) => a.into_python_compat(to_row_col),
            Expression::Compare(c) => c.into_python_compat(to_row_col),
            Expression::Lambda(l) => l.into_python_compat(to_row_col),
            Expression::IfExp(i) => i.into_python_compat(to_row_col),
            Expression::JoinedStr(j) => j.into_python_compat(to_row_col),
            Expression::FormattedValue(f) => f.into_python_compat(to_row_col),
        }
    }
}

impl IntoPythonCompat for Name {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Name", self, to_row_col, {
            "id": self.id,
        })
    }
}

impl IntoPythonCompat for Constant {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Constant", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for ConstantValue {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        match self {
            ConstantValue::None => json!(null),
            ConstantValue::Ellipsis => json!("..."),
            ConstantValue::Bool(v) => json!(v),
            ConstantValue::Str(v) => json!(v),
            ConstantValue::Bytes(v) => json!(v),
            ConstantValue::Tuple(v) => json!(v.iter().map(|cons| cons.into_python_compat(to_row_col)).collect::<Vec<_>>()),
            ConstantValue::Int(v) => json!(v),
            ConstantValue::Float(v) => json!(v),
            ConstantValue::Complex { real, imaginary } => json!({"real": real, "imaginary": imaginary}),
        }
    }
}

impl IntoPythonCompat for List {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("List", self, to_row_col, {
            "elements": self.elements.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for Tuple {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Tuple", self, to_row_col, {
            "elements": self.elements.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for Dict {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Dict", self, to_row_col, {
            "keys": self.keys.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "values": self.values.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for Set {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Set", self, to_row_col, {
            "elements": self.elements.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for BoolOperation {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("BoolOperation", self, to_row_col, {
            "op": self.op.into_python_compat(to_row_col),
            "values": self.values.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for BooleanOperator {
    fn into_python_compat(&self, _: &ToRowColFn) -> Value {
        match self {
            BooleanOperator::And => json!({"_type": "And"}),
            BooleanOperator::Or => json!({"_type": "Or"}),
        }
    }
}

impl IntoPythonCompat for UnaryOperation {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("UnaryOperation", self, to_row_col, {
            "op": self.op.into_python_compat(to_row_col),
            "operand": self.operand.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for UnaryOperator {
    fn into_python_compat(&self, _: &ToRowColFn) -> Value {
        match self {
            UnaryOperator::Not => json!({"_type": "Not"}),
            UnaryOperator::Invert => json!({"_type": "Invert"}),
            UnaryOperator::UAdd => json!({"_type": "UAdd"}),
            UnaryOperator::USub => json!({"_type": "USub"}),
        }
    }
}

impl IntoPythonCompat for BinOp {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("BinOp", self, to_row_col, {
            "op": self.op.into_python_compat(to_row_col),
            "left": self.left.into_python_compat(to_row_col),
            "right": self.right.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for BinaryOperator {
    fn into_python_compat(&self, _: &ToRowColFn) -> Value {
        match self {
            BinaryOperator::Add => json!({"_type": "Add"}),
            BinaryOperator::Sub => json!({"_type": "Sub"}),
            BinaryOperator::Mult => json!({"_type": "Mult"}),
            BinaryOperator::MatMult => json!({"_type": "MatMult"}),
            BinaryOperator::Div => json!({"_type": "Div"}),
            BinaryOperator::Mod => json!({"_type": "Mod"}),
            BinaryOperator::Pow => json!({"_type": "Pow"}),
            BinaryOperator::LShift => json!({"_type": "LShift"}),
            BinaryOperator::RShift => json!({"_type": "RShift"}),
            BinaryOperator::BitOr => json!({"_type": "BitOr"}),
            BinaryOperator::BitXor => json!({"_type": "BitXor"}),
            BinaryOperator::BitAnd => json!({"_type": "BitAnd"}),
            BinaryOperator::FloorDiv => json!({"_type": "FloorDiv"}),
        }
    }
}

impl IntoPythonCompat for NamedExpression {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("NamedExpression", self, to_row_col, {
            "target": self.target.into_python_compat(to_row_col),
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Yield {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Yield", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for YieldFrom {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("YieldForm", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Starred {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Starred", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Generator {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Generator", self, to_row_col, {
            "element": self.element.into_python_compat(to_row_col),
            "generators": self.generators.iter().map(|gen| gen.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for ListComp {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("ListComp", self, to_row_col, {
            "element": self.element.into_python_compat(to_row_col),
            "generators": self.generators.iter().map(|gen| gen.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for SetComp {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("SetComp", self, to_row_col, {
            "element": self.element.into_python_compat(to_row_col),
            "generators": self.generators.iter().map(|gen| gen.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for DictComp {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("DictComp", self, to_row_col, {
            "key": self.key.into_python_compat(to_row_col),
            "value": self.value.into_python_compat(to_row_col),
            "generators": self.generators.iter().map(|gen| gen.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for Comprehension {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Comprehension", self, to_row_col, {
            "target": self.target.into_python_compat(to_row_col),
            "iter": self.iter.into_python_compat(to_row_col),
            "ifs": self.ifs.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "is_async": self.is_async,
        })
    }
}

impl IntoPythonCompat for Attribute {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Attribute", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
            "attr": self.attr,
        })
    }
}

impl IntoPythonCompat for Subscript {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Subscript", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
            "slice": self.slice.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Slice {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Slice", self, to_row_col, {
            "lower": self.lower.into_python_compat(to_row_col),
            "upper": self.upper.into_python_compat(to_row_col),
            "step": self.step.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Call {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Call", self, to_row_col, {
            "func": self.func.into_python_compat(to_row_col),
            "args": self.args.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "keywords": self.keywords.iter().map(|kw| kw.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "starargs": self.starargs.into_python_compat(to_row_col),
            "kwargs": self.kwargs.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Keyword {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Keyword", self, to_row_col, {
            "arg": self.arg.as_ref().map_or(json!(null), |s| json!(s)),
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Await {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Await", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Compare {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Compar", self, to_row_col, {
            "left": self.left.into_python_compat(to_row_col),
            "ops": self.ops.iter().map(|op| op.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "comparators": self.comparators.iter().map(|op| op.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for ComparisonOperator {
    fn into_python_compat(&self, _: &ToRowColFn) -> Value {
        match self {
            ComparisonOperator::Eq => json!({"_type": "Eq"}),
            ComparisonOperator::NotEq => json!({"_type": "NotEq"}),
            ComparisonOperator::Lt => json!({"_type": "Lt"}),
            ComparisonOperator::LtE => json!({"_type": "LtE"}),
            ComparisonOperator::Gt => json!({"_type": "Gt"}),
            ComparisonOperator::GtE => json!({"_type": "GtE"}),
            ComparisonOperator::Is => json!({"_type": "Is"}),
            ComparisonOperator::IsNot => json!({"_type": "IsNot"}),
            ComparisonOperator::In => json!({"_type": "In"}),
            ComparisonOperator::NotIn => json!({"_type": "NotIn"}),
        }
    }
}

impl IntoPythonCompat for Lambda {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Lambda", self, to_row_col, {
            "args": self.args.into_python_compat(to_row_col),
            "body": self.body.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for crate::ast::Arguments {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Arguments", self, to_row_col, {
            "posonlyargs": self.posonlyargs.iter().map(|arg| arg.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "args": self.args.iter().map(|arg| arg.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "vararg": self.vararg.into_python_compat(to_row_col),
            "kwonlyargs": self.kwonlyargs.iter().map(|arg| arg.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "kw_defaults": self.kw_defaults.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "kwarg": self.kwarg.into_python_compat(to_row_col),
            "defaults": self.defaults.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for Arg {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Arg", self, to_row_col, {
            "arg": self.arg,
            "annotation": self.annotation.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for IfExp {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("IfExp", self, to_row_col, {
            "test": self.test.into_python_compat(to_row_col),
            "body": self.body.into_python_compat(to_row_col),
            "orelse": self.orelse.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for FormattedValue {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("FormattedValue", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
            "conversion": self.conversion,
            "format_spec": self.format_spec.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for JoinedStr {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("JoinedStr", self, to_row_col, {
            "values": self.values.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for If {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("If", self, to_row_col, {
            "test": self.test.into_python_compat(to_row_col),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for While {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("While", self, to_row_col, {
            "test": self.test.into_python_compat(to_row_col),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for For {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("For", self, to_row_col, {
            "target": self.target.into_python_compat(to_row_col),
            "iter": self.iter.into_python_compat(to_row_col),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for AsyncFor {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("AsyncFor", self, to_row_col, {
            "target": self.target.into_python_compat(to_row_col),
            "iter": self.iter.into_python_compat(to_row_col),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for With {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("With", self, to_row_col, {
            "items": self.items.iter().map(|wi| wi.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for AsyncWith {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("AsyncWith", self, to_row_col, {
            "items": self.items.iter().map(|wi| wi.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for WithItem {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("WithItem", self, to_row_col, {
            "context_expr": self.context_expr.into_python_compat(to_row_col),
            "optional_vars": self.optional_vars.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for Try {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Try", self, to_row_col, {
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "handlers": self.handlers.iter().map(|hndl| hndl.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "finalbody": self.finalbody.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for TryStar {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("TryStar", self, to_row_col, {
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "handlers": self.handlers.iter().map(|hndl| hndl.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "finalbody": self.finalbody.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for ExceptHandler {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("ExceptHandler", self, to_row_col, {
            "type": self.typ.into_python_compat(to_row_col),
            "name": self.name.as_ref().map_or(json!(null), |s| json!(s)),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for FunctionDef {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("FunctionDef", self, to_row_col, {
            "name": self.name,
            "args": self.args.into_python_compat(to_row_col),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "decorator_list": self.decorator_list.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "returns": self.returns.into_python_compat(to_row_col),
            "type_comment": self.type_comment.as_ref().map_or(json!(null), |s| json!(s)),
            "type_params": self.type_params.iter().map(|tp| tp.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for AsyncFunctionDef {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("AsyncFunctionDef", self, to_row_col, {
            "name": self.name,
            "args": self.args.into_python_compat(to_row_col),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "decorator_list": self.decorator_list.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "returns": self.returns.into_python_compat(to_row_col),
            "type_comment": self.type_comment.as_ref().map_or(json!(null), |s| json!(s)),
            "type_params": self.type_params.iter().map(|tp| tp.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for ClassDef {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("ClassDef", self, to_row_col, {
            "name": self.name,
            "bases": self.bases.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "keywords": self.keywords.iter().map(|kw| kw.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "decorator_list": self.decorator_list.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "type_params": self.type_params.iter().map(|tp| tp.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for Match {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("Match", self, to_row_col, {
            "subject": self.subject.into_python_compat(to_row_col),
            "cases": self.cases.iter().map(|mc| mc.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for MatchCase {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("MatchCase", self, to_row_col, {
            "pattern": self.pattern.into_python_compat(to_row_col),
            "guard": self.guard.into_python_compat(to_row_col),
            "body": self.body.iter().map(|stmt| stmt.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for MatchPattern {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        match self {
            MatchPattern::MatchValue(val) => val.into_python_compat(to_row_col),
            MatchPattern::MatchSingleton(expr) => expr.into_python_compat(to_row_col),
            MatchPattern::MatchSequence(pats) => json!(pats.iter().map(|pat| pat.into_python_compat(to_row_col)).collect::<Vec<_>>()),
            MatchPattern::MatchStar(expr) => expr.into_python_compat(to_row_col),
            MatchPattern::MatchMapping(map) => map.into_python_compat(to_row_col),
            MatchPattern::MatchAs(mas) => mas.into_python_compat(to_row_col),
            MatchPattern::MatchClass(cls) => cls.into_python_compat(to_row_col),
            MatchPattern::MatchOr(pats) => json!(pats.iter().map(|pat| pat.into_python_compat(to_row_col)).collect::<Vec<_>>()),
        }
    }
}

impl IntoPythonCompat for MatchValue {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("MatchValue", self, to_row_col, {
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for MatchAs {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("MatchAs", self, to_row_col, {
            "name": self.name.as_ref().map_or(json!(null), |s| json!(s)),
            "pattern": self.pattern.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for MatchMapping {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("MatchMapping", self, to_row_col, {
            "keys": self.keys.iter().map(|expr| expr.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "patterns": self.patterns.iter().map(|pat| pat.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "rest": self.rest.as_ref().map_or(json!(null), |s| json!(s)),
        })
    }
}

impl IntoPythonCompat for MatchClass {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("MatchClass", self, to_row_col, {
            "cls": self.cls.into_python_compat(to_row_col),
            "patterns": self.patterns.iter().map(|pat| pat.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "kwd_attrs": self.kwd_attrs,
            "kwd_patterns": self.kwd_patterns.iter().map(|pat| pat.into_python_compat(to_row_col)).collect::<Vec<_>>(),
        })
    }
}

impl IntoPythonCompat for TypeParam {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        match self {
            TypeParam::TypeVar(var) => var.into_python_compat(to_row_col),
            TypeParam::ParamSpec(spec) => spec.into_python_compat(to_row_col),
            TypeParam::TypeVarTuple(tup) => tup.into_python_compat(to_row_col),
        }
    }
}

impl IntoPythonCompat for TypeVar {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("TypeVar", self, to_row_col, {
            "name": self.name,
            "bound": self.bound.into_python_compat(to_row_col),
        })
    }
}

impl IntoPythonCompat for ParamSpec {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("ParamSpec", self, to_row_col, {
            "name": self.name,
        })
    }
}

impl IntoPythonCompat for TypeVarTuple {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("TypeVarTuple", self, to_row_col, {
            "name": self.name,
        })
    }
}

impl IntoPythonCompat for TypeAlias {
    fn into_python_compat(&self, to_row_col: &ToRowColFn) -> Value {
        json_python_compat_node!("TypeAlias", self, to_row_col, {
            "name": self.name,
            "type_params": self.type_params.iter().map(|tp| tp.into_python_compat(to_row_col)).collect::<Vec<_>>(),
            "value": self.value.into_python_compat(to_row_col),
        })
    }
}

#[cfg(test)]
mod tests {
    use serde_json::Value;
    use miette::{bail, IntoDiagnostic, Result};
    use crate::Parser;
    use crate::ast::*;

    use super::parse_python_source;

    #[test]
    fn test_simple_compat() {
        let source = r#"
a: int = 1
print(a)
"#;
        let mut parser = Parser::new(source, "string");
        let enderpy_ast = parser.parse().into_diagnostic().unwrap();
        let python_ast = parse_python_source(source).unwrap();

        assert_ast_eq(python_ast, enderpy_ast);

        // let mut lexer = Lexer::new(source);
        // let enderpy_tokens = lexer.lex();
        // let python_tokens = lex_python_source(source).unwrap();
        // assert_tokens_eq(python_tokens, enderpy_tokens, &lexer);
    }

    fn assert_ast_eq(python_ast: Value, enderpy_ast: Module) {
    }
}
