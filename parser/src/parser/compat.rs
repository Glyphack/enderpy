use miette::{bail, IntoDiagnostic, Result};
use serde_json::Number;
use serde_json::{json, Value};
use std::convert::From;
use std::io::Write;
use std::str::FromStr;

use crate::ast::*;
use crate::runpython::{default_python_path, spawn_python_script_command};
use crate::Parser;

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
    let mut ast =
        serde_json::from_str(String::from_utf8_lossy(&output.stdout).as_ref()).into_diagnostic()?;
    remove_unimplemented_attributes(&mut ast);
    Ok(ast)
}

fn remove_unimplemented_attributes(value: &mut Value) {
    match value {
        Value::Object(map) => {
            // TODO ast_python: Adjust these ignored values as Enderpy adds support.
            map.retain(|key, _| !matches!(key.as_str(), "ctx" | "type_ignores" | "kind"));
            for (_, v) in map.iter_mut() {
                remove_unimplemented_attributes(v);
            }
        }
        Value::Array(vec) => {
            for v in vec.iter_mut() {
                remove_unimplemented_attributes(v);
            }
        }
        _ => {
            // Nothing to do for other value types.
        }
    };
}

fn parse_enderpy_source(source: &str) -> Result<Value> {
    let mut parser = Parser::new(source, "string");
    let typed_ast = parser.parse().into_diagnostic()?;
    let ast = typed_ast.as_python_compat(&parser);
    Ok(ast)
}

trait AsPythonCompat {
    fn as_python_compat(&self, parser: &Parser) -> Value;
}

trait AsNullablePythonCompat<T: AsPythonCompat> {
    fn as_python_compat(&self, parser: &Parser) -> Value;
}

impl<T: AsPythonCompat> AsNullablePythonCompat<T> for Option<T> {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        if let Some(v) = self {
            v.as_python_compat(parser)
        } else {
            json!(null)
        }
    }
}

macro_rules! json_python_compat_node {
    ($name:literal, $instance:ident, $parser:ident, $other_fields:tt) => {{
        let mut node = json!($other_fields);
        let (start_row, start_col, end_row, end_col) =
            $parser.to_row_col($instance.node.start, $instance.node.end);
        node["_type"] = json!($name);
        node["lineno"] = json!(start_row);
        node["col_offset"] = json!(start_col);
        node["end_lineno"] = json!(end_row);
        node["end_col_offset"] = json!(end_col);
        node
    }};
}

impl AsPythonCompat for Module {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json!({
            "_type": "Module",
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for Statement {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        match self {
            Statement::ExpressionStatement(e) => {
                let expr = e.as_python_compat(parser);
                json!({
                    "_type": "Expr",
                    "lineno": expr["lineno"],
                    "col_offset": expr["col_offset"],
                    "end_lineno": expr["end_lineno"],
                    "end_col_offset": expr["end_col_offset"],
                    "value": expr,
                })
            }
            Statement::Import(i) => i.as_python_compat(parser),
            Statement::ImportFrom(i) => i.as_python_compat(parser),
            Statement::AssignStatement(a) => a.as_python_compat(parser),
            Statement::AnnAssignStatement(a) => a.as_python_compat(parser),
            Statement::AugAssignStatement(a) => a.as_python_compat(parser),
            Statement::Assert(a) => a.as_python_compat(parser),
            Statement::Pass(p) => p.as_python_compat(parser),
            Statement::Delete(d) => d.as_python_compat(parser),
            Statement::Return(r) => r.as_python_compat(parser),
            Statement::Raise(r) => r.as_python_compat(parser),
            Statement::Break(b) => b.as_python_compat(parser),
            Statement::Continue(c) => c.as_python_compat(parser),
            Statement::Global(g) => g.as_python_compat(parser),
            Statement::Nonlocal(n) => n.as_python_compat(parser),
            Statement::IfStatement(i) => i.as_python_compat(parser),
            Statement::WhileStatement(w) => w.as_python_compat(parser),
            Statement::ForStatement(f) => f.as_python_compat(parser),
            Statement::WithStatement(w) => w.as_python_compat(parser),
            Statement::TryStatement(t) => t.as_python_compat(parser),
            Statement::TryStarStatement(t) => t.as_python_compat(parser),
            Statement::FunctionDef(f) => f.as_python_compat(parser),
            Statement::ClassDef(c) => c.as_python_compat(parser),
            Statement::Match(m) => m.as_python_compat(parser),
            Statement::AsyncForStatement(f) => f.as_python_compat(parser),
            Statement::AsyncWithStatement(w) => w.as_python_compat(parser),
            Statement::AsyncFunctionDef(f) => f.as_python_compat(parser),
            Statement::TypeAlias(t) => t.as_python_compat(parser),
        }
    }
}

impl AsPythonCompat for Assign {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Assign", self, parser, {
            "targets": self.targets.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "value": self.value.as_python_compat(parser),
            // TODO ast_python: Support for type_comment.
            "type_comment": json!(null),
        })
    }
}

impl AsPythonCompat for AnnAssign {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("AnnAssign", self, parser, {
            "target": self.target.as_python_compat(parser),
            "annotation": self.annotation.as_python_compat(parser),
            "value": self.value.as_python_compat(parser),
            "simple": self.simple,
        })
    }
}

impl AsPythonCompat for AugAssign {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("AugAssign", self, parser, {
            "target": self.target.as_python_compat(parser),
            "op": self.op.as_python_compat(parser),
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for AugAssignOp {
    fn as_python_compat(&self, _: &Parser) -> Value {
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

impl AsPythonCompat for Assert {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Assert", self, parser, {
            "test": self.test.as_python_compat(parser),
            "msg": self.msg.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Pass {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Pass", self, parser, {})
    }
}

impl AsPythonCompat for Delete {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Delete", self, parser, {
            "targets": self.targets.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for Return {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Return", self, parser, {
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Raise {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Raise", self, parser, {
            "exc": self.exc.as_python_compat(parser),
            "cause": self.cause.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Break {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Break", self, parser, {})
    }
}

impl AsPythonCompat for Continue {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Continue", self, parser, {})
    }
}

impl AsPythonCompat for Import {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Import", self, parser, {
            "names": self.names.iter().map(|alias| alias.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for Alias {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Alias", self, parser, {
            "name": self.name,
            "asname": self.asname,
        })
    }
}

impl AsPythonCompat for ImportFrom {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("ImportFrom", self, parser, {
            "module": self.module,
            "names": self.names.iter().map(|alias| alias.as_python_compat(parser)).collect::<Vec<_>>(),
            "level": self.level,
        })
    }
}

impl AsPythonCompat for Global {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Global", self, parser, {
            "names": self.names,
        })
    }
}

impl AsPythonCompat for Nonlocal {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Nonlocal", self, parser, {
            "names": self.names,
        })
    }
}

impl AsPythonCompat for Expression {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        match self {
            Expression::Constant(c) => c.as_python_compat(parser),
            Expression::List(l) => l.as_python_compat(parser),
            Expression::Tuple(t) => t.as_python_compat(parser),
            Expression::Dict(d) => d.as_python_compat(parser),
            Expression::Set(s) => s.as_python_compat(parser),
            Expression::Name(n) => n.as_python_compat(parser),
            Expression::BoolOp(b) => b.as_python_compat(parser),
            Expression::UnaryOp(u) => u.as_python_compat(parser),
            Expression::BinOp(b) => b.as_python_compat(parser),
            Expression::NamedExpr(n) => n.as_python_compat(parser),
            Expression::Yield(y) => y.as_python_compat(parser),
            Expression::YieldFrom(y) => y.as_python_compat(parser),
            Expression::Starred(s) => s.as_python_compat(parser),
            Expression::Generator(g) => g.as_python_compat(parser),
            Expression::ListComp(l) => l.as_python_compat(parser),
            Expression::SetComp(s) => s.as_python_compat(parser),
            Expression::DictComp(d) => d.as_python_compat(parser),
            Expression::Attribute(a) => a.as_python_compat(parser),
            Expression::Subscript(s) => s.as_python_compat(parser),
            Expression::Slice(s) => s.as_python_compat(parser),
            Expression::Call(c) => c.as_python_compat(parser),
            Expression::Await(a) => a.as_python_compat(parser),
            Expression::Compare(c) => c.as_python_compat(parser),
            Expression::Lambda(l) => l.as_python_compat(parser),
            Expression::IfExp(i) => i.as_python_compat(parser),
            Expression::JoinedStr(j) => j.as_python_compat(parser),
            Expression::FormattedValue(f) => f.as_python_compat(parser),
        }
    }
}

impl AsPythonCompat for Name {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Name", self, parser, {
            "id": self.id,
        })
    }
}

impl AsPythonCompat for Constant {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Constant", self, parser, {
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for ConstantValue {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        match self {
            ConstantValue::None => json!(null),
            ConstantValue::Ellipsis => json!("..."),
            ConstantValue::Bool(v) => json!(v),
            ConstantValue::Str(v) => json!(v),
            ConstantValue::Bytes(v) => json!(v),
            ConstantValue::Tuple(v) => json!(v
                .iter()
                .map(|cons| cons.as_python_compat(parser))
                .collect::<Vec<_>>()),
            ConstantValue::Int(v) => Value::Number(Number::from_str(v).unwrap()),
            ConstantValue::Float(v) => Value::Number(Number::from_str(v).unwrap()),
            ConstantValue::Complex {
                real: _real,
                imaginary,
            } => json!(imaginary),
        }
    }
}

impl AsPythonCompat for List {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("List", self, parser, {
            "elts": self.elements.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for Tuple {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Tuple", self, parser, {
            // Yes, these are repeated.
            "dims": self.elements.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "elts": self.elements.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for Dict {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Dict", self, parser, {
            "keys": self.keys.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "values": self.values.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for Set {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Set", self, parser, {
            "elements": self.elements.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for BoolOperation {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("BoolOp", self, parser, {
            "op": self.op.as_python_compat(parser),
            "values": self.values.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for BooleanOperator {
    fn as_python_compat(&self, _: &Parser) -> Value {
        match self {
            BooleanOperator::And => json!({"_type": "And"}),
            BooleanOperator::Or => json!({"_type": "Or"}),
        }
    }
}

impl AsPythonCompat for UnaryOperation {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("UnaryOp", self, parser, {
            "op": self.op.as_python_compat(parser),
            "operand": self.operand.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for UnaryOperator {
    fn as_python_compat(&self, _: &Parser) -> Value {
        match self {
            UnaryOperator::Not => json!({"_type": "Not"}),
            UnaryOperator::Invert => json!({"_type": "Invert"}),
            UnaryOperator::UAdd => json!({"_type": "UAdd"}),
            UnaryOperator::USub => json!({"_type": "USub"}),
        }
    }
}

impl AsPythonCompat for BinOp {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("BinOp", self, parser, {
            "op": self.op.as_python_compat(parser),
            "left": self.left.as_python_compat(parser),
            "right": self.right.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for BinaryOperator {
    fn as_python_compat(&self, _: &Parser) -> Value {
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

impl AsPythonCompat for NamedExpression {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("NamedExpression", self, parser, {
            "target": self.target.as_python_compat(parser),
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Yield {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Yield", self, parser, {
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for YieldFrom {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("YieldFrom", self, parser, {
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Starred {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Starred", self, parser, {
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Generator {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Generator", self, parser, {
            "element": self.element.as_python_compat(parser),
            "generators": self.generators.iter().map(|gen| gen.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for ListComp {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("ListComp", self, parser, {
            "element": self.element.as_python_compat(parser),
            "generators": self.generators.iter().map(|gen| gen.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for SetComp {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("SetComp", self, parser, {
            "element": self.element.as_python_compat(parser),
            "generators": self.generators.iter().map(|gen| gen.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for DictComp {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("DictComp", self, parser, {
            "key": self.key.as_python_compat(parser),
            "value": self.value.as_python_compat(parser),
            "generators": self.generators.iter().map(|gen| gen.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for Comprehension {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Comprehension", self, parser, {
            "target": self.target.as_python_compat(parser),
            "iter": self.iter.as_python_compat(parser),
            "ifs": self.ifs.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "is_async": self.is_async,
        })
    }
}

impl AsPythonCompat for Attribute {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Attribute", self, parser, {
            "value": self.value.as_python_compat(parser),
            "attr": self.attr,
        })
    }
}

impl AsPythonCompat for Subscript {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Subscript", self, parser, {
            "value": self.value.as_python_compat(parser),
            "slice": self.slice.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Slice {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Slice", self, parser, {
            "lower": self.lower.as_python_compat(parser),
            "upper": self.upper.as_python_compat(parser),
            "step": self.step.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Call {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        let mut node = json_python_compat_node!("Call", self, parser, {
            "func": self.func.as_python_compat(parser),
            "args": self.args.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "keywords": self.keywords.iter().map(|kw| kw.as_python_compat(parser)).collect::<Vec<_>>(),
        });
        if let Some(expr) = &self.starargs {
            node["starargs"] = expr.as_python_compat(parser);
        }
        if let Some(expr) = &self.kwargs {
            node["kwargs"] = expr.as_python_compat(parser);
        }
        node
    }
}

impl AsPythonCompat for Keyword {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("keyword", self, parser, {
            "arg": self.arg.as_ref().map_or(json!(null), |s| json!(s)),
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Await {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Await", self, parser, {
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Compare {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Compare", self, parser, {
            "left": self.left.as_python_compat(parser),
            "ops": self.ops.iter().map(|op| op.as_python_compat(parser)).collect::<Vec<_>>(),
            "comparators": self.comparators.iter().map(|op| op.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for ComparisonOperator {
    fn as_python_compat(&self, _: &Parser) -> Value {
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

impl AsPythonCompat for Lambda {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Lambda", self, parser, {
            "args": self.args.as_python_compat(parser),
            "body": self.body.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for crate::ast::Arguments {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json!({
            "_type": "arguments",
            "posonlyargs": self.posonlyargs.iter().map(|arg| arg.as_python_compat(parser)).collect::<Vec<_>>(),
            "args": self.args.iter().map(|arg| arg.as_python_compat(parser)).collect::<Vec<_>>(),
            "vararg": self.vararg.as_python_compat(parser),
            "kwonlyargs": self.kwonlyargs.iter().map(|arg| arg.as_python_compat(parser)).collect::<Vec<_>>(),
            "kw_defaults": self.kw_defaults.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "kwarg": self.kwarg.as_python_compat(parser),
            "defaults": self.defaults.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for Arg {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("arg", self, parser, {
            "arg": self.arg,
            "annotation": self.annotation.as_python_compat(parser),
            // TODO ast_python: Support for type_comment.
            "type_comment": json!(null),
        })
    }
}

impl AsPythonCompat for IfExp {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("IfExp", self, parser, {
            "test": self.test.as_python_compat(parser),
            "body": self.body.as_python_compat(parser),
            "orelse": self.orelse.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for FormattedValue {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("FormattedValue", self, parser, {
            "value": self.value.as_python_compat(parser),
            "conversion": self.conversion,
            "format_spec": self.format_spec.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for JoinedStr {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("JoinedStr", self, parser, {
            "values": self.values.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for If {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("If", self, parser, {
            "test": self.test.as_python_compat(parser),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for While {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("While", self, parser, {
            "test": self.test.as_python_compat(parser),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for For {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("For", self, parser, {
            "target": self.target.as_python_compat(parser),
            "iter": self.iter.as_python_compat(parser),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for AsyncFor {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("AsyncFor", self, parser, {
            "target": self.target.as_python_compat(parser),
            "iter": self.iter.as_python_compat(parser),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for With {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("With", self, parser, {
            "items": self.items.iter().map(|wi| wi.as_python_compat(parser)).collect::<Vec<_>>(),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            // TODO ast_python: Support for type_comment.
            "type_comment": json!(null),
        })
    }
}

impl AsPythonCompat for AsyncWith {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("AsyncWith", self, parser, {
            "items": self.items.iter().map(|wi| wi.as_python_compat(parser)).collect::<Vec<_>>(),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for WithItem {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json!({
            "_type": "withitem",
            "context_expr": self.context_expr.as_python_compat(parser),
            "optional_vars": self.optional_vars.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for Try {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Try", self, parser, {
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "handlers": self.handlers.iter().map(|hndl| hndl.as_python_compat(parser)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "finalbody": self.finalbody.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for TryStar {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("TryStar", self, parser, {
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "handlers": self.handlers.iter().map(|hndl| hndl.as_python_compat(parser)).collect::<Vec<_>>(),
            "orelse": self.orelse.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "finalbody": self.finalbody.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for ExceptHandler {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("ExceptHandler", self, parser, {
            "type": self.typ.as_python_compat(parser),
            "name": self.name.as_ref().map_or(json!(null), |s| json!(s)),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for FunctionDef {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("FunctionDef", self, parser, {
            "name": self.name,
            "args": self.args.as_python_compat(parser),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "decorator_list": self.decorator_list.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "returns": self.returns.as_python_compat(parser),
            "type_comment": self.type_comment.as_ref().map_or(json!(null), |s| json!(s)),
            "type_params": self.type_params.iter().map(|tp| tp.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for AsyncFunctionDef {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("AsyncFunctionDef", self, parser, {
            "name": self.name,
            "args": self.args.as_python_compat(parser),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "decorator_list": self.decorator_list.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "returns": self.returns.as_python_compat(parser),
            "type_comment": self.type_comment.as_ref().map_or(json!(null), |s| json!(s)),
            "type_params": self.type_params.iter().map(|tp| tp.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for ClassDef {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("ClassDef", self, parser, {
            "name": self.name,
            "bases": self.bases.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "keywords": self.keywords.iter().map(|kw| kw.as_python_compat(parser)).collect::<Vec<_>>(),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
            "decorator_list": self.decorator_list.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "type_params": self.type_params.iter().map(|tp| tp.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for Match {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("Match", self, parser, {
            "subject": self.subject.as_python_compat(parser),
            "cases": self.cases.iter().map(|mc| mc.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for MatchCase {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("MatchCase", self, parser, {
            "pattern": self.pattern.as_python_compat(parser),
            "guard": self.guard.as_python_compat(parser),
            "body": self.body.iter().map(|stmt| stmt.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for MatchPattern {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        match self {
            MatchPattern::MatchValue(val) => val.as_python_compat(parser),
            MatchPattern::MatchSingleton(expr) => expr.as_python_compat(parser),
            MatchPattern::MatchSequence(pats) => json!(pats
                .iter()
                .map(|pat| pat.as_python_compat(parser))
                .collect::<Vec<_>>()),
            MatchPattern::MatchStar(expr) => expr.as_python_compat(parser),
            MatchPattern::MatchMapping(map) => map.as_python_compat(parser),
            MatchPattern::MatchAs(mas) => mas.as_python_compat(parser),
            MatchPattern::MatchClass(cls) => cls.as_python_compat(parser),
            MatchPattern::MatchOr(pats) => json!(pats
                .iter()
                .map(|pat| pat.as_python_compat(parser))
                .collect::<Vec<_>>()),
        }
    }
}

impl AsPythonCompat for MatchValue {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("MatchValue", self, parser, {
            "value": self.value.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for MatchAs {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("MatchAs", self, parser, {
            "name": self.name.as_ref().map_or(json!(null), |s| json!(s)),
            "pattern": self.pattern.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for MatchMapping {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("MatchMapping", self, parser, {
            "keys": self.keys.iter().map(|expr| expr.as_python_compat(parser)).collect::<Vec<_>>(),
            "patterns": self.patterns.iter().map(|pat| pat.as_python_compat(parser)).collect::<Vec<_>>(),
            "rest": self.rest.as_ref().map_or(json!(null), |s| json!(s)),
        })
    }
}

impl AsPythonCompat for MatchClass {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("MatchClass", self, parser, {
            "cls": self.cls.as_python_compat(parser),
            "patterns": self.patterns.iter().map(|pat| pat.as_python_compat(parser)).collect::<Vec<_>>(),
            "kwd_attrs": self.kwd_attrs,
            "kwd_patterns": self.kwd_patterns.iter().map(|pat| pat.as_python_compat(parser)).collect::<Vec<_>>(),
        })
    }
}

impl AsPythonCompat for TypeParam {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        match self {
            TypeParam::TypeVar(var) => var.as_python_compat(parser),
            TypeParam::ParamSpec(spec) => spec.as_python_compat(parser),
            TypeParam::TypeVarTuple(tup) => tup.as_python_compat(parser),
        }
    }
}

impl AsPythonCompat for TypeVar {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("TypeVar", self, parser, {
            "name": self.name,
            "bound": self.bound.as_python_compat(parser),
        })
    }
}

impl AsPythonCompat for ParamSpec {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("ParamSpec", self, parser, {
            "name": self.name,
        })
    }
}

impl AsPythonCompat for TypeVarTuple {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("TypeVarTuple", self, parser, {
            "name": self.name,
        })
    }
}

impl AsPythonCompat for TypeAlias {
    fn as_python_compat(&self, parser: &Parser) -> Value {
        json_python_compat_node!("TypeAlias", self, parser, {
            "name": self.name,
            "type_params": self.type_params.iter().map(|tp| tp.as_python_compat(parser)).collect::<Vec<_>>(),
            "value": self.value.as_python_compat(parser),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_enderpy_source, parse_python_source};
    use assert_json_diff::assert_json_matches_no_panic;
    use serde_json::Value;
    use tabled::{
        builder::Builder,
        settings::peaker::PriorityMax,
        settings::{Style, Width},
    };
    use terminal_size::{terminal_size, Width as TerminalWidth};

    #[test]
    fn test_simple_compat() {
        //         let source = r#"
        // def x(a: int) -> int:
        //     return 1 + 1
        // b = x(1)
        // print(b)
        // "#;

        let source = r#"(a
, b, c)
"#;

        let enderpy_ast = parse_enderpy_source(source).unwrap();
        let python_ast = parse_python_source(source).unwrap();
        assert_ast_eq(&python_ast, &enderpy_ast, source);
    }

    fn python_parser_test_ast(inputs: &[&str]) {
        for test_input in inputs.iter() {
            let enderpy_ast = parse_enderpy_source(test_input).unwrap();
            let python_ast = parse_python_source(test_input).unwrap();
            assert_ast_eq(&python_ast, &enderpy_ast, test_input);
        }
    }

    #[test]
    fn test_parse_assignment() {
        python_parser_test_ast(&[
            "a = 1",
            "a = None",
            "a = True",
            "a = False",
            "a = 1j",
            // TODO ast_python: Python does not evaluate bytes.
            // "a = b'1'",
            // "a = rb'1'",
            // "a = br'1'",
            "a = \"a\"",
            "a = '''a'''",
            "a = \"\"\"a\"\"\"",
            "a = 'a'",
            "a = 1, 2",
            "a = 1, 2, ",
            "a = b = 1",
            "a,b = c,d = 1,2",
            // augmented assignment
            "a += 1",
            "a -= 1",
            "a *= 1",
            "a /= 1",
            "a //= 1",
            "a %= 1",
            "a **= 1",
            "a <<= 1",
            "a >>= 1",
            "a &= 1",
            "a ^= 1",
            "a |= 1",
            // annotated assignment
        ]);
    }

    #[test]
    fn test_parse_assert_stmt() {
        python_parser_test_ast(&["assert a", "assert a, b", "assert True, 'fancy message'"]);
    }

    #[test]
    fn test_pass_stmt() {
        python_parser_test_ast(&["pass", "pass ", "pass\n"]);
    }

    #[test]
    fn test_parse_del_stmt() {
        python_parser_test_ast(&["del a", "del a, b", "del a, b, "]);
    }

    #[test]
    fn parse_yield_statement() {
        python_parser_test_ast(&["yield", "yield a", "yield a, b", "yield a, b, "]);
    }

    #[test]
    fn test_raise_statement() {
        python_parser_test_ast(&["raise", "raise a", "raise a from c"]);
    }

    #[test]
    fn test_parse_break_continue() {
        python_parser_test_ast(&["break", "continue"]);
    }

    #[test]
    fn test_parse_bool_op() {
        python_parser_test_ast(&[
            "a or b",
            "a and b",
            // TODO ast_python: Python parses this as a BoolOp with 3 values.
            // i.e. {"op": "or", "values": ["a", "b", "c"]}
            // Enderpy parses this as a nested set of BoolOps.
            // i.e. {"op": "or", "values": ["a", {"op": "or", "values": ["b", "c"]}]}
            // I'm not sure which is correct.
            // "a or b or c",
            "a and b or c",
        ]);
    }

    #[test]
    fn test_parse_unary_op() {
        python_parser_test_ast(&["not a", "+ a", "~ a", "-a"]);
    }

    #[test]
    fn test_named_expression() {
        // TODO ast_python: Enderpy chokes on this.
        // python_parser_test_ast(&["(a := b)"]);
    }

    #[test]
    fn test_tuple() {
        python_parser_test_ast(&[
            "(a, b, c)",
            // TODO ast_python: Enderpy doesn't handle newlines within a nested context.
            "(a,
            b, c)",
            "(a
            , b, c)",
            // "(a,
            // b,
            //     c)",
            //             "(a,
            // )",
            "(a, b, c,)",
        ]);
    }

    #[test]
    fn test_yield_expression() {
        python_parser_test_ast(&["yield", "yield a", "yield from a"]);
    }

    #[test]
    fn test_starred() {
        // TODO ast_python: Enderpy chokes on this.
        // python_parser_test_ast(&["(*a)"]);
    }

    #[test]
    fn test_await_expression() {
        python_parser_test_ast(&["await a"]);
    }

    #[test]
    fn test_attribute_ref() {
        python_parser_test_ast(&["a.b", "a.b.c", "a.b_c", "a.b.c.d"]);
    }
    #[test]
    fn test_subscript() {
        python_parser_test_ast(&["a[1]", "a.b[1]"]);
    }

    #[test]
    fn parse_call() {
        python_parser_test_ast(&[
            "a()",
            "a(b)",
            "a(b, c)",
            "func(b=c)",
            "func(a, b=c, d=e)",
            "func(a, b=c, d=e, *f)",
            "func(a, b=c, d=e, *f, **g)",
            "func(a,)",
        ]);
    }

    #[test]
    fn test_lambda() {
        python_parser_test_ast(&[
            "lambda: a",
            "lambda a: a",
            "lambda a, b: a",
            "lambda a, b, c: a",
            "lambda a, *b: a",
            "lambda a, *b, c: a",
            "lambda a, *b, c, **d: a",
            "lambda a=1 : a",
            "lambda a=1 : a,",
        ]);
    }

    #[test]
    fn test_conditional_expression() {
        python_parser_test_ast(&["a if b else c if d else e"]);
    }

    #[test]
    fn test_string_literal_concatenation() {
        python_parser_test_ast(&[
            "'a' 'b'",
            // TODO ast_python: Python evaluates this as "ab".
            // "b'a' b'b'",
            "'a'   'b'",
            // TODO ast_python: Enderpy evaluates this as 'r"a"b'. This seems wrong.
            // "r'a' 'b'",
            // TODO ast_python: Enderpy doesn't handle newlines within a nested context.
            // "('a'
            // 'b')",
            // "('a'
            // 'b', 'c')",
            //             "('a'
            //                 'b'
            // 'c')",
            // TODO ast_python: Python evaluates this as "ac". Enderpy creates 2 constants.
            // "f'a' 'c'",
            // TODO ast_python: Python evaluates this as "abc". Enderpy creates 3 constants.
            // "f'a' 'b' 'c'",
            // TODO ast_python: Python evaluates this as "dab". Enderpy creates 3 constants.
            // "'d' f'a' 'b'",
            "f'a_{1}' 'b' ",
        ]);
    }

    #[test]
    fn test_fstring() {
        python_parser_test_ast(&[
            "f'a'",
            "f'hello_{a}'",
            "f'hello_{a} {b}'",
            "f'hello_{a} {b} {c}'",
            // unsupported
            // "f'hello_{f'''{a}'''}'",
        ]);
    }

    #[test]
    fn test_comparison() {
        python_parser_test_ast(&[
            "a == b",
            "a != b",
            "a > b",
            "a < b",
            "a >= b",
            "a <= b",
            "a is b",
            "a is not b",
            "a in b",
            "a not in b",
            "a < b < c",
        ]);
    }

    #[test]
    fn test_while_statement() {
        python_parser_test_ast(&[
            "while a: pass",
            "while a:
    pass",
            "while a:
        a = 1
else:
        b = 1
",
        ]);
    }

    #[test]
    fn test_try_statement() {
        python_parser_test_ast(&[
            "try:
    pass
except:
    pass",
            "try:
                pass
except Exception:
                pass",
            "try:
                pass
except Exception as e:
                pass",
            "try:
                pass
except Exception as e:
                pass
else:
                pass",
            "try:
                pass
except Exception as e:
                pass
else:
                pass
finally:
                pass",
            "try:
    pass
except *Exception as e:
    pass
",
        ]);
    }

    #[test]
    fn test_ellipsis_statement() {
        python_parser_test_ast(&[
            "def a(): ...",
            "def a():
    ...",
            "a = ...",
            "... + 1",
        ]);
    }

    macro_rules! parser_test {
        ($test_name:ident, $test_file:expr) => {
            #[test]
            fn $test_name() {
                let test_case = std::fs::read_to_string($test_file).unwrap();
                python_parser_test_ast(&[test_case.as_str()]);
            }
        };
    }

    // parser_test!(test_functions, "test_data/inputs/functions.py");
    // parser_test!(test_if, "test_data/inputs/if.py");
    // parser_test!(test_indentation, "test_data/inputs/indentation.py");
    // parser_test!(
    //     test_separate_statements,
    //     "test_data/inputs/separate_statements.py"
    // );
    // parser_test!(test_try, "test_data/inputs/try.py");
    // parser_test!(
    //     annotated_assignment,
    //     "test_data/inputs/annotated_assignment.py"
    // );
    // parser_test!(binary_op, "test_data/inputs/binary_op.py");
    // parser_test!(class, "test_data/inputs/class.py");
    // parser_test!(dict, "test_data/inputs/dict.py");
    // parser_test!(test_for, "test_data/inputs/for.py");
    // parser_test!(from_import, "test_data/inputs/from_import.py");
    // parser_test!(function_def, "test_data/inputs/function_def.py");
    // parser_test!(
    //     generator_expressions,
    //     "test_data/inputs/generator_expressions.py"
    // );
    // parser_test!(lists, "test_data/inputs/lists.py");
    // parser_test!(test_match, "test_data/inputs/match.py");
    // parser_test!(sets, "test_data/inputs/sets.py");
    // parser_test!(string, "test_data/inputs/string.py");
    // parser_test!(subscript, "test_data/inputs/subscript.py");
    // parser_test!(with, "test_data/inputs/with.py");
    // parser_test!(newlines, "test_data/inputs/newlines.py");
    // parser_test!(comments, "test_data/inputs/comments.py");
    // parser_test!(types_alias, "test_data/inputs/type_alias.py");

    fn assert_ast_eq(python_ast: &Value, enderpy_ast: &Value, source: &str) {
        let include_source = std::env::var("INCLUDE_SOURCE").is_ok();
        let side_by_side = std::env::var("SIDE_BY_SIDE").is_ok();

        let formatted_source = if include_source {
            format!("\nSource:\n{}\n", source)
        } else {
            "".to_string()
        };
        if !side_by_side {
            pretty_assertions::assert_eq!(
                &python_ast,
                &enderpy_ast,
                "Enderpy AST does not match Python AST.\n{}\x1b[31mPython AST\x1b[0m / \x1b[32mEnderpy AST\x1b[0m",
                formatted_source,
            );
        } else if let Err(message) = assert_json_matches_no_panic(
            &python_ast,
            &enderpy_ast,
            assert_json_diff::Config::new(assert_json_diff::CompareMode::Strict),
        ) {
            let mut table_builder = Builder::default();
            table_builder.push_record(["Python AST", "Enderpy AST"]);
            table_builder.push_record([
                serde_json::to_string_pretty(&python_ast).unwrap(),
                serde_json::to_string_pretty(&enderpy_ast).unwrap(),
            ]);
            let mut table = table_builder.build();
            table.with(Style::modern());
            // If run in a terminal, don't expand table beyond terminal width.
            if let Some((TerminalWidth(width), _)) = terminal_size() {
                table
                    .with(
                        Width::wrap(width as usize)
                            .keep_words()
                            .priority::<PriorityMax>(),
                    )
                    .with(Width::increase(width as usize));
            }
            panic!(
                "Enderpy AST does not match Python AST.\n{}{}\n{}",
                formatted_source, table, message
            );
        }
    }
}
