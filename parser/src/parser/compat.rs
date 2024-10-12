#![allow(unused_variables)]
use crate::ast::*;
use crate::parser::parser::Parser;
use serde_json::{json, Value};

pub trait AsPythonCompat {
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
        // TODO: (offset_compat)
        // node["lineno"] = json!(start_row);
        // node["col_offset"] = json!(start_col);
        // node["end_lineno"] = json!(end_row);
        // node["end_col_offset"] = json!(end_col);
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
                    // TODO: (offset_compat) python parser and enderpy start/end offsets are different
                    // Fix it when have some time
                    // "lineno": expr["lineno"],
                    // "col_offset": expr["col_offset"],
                    // "end_lineno": expr["end_lineno"],
                    // "end_col_offset": expr["end_col_offset"],
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
            Statement::ReturnStmt(r) => r.as_python_compat(parser),
            Statement::Raise(r) => r.as_python_compat(parser),
            Statement::BreakStmt(b) => b.as_python_compat(parser),
            Statement::ContinueStmt(c) => c.as_python_compat(parser),
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
            Statement::MatchStmt(m) => m.as_python_compat(parser),
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
        json_python_compat_node!("alias", self, parser, {
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
            "value": json!(self.get_value(parser.source)),
        })
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
        json_python_compat_node!("NamedExpr", self, parser, {
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
