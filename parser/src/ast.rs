use is_macro::Is;
use std::fmt::{self};
use std::sync::Arc;

use miette::{SourceOffset, SourceSpan};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)] // #[serde(tag = "type")]
pub struct Node {
    /// Start offset in source
    pub start: u32,

    /// End offset in source
    pub end: u32,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)] // #[serde(tag = "type")]
pub struct TextRange {
    pub start: u32,
    pub end: u32,
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}

impl Node {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub trait GetNode {
    fn get_node(&self) -> Node;
}

impl From<Node> for SourceSpan {
    fn from(val: Node) -> Self {
        Self::new(SourceOffset::from(val.start as usize), val.len() as usize)
    }
}

// The following structs are used to represent the AST
// https://docs.python.org/3/library/ast.html#abstract-grammar
#[derive(Debug, Clone)]
pub struct Module {
    pub node: Node,
    pub body: Vec<Statement>,
}

// Use box to reduce the enum size
#[derive(Debug, Clone)]
pub enum Statement {
    AssignStatement(Box<Assign>),
    AnnAssignStatement(Box<AnnAssign>),
    AugAssignStatement(Box<AugAssign>),
    ExpressionStatement(Box<Expression>),
    Assert(Box<Assert>),
    Pass(Box<Pass>),
    Delete(Box<Delete>),
    Return(Box<Return>),
    Raise(Box<Raise>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Import(Box<Import>),
    ImportFrom(Box<ImportFrom>),
    Global(Box<Global>),
    Nonlocal(Box<Nonlocal>),
    IfStatement(Box<If>),
    WhileStatement(Box<While>),
    ForStatement(Box<For>),
    AsyncForStatement(Box<AsyncFor>),
    WithStatement(Box<With>),
    AsyncWithStatement(Box<AsyncWith>),
    TryStatement(Box<Try>),
    TryStarStatement(Box<TryStar>),
    FunctionDef(Arc<FunctionDef>),
    AsyncFunctionDef(Arc<AsyncFunctionDef>),
    ClassDef(Arc<ClassDef>),
    Match(Box<Match>),
    TypeAlias(Box<TypeAlias>),
}

impl GetNode for Statement {
    fn get_node(&self) -> Node {
        match self {
            Statement::AssignStatement(s) => s.node,
            Statement::AnnAssignStatement(s) => s.node,
            Statement::AugAssignStatement(s) => s.node,
            Statement::ExpressionStatement(s) => s.get_node(),
            Statement::Assert(s) => s.node,
            Statement::Pass(s) => s.node,
            Statement::Delete(s) => s.node,
            Statement::Return(s) => s.node,
            Statement::Raise(s) => s.node,
            Statement::Break(s) => s.node,
            Statement::Continue(s) => s.node,
            Statement::Import(s) => s.node,
            Statement::ImportFrom(s) => s.node,
            Statement::Global(s) => s.node,
            Statement::Nonlocal(s) => s.node,
            Statement::IfStatement(s) => s.node,
            Statement::WhileStatement(s) => s.node,
            Statement::ForStatement(s) => s.node,
            Statement::AsyncForStatement(s) => s.node,
            Statement::WithStatement(s) => s.node,
            Statement::AsyncWithStatement(s) => s.node,
            Statement::TryStatement(s) => s.node,
            Statement::TryStarStatement(s) => s.node,
            Statement::FunctionDef(s) => s.node,
            Statement::AsyncFunctionDef(s) => s.node,
            Statement::ClassDef(s) => s.node,
            Statement::Match(s) => s.node,
            Statement::TypeAlias(s) => s.node,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub node: Node,
    pub targets: Vec<Expression>,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct AnnAssign {
    pub node: Node,
    pub target: Expression,
    pub annotation: Expression,
    pub value: Option<Expression>,
    pub simple: bool,
}

#[derive(Debug, Clone)]
pub struct AugAssign {
    pub node: Node,
    pub target: Expression,
    pub op: AugAssignOp,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum AugAssignOp {
    Add,
    Sub,
    Mult,
    MatMult,
    Div,
    Mod,
    Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
    FloorDiv,
}

#[derive(Debug, Clone)]
pub struct Assert {
    pub node: Node,
    pub test: Expression,
    pub msg: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Pass {
    pub node: Node,
}

#[derive(Debug, Clone)]
pub struct Delete {
    pub node: Node,
    pub targets: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub node: Node,
    pub value: Option<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Raise
#[derive(Debug, Clone)]
pub struct Raise {
    pub node: Node,
    pub exc: Option<Expression>,
    pub cause: Option<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Break
#[derive(Debug, Clone)]
pub struct Break {
    pub node: Node,
}

// https://docs.python.org/3/library/ast.html#ast.Continue
#[derive(Debug, Clone)]
pub struct Continue {
    pub node: Node,
}

// https://docs.python.org/3/library/ast.html#ast.Import
#[derive(Debug, Clone)]
pub struct Import {
    pub node: Node,
    pub names: Vec<Alias>,
}

// https://docs.python.org/3/library/ast.html#ast.alias
#[derive(Debug, Clone)]
pub struct Alias {
    pub node: Node,
    pub name: String,
    pub asname: Option<String>,
}

impl Alias {
    pub fn name(&self) -> String {
        if let Some(asname) = &self.asname {
            asname.clone()
        } else {
            self.name.clone()
        }
    }
}

// https://docs.python.org/3/library/ast.html#ast.ImportFrom
#[derive(Debug, Clone)]
pub struct ImportFrom {
    pub node: Node,
    pub module: String,
    pub names: Vec<Alias>,
    pub level: usize,
}

// https://docs.python.org/3/library/ast.html#ast.Global
#[derive(Debug, Clone)]
pub struct Global {
    pub node: Node,
    pub names: Vec<String>,
}

// https://docs.python.org/3/library/ast.html#ast.Nonlocal
#[derive(Debug, Clone)]
pub struct Nonlocal {
    pub node: Node,
    pub names: Vec<String>,
}

#[derive(Debug, Clone, Is)]
pub enum Expression {
    Constant(Box<Constant>),
    List(Box<List>),
    Tuple(Box<Tuple>),
    Dict(Box<Dict>),
    Set(Box<Set>),
    Name(Box<Name>),
    BoolOp(Box<BoolOperation>),
    UnaryOp(Box<UnaryOperation>),
    BinOp(Box<BinOp>),
    NamedExpr(Box<NamedExpression>),
    #[is(name = "yield_expr")]
    Yield(Box<Yield>),
    YieldFrom(Box<YieldFrom>),
    Starred(Box<Starred>),
    Generator(Box<Generator>),
    ListComp(Box<ListComp>),
    SetComp(Box<SetComp>),
    DictComp(Box<DictComp>),
    Attribute(Box<Attribute>),
    Subscript(Box<Subscript>),
    Slice(Box<Slice>),
    Call(Box<Call>),
    #[is(name = "await_expr")]
    Await(Box<Await>),
    Compare(Box<Compare>),
    Lambda(Box<Lambda>),
    IfExp(Box<IfExp>),
    JoinedStr(Box<JoinedStr>),
    FormattedValue(Box<FormattedValue>),
}

impl GetNode for Expression {
    fn get_node(&self) -> Node {
        match self {
            Expression::Constant(c) => c.node,
            Expression::List(l) => l.node,
            Expression::Tuple(t) => t.node,
            Expression::Dict(d) => d.node,
            Expression::Set(s) => s.node,
            Expression::Name(n) => n.node,
            Expression::BoolOp(b) => b.node,
            Expression::UnaryOp(u) => u.node,
            Expression::BinOp(b) => b.node,
            Expression::NamedExpr(n) => n.node,
            Expression::Yield(y) => y.node,
            Expression::YieldFrom(y) => y.node,
            Expression::Starred(s) => s.node,
            Expression::Generator(g) => g.node,
            Expression::ListComp(l) => l.node,
            Expression::SetComp(s) => s.node,
            Expression::DictComp(d) => d.node,
            Expression::Attribute(a) => a.node,
            Expression::Subscript(s) => s.node,
            Expression::Slice(s) => s.node,
            Expression::Call(c) => c.node,
            Expression::Await(a) => a.node,
            Expression::Compare(c) => c.node,
            Expression::Lambda(l) => l.node,
            Expression::IfExp(i) => i.node,
            Expression::JoinedStr(j) => j.node,
            Expression::FormattedValue(f) => f.node,
        }
    }
}

// https://docs.python.org/3/reference/expressions.html#atom-identifiers
#[derive(Clone)]
pub struct Name {
    pub node: Node,
    pub id: String,
    pub parenthesized: bool,
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Name")
            .field("node", &self.node)
            .field("id", &self.id)
            .finish()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constant {
    pub node: Node,
    pub value: ConstantValue,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ConstantValue {
    None,
    Ellipsis,
    Bool,
    Str,
    Bytes,
    Tuple,
    // Numbers are string because we don't care about the value rn.
    Int,
    Float,
    Complex,
}

impl Constant {
    pub fn get_value<'a>(&self, source: &'a str) -> &'a str {
        return &source[self.node.start as usize..self.node.end as usize];
    }
}

#[derive(Debug, Clone)]
pub struct List {
    pub node: Node,
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub node: Node,
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Dict {
    pub node: Node,
    pub keys: Vec<Expression>,
    pub values: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub node: Node,
    pub elements: Vec<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.BoolOp
#[derive(Debug, Clone)]
pub struct BoolOperation {
    pub node: Node,
    pub op: BooleanOperator,
    pub values: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum BooleanOperator {
    And,
    Or,
}

// https://docs.python.org/3/library/ast.html#ast.UnaryOp
#[derive(Debug, Clone)]
pub struct UnaryOperation {
    pub node: Node,
    pub op: UnaryOperator,
    pub operand: Expression,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Invert,
    UAdd,
    USub,
}

// https://docs.python.org/3/library/ast.html#ast.BinOp
#[derive(Debug, Clone)]
pub struct BinOp {
    pub node: Node,
    pub op: BinaryOperator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    MatMult,
    Div,
    Mod,
    Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
    FloorDiv,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let op_str = match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mult => "*",
            BinaryOperator::MatMult => "@",
            BinaryOperator::Div => "/",
            BinaryOperator::Mod => "%",
            BinaryOperator::Pow => "**",
            BinaryOperator::LShift => "<<",
            BinaryOperator::RShift => ">>",
            BinaryOperator::BitOr => "|",
            BinaryOperator::BitXor => "^",
            BinaryOperator::BitAnd => "&",
            BinaryOperator::FloorDiv => "//",
        };

        write!(f, "{}", op_str)
    }
}

// https://docs.python.org/3/library/ast.html#ast.NamedExpr
#[derive(Debug, Clone)]
pub struct NamedExpression {
    pub node: Node,
    pub target: Expression,
    pub value: Expression,
}

// https://docs.python.org/3/library/ast.html#ast.Yield
#[derive(Debug, Clone)]
pub struct Yield {
    pub node: Node,
    pub value: Option<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.YieldFrom
#[derive(Debug, Clone)]
pub struct YieldFrom {
    pub node: Node,
    pub value: Expression,
}

// https://docs.python.org/3/library/ast.html#ast.Starred
#[derive(Debug, Clone)]
pub struct Starred {
    pub node: Node,
    pub value: Expression,
}

// https://docs.python.org/3/library/ast.html#ast.GeneratorExp
#[derive(Debug, Clone)]
pub struct Generator {
    pub node: Node,
    pub element: Expression,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug, Clone)]
pub struct ListComp {
    pub node: Node,
    pub element: Expression,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug, Clone)]
pub struct SetComp {
    pub node: Node,
    pub element: Expression,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug, Clone)]
pub struct DictComp {
    pub node: Node,
    pub key: Expression,
    pub value: Expression,
    pub generators: Vec<Comprehension>,
}

// https://docs.python.org/3/library/ast.html#ast.comprehension
#[derive(Debug, Clone)]
pub struct Comprehension {
    pub node: Node,
    pub target: Expression,
    pub iter: Expression,
    pub ifs: Vec<Expression>,
    pub is_async: bool,
}

// https://docs.python.org/3/library/ast.html#ast.Attribute
#[derive(Debug, Clone)]
pub struct Attribute {
    pub node: Node,
    /// The x in x.y
    pub value: Expression,
    /// The y in x.y
    pub attr: String,
}

// https://docs.python.org/3/library/ast.html#ast.Subscript
#[derive(Debug, Clone)]
pub struct Subscript {
    pub node: Node,
    pub value: Expression,
    pub slice: Expression,
}

// https://docs.python.org/3/library/ast.html#ast.Slice
// can be used for Subscript
#[derive(Debug, Clone)]
pub struct Slice {
    pub node: Node,
    pub lower: Option<Expression>,
    pub upper: Option<Expression>,
    pub step: Option<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Call
#[derive(Debug, Clone)]
pub struct Call {
    pub node: Node,
    pub func: Expression,
    pub args: Vec<Expression>,
    pub keywords: Vec<Keyword>,
    pub starargs: Option<Expression>,
    pub kwargs: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Keyword {
    pub node: Node,
    pub arg: Option<String>,
    pub value: Expression,
}

// https://docs.python.org/3/library/ast.html#ast.Await
#[derive(Debug, Clone)]
pub struct Await {
    pub node: Node,
    pub value: Expression,
}

// https://docs.python.org/3/library/ast.html#ast.Compare
#[derive(Debug, Clone)]
pub struct Compare {
    pub node: Node,
    pub left: Expression,
    pub ops: Vec<ComparisonOperator>,
    pub comparators: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum ComparisonOperator {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
    Is,
    IsNot,
    In,
    NotIn,
}

// https://docs.python.org/3/library/ast.html#ast.Lambda
#[derive(Debug, Clone)]
pub struct Lambda {
    pub node: Node,
    pub args: Arguments,
    pub body: Expression,
}

// https://docs.python.org/3/library/ast.html#ast.arguments
#[derive(Debug, Clone)]
pub struct Arguments {
    pub node: Node,
    pub posonlyargs: Vec<Arg>,
    pub args: Vec<Arg>,
    pub vararg: Option<Arg>,
    pub kwonlyargs: Vec<Arg>,
    pub kw_defaults: Vec<Option<Expression>>,
    pub kwarg: Option<Arg>,
    pub defaults: Vec<Expression>,
}

impl Arguments {
    pub fn len(&self) -> usize {
        self.posonlyargs.len()
            + self.args.len()
            + self.kwonlyargs.len()
            + if self.vararg.is_some() { 1 } else { 0 }
            + if self.kwarg.is_some() { 1 } else { 0 }
    }
}

impl IntoIterator for Arguments {
    type Item = Arg;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        let mut args = self.posonlyargs;
        args.extend(self.args);
        if let Some(vararg) = self.vararg {
            args.push(vararg);
        }
        args.extend(self.kwonlyargs);
        args.into_iter()
    }
}

impl std::fmt::Display for Arguments {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut args = vec![];

        for arg in &self.args {
            args.push(arg.arg.clone());
        }

        for arg in &self.kwonlyargs {
            args.push(arg.arg.clone());
        }

        if let Some(vararg) = &self.vararg {
            args.push(vararg.arg.clone());
        }

        if let Some(kwarg) = &self.kwarg {
            args.push(kwarg.arg.clone());
        }

        write!(f, "({})", args.join(", "))
    }
}

// https://docs.python.org/3/library/ast.html#ast.arg
#[derive(Debug, Clone)]
pub struct Arg {
    pub node: Node,
    pub arg: String,
    pub annotation: Option<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.IfExp
#[derive(Debug, Clone)]
pub struct IfExp {
    pub node: Node,
    pub test: Expression,
    pub body: Expression,
    pub orelse: Expression,
}

// https://docs.python.org/3/library/ast.html#ast.FormattedValue
#[derive(Debug, Clone)]
pub struct FormattedValue {
    pub node: Node,
    pub value: Expression,
    pub conversion: i32,
    pub format_spec: Option<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.JoinedStr
#[derive(Debug, Clone)]
pub struct JoinedStr {
    pub node: Node,
    pub values: Vec<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.If
#[derive(Debug, Clone)]
pub struct If {
    pub node: Node,
    pub test: Expression,
    pub body: Vec<Statement>,
    pub orelse: Vec<Statement>,
}

impl If {
    pub fn update_orelse(&mut self, other_or_else: Vec<Statement>) {
        self.orelse = other_or_else;
    }
}

// https://docs.python.org/3/library/ast.html#ast.While
#[derive(Debug, Clone)]
pub struct While {
    pub node: Node,
    pub test: Expression,
    pub body: Vec<Statement>,
    pub orelse: Vec<Statement>,
}

// https://docs.python.org/3/library/ast.html#ast.For
#[derive(Debug, Clone)]
pub struct For {
    pub node: Node,
    pub target: Expression,
    pub iter: Expression,
    pub body: Vec<Statement>,
    pub orelse: Vec<Statement>,
}

// https://docs.python.org/3/library/ast.html#ast.AsyncFor
#[derive(Debug, Clone)]
pub struct AsyncFor {
    pub node: Node,
    pub target: Expression,
    pub iter: Expression,
    pub body: Vec<Statement>,
    pub orelse: Vec<Statement>,
}

// https://docs.python.org/3/library/ast.html#ast.With
#[derive(Debug, Clone)]
pub struct With {
    pub node: Node,
    pub items: Vec<WithItem>,
    pub body: Vec<Statement>,
}

// https://docs.python.org/3/library/ast.html#ast.AsyncWith
#[derive(Debug, Clone)]
pub struct AsyncWith {
    pub node: Node,
    pub items: Vec<WithItem>,
    pub body: Vec<Statement>,
}

// https://docs.python.org/3/library/ast.html#ast.withitem
// can be used for With
#[derive(Debug, Clone)]
pub struct WithItem {
    pub node: Node,
    pub context_expr: Expression,
    pub optional_vars: Option<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Try
#[derive(Debug, Clone)]
pub struct Try {
    pub node: Node,
    pub body: Vec<Statement>,
    pub handlers: Vec<ExceptHandler>,
    pub orelse: Vec<Statement>,
    pub finalbody: Vec<Statement>,
}

// https://docs.python.org/3/library/ast.html#ast.TryStar
#[derive(Debug, Clone)]
pub struct TryStar {
    pub node: Node,
    pub body: Vec<Statement>,
    pub handlers: Vec<ExceptHandler>,
    pub orelse: Vec<Statement>,
    pub finalbody: Vec<Statement>,
}

// https://docs.python.org/3/library/ast.html#ast.ExceptHandler
#[derive(Debug, Clone)]
pub struct ExceptHandler {
    pub node: Node,
    pub typ: Option<Expression>,
    pub name: Option<String>,
    pub body: Vec<Statement>,
}

// https://docs.python.org/3/library/ast.html#functiondef
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub node: Node,
    pub name: String,
    pub args: Arguments,
    pub body: Vec<Statement>,
    pub decorator_list: Vec<Expression>,
    pub returns: Option<Expression>,
    pub type_comment: Option<String>,
    pub type_params: Vec<TypeParam>,
}

// https://docs.python.org/3/library/ast.html#ast.AsyncFunctionDef
#[derive(Debug, Clone)]
pub struct AsyncFunctionDef {
    pub node: Node,
    pub name: String,
    pub args: Arguments,
    pub body: Vec<Statement>,
    pub decorator_list: Vec<Expression>,
    pub returns: Option<Expression>,
    pub type_comment: Option<String>,
    pub type_params: Vec<TypeParam>,
}

impl AsyncFunctionDef {
    pub fn to_function_def(&self) -> FunctionDef {
        FunctionDef {
            node: self.node,
            name: self.name.clone(),
            args: self.args.clone(),
            body: self.body.clone(),
            decorator_list: self.decorator_list.clone(),
            returns: self.returns.clone(),
            type_comment: self.type_comment.clone(),
            type_params: self.type_params.clone(),
        }
    }
}

// https://docs.python.org/3/library/ast.html#ast.ClassDef
#[derive(Debug, Clone)]
pub struct ClassDef {
    pub node: Node,
    pub name: String,
    pub bases: Vec<Expression>,
    pub keywords: Vec<Keyword>,
    pub body: Vec<Statement>,
    pub decorator_list: Vec<Expression>,
    pub type_params: Vec<TypeParam>,
}

// https://docs.python.org/3/library/ast.html#ast.Match
#[derive(Debug, Clone)]
pub struct Match {
    pub node: Node,
    pub subject: Expression,
    pub cases: Vec<MatchCase>,
}

// https://docs.python.org/3/library/ast.html#ast.match_case
#[derive(Debug, Clone)]
pub struct MatchCase {
    pub node: Node,
    pub pattern: MatchPattern,
    pub guard: Option<Expression>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
    MatchValue(MatchValue),
    MatchSingleton(Expression),
    MatchSequence(Vec<MatchPattern>),
    MatchStar(Expression),
    MatchMapping(MatchMapping),
    MatchAs(Box<MatchAs>),
    MatchClass(MatchClass),
    MatchOr(Vec<MatchPattern>),
}

#[derive(Debug, Clone)]
pub struct MatchValue {
    pub node: Node,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct MatchAs {
    pub node: Node,
    pub name: Option<String>,
    pub pattern: Option<MatchPattern>,
}

#[derive(Debug, Clone)]
pub struct MatchMapping {
    pub node: Node,
    pub keys: Vec<Expression>,
    pub patterns: Vec<MatchPattern>,
    pub rest: Option<String>,
}

#[derive(Debug, Clone)]
pub struct MatchClass {
    pub node: Node,
    pub cls: Expression,
    pub patterns: Vec<MatchPattern>,
    pub kwd_attrs: Vec<String>,
    pub kwd_patterns: Vec<MatchPattern>,
}

// https://docs.python.org/3/library/ast.html#ast-type-params
#[derive(Debug, Clone)]
pub enum TypeParam {
    TypeVar(TypeVar),
    ParamSpec(ParamSpec),
    TypeVarTuple(TypeVarTuple),
}

impl GetNode for TypeParam {
    fn get_node(&self) -> Node {
        match self {
            TypeParam::TypeVar(t) => t.node,
            TypeParam::ParamSpec(p) => p.node,
            TypeParam::TypeVarTuple(t) => t.node,
        }
    }
}

impl TypeParam {
    pub fn get_name(&self) -> String {
        match self {
            TypeParam::TypeVar(t) => t.name.clone(),
            TypeParam::ParamSpec(p) => p.name.clone(),
            TypeParam::TypeVarTuple(t) => t.name.clone(),
        }
    }
}

// https://docs.python.org/3/library/ast.html#ast.TypeVar
#[derive(Debug, Clone)]
pub struct TypeVar {
    pub node: Node,
    pub name: String,
    pub bound: Option<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.ParamSpec
#[derive(Debug, Clone)]
pub struct ParamSpec {
    pub node: Node,
    pub name: String,
}

// https://docs.python.org/3/library/ast.html#ast.TypeVarTuple
#[derive(Debug, Clone)]
pub struct TypeVarTuple {
    pub node: Node,
    pub name: String,
}

// https://docs.python.org/3/library/ast.html#ast.TypeAlias
#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub node: Node,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub value: Expression,
}

#[cfg(target_pointer_width = "64")]
#[test]
fn no_bloat_enum_sizes() {
    use crate::ast::*;
    use std::mem::size_of;
    assert_eq!(size_of::<Statement>(), 16);
    assert_eq!(size_of::<Expression>(), 16);
}
