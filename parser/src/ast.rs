use is_macro::Is;
use std::borrow::Cow;
use std::fmt::{self};
use std::sync::Arc;

use miette::{SourceOffset, SourceSpan};

use crate::intern::StrId;

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
#[derive(Debug, Clone, Is)]
pub enum Statement {
    AssignStatement(Box<Assign>),
    AnnAssignStatement(Box<AnnAssign>),
    AugAssignStatement(Box<AugAssign>),
    ExpressionStatement(Box<Expression>),
    Assert(Box<Assert>),
    Pass(Box<Pass>),
    Delete(Box<Delete>),
    ReturnStmt(Box<Return>),
    Raise(Box<Raise>),
    BreakStmt(Box<Break>),
    ContinueStmt(Box<Continue>),
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
    MatchStmt(Box<Match>),
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
            Statement::ReturnStmt(s) => s.node,
            Statement::Raise(s) => s.node,
            Statement::BreakStmt(s) => s.node,
            Statement::ContinueStmt(s) => s.node,
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
            Statement::MatchStmt(s) => s.node,
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

impl Name {
    pub fn get_value<'a>(&self, source: &'a str) -> &'a str {
        &source[(self.node.start) as usize..(self.node.end) as usize]
    }
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

impl Constant {
    pub fn get_value<'a>(&self, source: &'a str) -> Cow<'a, str> {
        match &self.value {
            ConstantValue::Str(quote_type) => match quote_type {
                QuoteType::Single => Cow::Borrowed(
                    &source[(self.node.start + 1) as usize..(self.node.end - 1) as usize],
                ),
                QuoteType::Triple => Cow::Borrowed(
                    &source[(self.node.start + 3) as usize..(self.node.end - 3) as usize],
                ),
                QuoteType::Concat => {
                    let input = &source[(self.node.start) as usize..(self.node.end) as usize];
                    let mut result = String::new();
                    let mut chars = input.chars().peekable();

                    while let Some(c) = chars.next() {
                        let quote_type = match c {
                            '\'' => {
                                if chars.peek() == Some(&'\'') && chars.nth(1) == Some('\'') {
                                    // Triple single quote
                                    "'''"
                                } else {
                                    // Single quote
                                    "'"
                                }
                            }
                            '"' => {
                                if chars.peek() == Some(&'"') && chars.nth(1) == Some('"') {
                                    // Triple double quote
                                    "\"\"\""
                                } else {
                                    // Double quote
                                    "\""
                                }
                            }
                            _ => continue, // Ignore any non-quote characters
                        };

                        // Extract content between quotes
                        let mut content = String::new();
                        let mut quote_ending = quote_type.chars().peekable();

                        for next_char in chars.by_ref() {
                            // Check for quote ending
                            if Some(&next_char) == quote_ending.peek() {
                                quote_ending.next();
                                if quote_ending.peek().is_none() {
                                    break; // End of the string literal
                                }
                            } else {
                                content.push(next_char);
                                quote_ending = quote_type.chars().peekable(); // Reset the ending check
                            }
                        }

                        // Concatenate the cleaned-up string
                        result.push_str(&content);
                    }

                    Cow::Owned(result)
                }
            },
            ConstantValue::Bool(b) => {
                if *b {
                    Cow::Borrowed("true")
                } else {
                    Cow::Borrowed("false")
                }
            },
            ConstantValue::Int => Cow::Borrowed(
                &source[self.node.start as usize..self.node.end as usize],
            ),
            ConstantValue::Float => Cow::Borrowed(
                &source[self.node.start as usize..self.node.end as usize],
            ),
            _ => todo!("Call the parser and get the value"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum ConstantValue {
    None,
    Ellipsis,
    Bool(bool),
    // If the string start with triple quotes or single
    // true => triple
    // false => single
    Str(QuoteType),
    // Str,
    Bytes,
    Tuple,
    // Numbers are string because we don't care about the value rn.
    Int,
    Float,
    Complex,
}

#[derive(Clone, PartialEq, Debug)]
pub enum QuoteType {
    Single,
    Triple,
    // When this string was created because two strings were concatenated
    Concat,
}

impl From<&str> for QuoteType {
    fn from(value: &str) -> Self {
        if value.starts_with("\"\"\"") || value.starts_with("'''") {
            return Self::Triple;
        }
        Self::Single
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
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
    pub name: StrId,
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
    pub name: StrId,
    pub args: Arguments,
    pub body: Vec<Statement>,
    pub decorator_list: Vec<Expression>,
    pub returns: Option<Expression>,
    pub type_comment: Option<String>,
    pub type_params: Vec<TypeParam>,
}

impl AsyncFunctionDef {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        node: Node,
        name: StrId,
        args: Arguments,
        body: Vec<Statement>,
        decorator_list: Vec<Expression>,
        returns: Option<Expression>,
        type_comment: Option<&str>,
        type_params: Vec<TypeParam>,
    ) -> Self {
        Self {
            node,
            name,
            args,
            body,
            decorator_list,
            returns,
            type_comment: type_comment.map(|s| s.to_owned()),
            type_params,
        }
    }
    pub fn to_function_def(&self) -> FunctionDef {
        FunctionDef {
            node: self.node,
            name: self.name,
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
    pub name: StrId,
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

impl Module {
    pub fn new(node: Node, body: Vec<Statement>) -> Self {
        Self { node, body }
    }
}
impl Assign {
    pub fn new(node: Node, targets: Vec<Expression>, value: Expression) -> Self {
        Self {
            node,
            targets,
            value,
        }
    }
}
impl AnnAssign {
    pub fn new(
        node: Node,
        target: Expression,
        annotation: Expression,
        value: Option<Expression>,
        simple: bool,
    ) -> Self {
        Self {
            node,
            target,
            annotation,
            value,
            simple,
        }
    }
}
impl AugAssign {
    pub fn new(node: Node, target: Expression, op: AugAssignOp, value: Expression) -> Self {
        Self {
            node,
            target,
            op,
            value,
        }
    }
}
impl Assert {
    pub fn new(node: Node, test: Expression, msg: Option<Expression>) -> Self {
        Self { node, test, msg }
    }
}
impl Pass {
    pub fn new(node: Node) -> Self {
        Self { node }
    }
}
impl Delete {
    pub fn new(node: Node, targets: Vec<Expression>) -> Self {
        Self { node, targets }
    }
}
impl Return {
    pub fn new(node: Node, value: Option<Expression>) -> Self {
        Self { node, value }
    }
}
impl Raise {
    pub fn new(node: Node, exc: Option<Expression>, cause: Option<Expression>) -> Self {
        Self { node, exc, cause }
    }
}
impl Break {
    pub fn new(node: Node) -> Self {
        Self { node }
    }
}
impl Continue {
    pub fn new(node: Node) -> Self {
        Self { node }
    }
}
impl Import {
    pub fn new(node: Node, names: Vec<Alias>) -> Self {
        Self { node, names }
    }
}
impl Alias {
    pub fn new(node: Node, name: &str, asname: Option<&str>) -> Self {
        Self {
            node,
            name: name.to_owned(),
            asname: asname.map(|s| s.to_owned()),
        }
    }
}
impl ImportFrom {
    pub fn new(node: Node, module: &str, names: Vec<Alias>, level: usize) -> Self {
        Self {
            node,
            module: module.to_owned(),
            names,
            level,
        }
    }
}
impl Global {
    pub fn new(node: Node, names: Vec<String>) -> Self {
        Self { node, names }
    }
}
impl Nonlocal {
    pub fn new(node: Node, names: Vec<String>) -> Self {
        Self { node, names }
    }
}
impl Name {
    pub fn new(node: Node, id: &str, parenthesized: bool) -> Self {
        Self {
            node,
            id: id.to_owned(),
            parenthesized,
        }
    }
}
impl Constant {
    pub fn new(node: Node, value: ConstantValue) -> Self {
        Self { node, value }
    }
}
impl List {
    pub fn new(node: Node, elements: Vec<Expression>) -> Self {
        Self { node, elements }
    }
}
impl Tuple {
    pub fn new(node: Node, elements: Vec<Expression>) -> Self {
        Self { node, elements }
    }
}
impl Dict {
    pub fn new(node: Node, keys: Vec<Expression>, values: Vec<Expression>) -> Self {
        Self { node, keys, values }
    }
}
impl Set {
    pub fn new(node: Node, elements: Vec<Expression>) -> Self {
        Self { node, elements }
    }
}
impl BoolOperation {
    pub fn new(node: Node, op: BooleanOperator, values: Vec<Expression>) -> Self {
        Self { node, op, values }
    }
}
impl UnaryOperation {
    pub fn new(node: Node, op: UnaryOperator, operand: Expression) -> Self {
        Self { node, op, operand }
    }
}
impl BinOp {
    pub fn new(node: Node, op: BinaryOperator, left: Expression, right: Expression) -> Self {
        Self {
            node,
            op,
            left,
            right,
        }
    }
}
impl NamedExpression {
    pub fn new(node: Node, target: Expression, value: Expression) -> Self {
        Self {
            node,
            target,
            value,
        }
    }
}
impl Yield {
    pub fn new(node: Node, value: Option<Expression>) -> Self {
        Self { node, value }
    }
}
impl YieldFrom {
    pub fn new(node: Node, value: Expression) -> Self {
        Self { node, value }
    }
}
impl Starred {
    pub fn new(node: Node, value: Expression) -> Self {
        Self { node, value }
    }
}
impl Generator {
    pub fn new(node: Node, element: Expression, generators: Vec<Comprehension>) -> Self {
        Self {
            node,
            element,
            generators,
        }
    }
}
impl ListComp {
    pub fn new(node: Node, element: Expression, generators: Vec<Comprehension>) -> Self {
        Self {
            node,
            element,
            generators,
        }
    }
}
impl SetComp {
    pub fn new(node: Node, element: Expression, generators: Vec<Comprehension>) -> Self {
        Self {
            node,
            element,
            generators,
        }
    }
}
impl DictComp {
    pub fn new(
        node: Node,
        key: Expression,
        value: Expression,
        generators: Vec<Comprehension>,
    ) -> Self {
        Self {
            node,
            key,
            value,
            generators,
        }
    }
}
impl Comprehension {
    pub fn new(
        node: Node,
        target: Expression,
        iter: Expression,
        ifs: Vec<Expression>,
        is_async: bool,
    ) -> Self {
        Self {
            node,
            target,
            iter,
            ifs,
            is_async,
        }
    }
}
impl Attribute {
    pub fn new(node: Node, value: Expression, attr: &str) -> Self {
        Self {
            node,
            value,
            attr: attr.to_owned(),
        }
    }
}
impl Subscript {
    pub fn new(node: Node, value: Expression, slice: Expression) -> Self {
        Self { node, value, slice }
    }
}
impl Slice {
    pub fn new(
        node: Node,
        lower: Option<Expression>,
        upper: Option<Expression>,
        step: Option<Expression>,
    ) -> Self {
        Self {
            node,
            lower,
            upper,
            step,
        }
    }
}
impl Call {
    pub fn new(
        node: Node,
        func: Expression,
        args: Vec<Expression>,
        keywords: Vec<Keyword>,
        starargs: Option<Expression>,
        kwargs: Option<Expression>,
    ) -> Self {
        Self {
            node,
            func,
            args,
            keywords,
            starargs,
            kwargs,
        }
    }
}
impl Keyword {
    pub fn new(node: Node, arg: Option<&str>, value: Expression) -> Self {
        Self {
            node,
            arg: arg.map(|s| s.to_owned()),
            value,
        }
    }
}
impl Await {
    pub fn new(node: Node, value: Expression) -> Self {
        Self { node, value }
    }
}
impl Compare {
    pub fn new(
        node: Node,
        left: Expression,
        ops: Vec<ComparisonOperator>,
        comparators: Vec<Expression>,
    ) -> Self {
        Self {
            node,
            left,
            ops,
            comparators,
        }
    }
}
impl Lambda {
    pub fn new(node: Node, args: Arguments, body: Expression) -> Self {
        Self { node, args, body }
    }
}
impl Arguments {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        node: Node,
        posonlyargs: Vec<Arg>,
        args: Vec<Arg>,
        vararg: Option<Arg>,
        kwonlyargs: Vec<Arg>,
        kw_defaults: Vec<Option<Expression>>,
        kwarg: Option<Arg>,
        defaults: Vec<Expression>,
    ) -> Self {
        Self {
            node,
            posonlyargs,
            args,
            vararg,
            kwonlyargs,
            kw_defaults,
            kwarg,
            defaults,
        }
    }
}
impl Arg {
    pub fn new(node: Node, arg: &str, annotation: Option<Expression>) -> Self {
        Self {
            node,
            arg: arg.to_owned(),
            annotation,
        }
    }
}
impl IfExp {
    pub fn new(node: Node, test: Expression, body: Expression, orelse: Expression) -> Self {
        Self {
            node,
            test,
            body,
            orelse,
        }
    }
}
impl FormattedValue {
    pub fn new(
        node: Node,
        value: Expression,
        conversion: i32,
        format_spec: Option<Expression>,
    ) -> Self {
        Self {
            node,
            value,
            conversion,
            format_spec,
        }
    }
}
impl JoinedStr {
    pub fn new(node: Node, values: Vec<Expression>) -> Self {
        Self { node, values }
    }
}
impl If {
    pub fn new(node: Node, test: Expression, body: Vec<Statement>, orelse: Vec<Statement>) -> Self {
        Self {
            node,
            test,
            body,
            orelse,
        }
    }
}
impl While {
    pub fn new(node: Node, test: Expression, body: Vec<Statement>, orelse: Vec<Statement>) -> Self {
        Self {
            node,
            test,
            body,
            orelse,
        }
    }
}
impl For {
    pub fn new(
        node: Node,
        target: Expression,
        iter: Expression,
        body: Vec<Statement>,
        orelse: Vec<Statement>,
    ) -> Self {
        Self {
            node,
            target,
            iter,
            body,
            orelse,
        }
    }
}
impl AsyncFor {
    pub fn new(
        node: Node,
        target: Expression,
        iter: Expression,
        body: Vec<Statement>,
        orelse: Vec<Statement>,
    ) -> Self {
        Self {
            node,
            target,
            iter,
            body,
            orelse,
        }
    }
}
impl With {
    pub fn new(node: Node, items: Vec<WithItem>, body: Vec<Statement>) -> Self {
        Self { node, items, body }
    }
}
impl AsyncWith {
    pub fn new(node: Node, items: Vec<WithItem>, body: Vec<Statement>) -> Self {
        Self { node, items, body }
    }
}
impl WithItem {
    pub fn new(node: Node, context_expr: Expression, optional_vars: Option<Expression>) -> Self {
        Self {
            node,
            context_expr,
            optional_vars,
        }
    }
}
impl Try {
    pub fn new(
        node: Node,
        body: Vec<Statement>,
        handlers: Vec<ExceptHandler>,
        orelse: Vec<Statement>,
        finalbody: Vec<Statement>,
    ) -> Self {
        Self {
            node,
            body,
            handlers,
            orelse,
            finalbody,
        }
    }
}
impl TryStar {
    pub fn new(
        node: Node,
        body: Vec<Statement>,
        handlers: Vec<ExceptHandler>,
        orelse: Vec<Statement>,
        finalbody: Vec<Statement>,
    ) -> Self {
        Self {
            node,
            body,
            handlers,
            orelse,
            finalbody,
        }
    }
}
impl ExceptHandler {
    pub fn new(
        node: Node,
        typ: Option<Expression>,
        name: Option<&str>,
        body: Vec<Statement>,
    ) -> Self {
        Self {
            node,
            typ,
            name: name.map(|s| s.to_owned()),
            body,
        }
    }
}
impl FunctionDef {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        node: Node,
        name: StrId,
        args: Arguments,
        body: Vec<Statement>,
        decorator_list: Vec<Expression>,
        returns: Option<Expression>,
        type_comment: Option<&str>,
        type_params: Vec<TypeParam>,
    ) -> Self {
        Self {
            node,
            name,
            args,
            body,
            decorator_list,
            returns,
            type_comment: type_comment.map(|s| s.to_owned()),
            type_params,
        }
    }
}
impl ClassDef {
    pub fn new(
        node: Node,
        name: StrId,
        bases: Vec<Expression>,
        keywords: Vec<Keyword>,
        body: Vec<Statement>,
        decorator_list: Vec<Expression>,
        type_params: Vec<TypeParam>,
    ) -> Self {
        Self {
            node,
            name,
            bases,
            keywords,
            body,
            decorator_list,
            type_params,
        }
    }
}
impl Match {
    pub fn new(node: Node, subject: Expression, cases: Vec<MatchCase>) -> Self {
        Self {
            node,
            subject,
            cases,
        }
    }
}
impl MatchCase {
    pub fn new(
        node: Node,
        pattern: MatchPattern,
        guard: Option<Expression>,
        body: Vec<Statement>,
    ) -> Self {
        Self {
            node,
            pattern,
            guard,
            body,
        }
    }
}
impl MatchValue {
    pub fn new(node: Node, value: Expression) -> Self {
        Self { node, value }
    }
}
impl MatchAs {
    pub fn new(node: Node, name: Option<&str>, pattern: Option<MatchPattern>) -> Self {
        Self {
            node,
            name: name.map(|s| s.to_owned()),
            pattern,
        }
    }
}
impl MatchMapping {
    pub fn new(
        node: Node,
        keys: Vec<Expression>,
        patterns: Vec<MatchPattern>,
        rest: Option<&str>,
    ) -> Self {
        Self {
            node,
            keys,
            patterns,
            rest: rest.map(|s| s.to_owned()),
        }
    }
}
impl MatchClass {
    pub fn new(
        node: Node,
        cls: Expression,
        patterns: Vec<MatchPattern>,
        kwd_attrs: Vec<String>,
        kwd_patterns: Vec<MatchPattern>,
    ) -> Self {
        Self {
            node,
            cls,
            patterns,
            kwd_attrs,
            kwd_patterns,
        }
    }
}
impl TypeVar {
    pub fn new(node: Node, name: &str, bound: Option<Expression>) -> Self {
        Self {
            node,
            name: name.to_owned(),
            bound,
        }
    }
}
impl ParamSpec {
    pub fn new(node: Node, name: &str) -> Self {
        Self {
            node,
            name: name.to_owned(),
        }
    }
}
impl TypeVarTuple {
    pub fn new(node: Node, name: &str) -> Self {
        Self {
            node,
            name: name.to_owned(),
        }
    }
}
impl TypeAlias {
    pub fn new(node: Node, name: &str, type_params: Vec<TypeParam>, value: Expression) -> Self {
        Self {
            node,
            name: name.to_owned(),
            type_params,
            value,
        }
    }
}

mod tests {
    #[cfg(target_pointer_width = "64")]
    #[test]
    fn no_bloat_enum_sizes() {
        use crate::ast::*;
        use std::mem::size_of;
        assert_eq!(size_of::<Statement>(), 16);
        assert_eq!(size_of::<Expression>(), 16);
    }
}
