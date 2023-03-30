use miette::{SourceOffset, SourceSpan};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)] // #[serde(tag = "type")]
pub struct Node {
    /// Start offset in source
    pub start: usize,

    /// End offset in source
    pub end: usize,
}

impl Node {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

impl From<Node> for SourceSpan {
    fn from(val: Node) -> Self {
        Self::new(
            SourceOffset::from(val.start as usize),
            SourceOffset::from(val.len() as usize),
        )
    }
}

// The following structs are used to represent the AST
// https://docs.python.org/3/library/ast.html#abstract-grammar
#[derive(Debug)]
pub struct Module {
    pub node: Node,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    AssignStatement(Assign),
    ExpressionStatement(Expression),
}

#[derive(Debug)]
pub struct Assign {
    pub node: Node,
    pub targets: Vec<Expression>,
    pub value: Expression,
}

#[derive(Debug)]
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
    Await(Box<Await>),
    Compare(Box<Compare>),
}

// https://docs.python.org/3/reference/expressions.html#atom-identifiers
#[derive(Debug)]
pub struct Name {
    pub node: Node,
    pub id: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constant {
    pub node: Node,
    pub value: ConstantValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantValue {
    None,
    Bool(bool),
    Str(String),
    Bytes(Vec<u8>),
    Tuple(Vec<Constant>),
    // Numbers are string because we don't care about the value rn.
    Int(String),
    Float(String),
    Complex { real: String, imaginary: String },
}

#[derive(Debug)]
pub struct List {
    pub node: Node,
    pub elements: Vec<Expression>,
}

#[derive(Debug)]
pub struct Tuple {
    pub node: Node,
    pub elements: Vec<Expression>,
}

#[derive(Debug)]
pub struct Dict {
    pub node: Node,
    pub keys: Vec<Expression>,
    pub values: Vec<Expression>,
}

#[derive(Debug)]
pub struct Set {
    pub node: Node,
    pub elements: Vec<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.BoolOp
#[derive(Debug)]
pub struct BoolOperation {
    pub node: Node,
    pub op: BooleanOperator,
    pub values: Vec<Expression>,
}

#[derive(Debug)]
pub enum BooleanOperator {
    And,
    Or,
}

// https://docs.python.org/3/library/ast.html#ast.UnaryOp
#[derive(Debug)]
pub struct UnaryOperation {
    pub node: Node,
    pub op: UnaryOperator,
    pub operand: Box<Expression>,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Not,
    Invert,
    UAdd,
    USub,
}

// https://docs.python.org/3/library/ast.html#ast.BinOp
#[derive(Debug)]
pub struct BinOp {
    pub node: Node,
    pub op: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug)]
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

// https://docs.python.org/3/library/ast.html#ast.NamedExpr
#[derive(Debug)]
pub struct NamedExpression {
    pub node: Node,
    pub target: Box<Expression>,
    pub value: Box<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Yield
#[derive(Debug)]
pub struct Yield {
    pub node: Node,
    pub value: Option<Box<Expression>>,
}

// https://docs.python.org/3/library/ast.html#ast.YieldFrom
#[derive(Debug)]
pub struct YieldFrom {
    pub node: Node,
    pub value: Box<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Starred
#[derive(Debug)]
pub struct Starred {
    pub node: Node,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Generator {
    pub node: Node,
    pub element: Box<Expression>,
}

#[derive(Debug)]
pub struct ListComp {
    pub node: Node,
    pub element: Box<Expression>,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug)]
pub struct SetComp {
    pub node: Node,
    pub element: Box<Expression>,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug)]
pub struct DictComp {
    pub node: Node,
    pub key: Box<Expression>,
    pub value: Box<Expression>,
    pub generators: Vec<Comprehension>,
}

#[derive(Debug)]
pub struct Comprehension {
    pub node: Node,
    pub target: Box<Expression>,
    pub iter: Box<Expression>,
    pub ifs: Vec<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Attribute
#[derive(Debug)]
pub struct Attribute {
    pub node: Node,
    pub value: Box<Expression>,
    pub attr: String,
}

// https://docs.python.org/3/library/ast.html#ast.Subscript
#[derive(Debug)]
pub struct Subscript {
    pub node: Node,
    pub value: Box<Expression>,
    pub slice: Box<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Slice
// can be used for Subscript
#[derive(Debug)]
pub struct Slice {
    pub node: Node,
    pub lower: Option<Box<Expression>>,
    pub upper: Option<Box<Expression>>,
    pub step: Option<Box<Expression>>,
}

// https://docs.python.org/3/library/ast.html#ast.Call
#[derive(Debug)]
pub struct Call {
    pub node: Node,
    pub func: Box<Expression>,
    pub args: Vec<Expression>,
    pub keywords: Vec<Keyword>,
}

#[derive(Debug)]
pub struct Keyword {
    pub node: Node,
    pub arg: String,
    pub value: Box<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Await
#[derive(Debug)]
pub struct Await {
    pub node: Node,
    pub value: Box<Expression>,
}

// https://docs.python.org/3/library/ast.html#ast.Compare
#[derive(Debug)]
pub struct Compare {
    pub node: Node,
    pub left: Box<Expression>,
    pub ops: Vec<ComparisonOperator>,
    pub comparators: Vec<Expression>,
}

#[derive(Debug)]
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
