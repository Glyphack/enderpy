use core::panic;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: Kind,
    // Value might be deleted in the future
    pub value: TokenValue,
    pub start: u32,
    pub end: u32,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind: &str = self.kind.into();
        let value = format!("({:?})", self.value);
        let start = self.start.to_string();
        let end = self.end.to_string();

        write!(f, "{},{}: {}   {}", start, end, kind, value,)
    }
}

// https://docs.python.org/3/reference/lexical_analysis.html
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Kind {
    // Line structure
    NewLine, // \n
    Indent,  // \t
    Dedent,  // \t

    // Identifiers
    Identifier,

    // Keywords
    False,    // False
    None,     // None
    True,     // True
    And,      // and
    As,       // as
    Assert,   // assert
    Async,    // async
    Await,    // await
    Break,    // break
    Class,    // class
    Continue, // continue
    Comment,  // the rest of the line after a # character
    Def,      // def
    Del,      // del
    Elif,     // elif
    Else,     // else
    Ellipsis, // ...
    Except,   // except
    Finally,  // finally
    For,      // for
    From,     // from
    Global,   // global
    If,       // if
    Import,   // import
    In,       // in
    Is,       // is
    Lambda,   // lambda
    Nonlocal, // nonlocal
    Not,      // not
    Or,       // or
    Pass,     // pass
    Raise,    // raise
    Return,   // return
    Try,      // try
    While,    // while
    With,     // with
    Yield,    // yield

    // String Literals
    StringLiteral,
    // f-string literal
    // To understand why using the following token, see:
    // https://peps.python.org/pep-0701/#handling-of-f-string-debug-expressions
    FStringStart,  // f or F prefix and the opening quote
    FStringMiddle, // portion of the test which is not in the {} pair
    FStringEnd,    // the closing quote
    // byte string literal
    Bytes, // b or B prefix
    // raw f-string literal,
    RawFStringStart, // rf or rF or Rf or RF or fr or fR or Fr or FR prefix
    // raw byte string literal
    RawBytes, // rb or rB or Rb or RB or br or bR or Br or BR prefix
    // unicode string literal,
    Unicode, // u or U prefix

    Integer,                // 123
    Binary,                 // 0b1010
    Octal,                  // 0o123
    Hexadecimal,            // 0x123
    PointFloat,             // 123.456
    ExponentFloat,          // 123.456e-10
    ImaginaryInteger,       // 123j
    ImaginaryPointFloat,    // 123.2j
    ImaginaryExponentFloat, // 123.456e-10j

    // Operators
    Plus,       // +
    Minus,      // -
    Mul,        // *
    Pow,        // **
    Div,        // /
    IntDiv,     // //
    Mod,        // %
    MatrixMul,  // @
    LeftShift,  // <<
    RightShift, // >>
    BitAnd,     // &
    BitOr,      // |
    BitXor,     // ^
    /// ~
    BitNot,
    Walrus,    // :=
    Less,      // <
    Greater,   // >
    LessEq,    // <=
    GreaterEq, // >=
    Eq,        // ==
    NotEq,     // !=

    // Delimiters
    LeftParen,        // (
    RightParen,       // )
    LeftBrace,        // [
    RightBrace,       // ]
    LeftBracket,      // {
    RightBracket,     // }
    Comma,            // ,
    Colon,            // :
    Dot,              // .
    SemiColon,        // ;
    Assign,           // =
    Arrow,            // ->
    AddAssign,        // +=
    SubAssign,        // -=
    MulAssign,        // *=
    DivAssign,        // /=
    ModAssign,        // %=
    MatrixMulAssign,  // @=
    BitAndAssign,     // &=
    BitOrAssign,      // |=
    BitXorAssign,     // ^=
    IntDivAssign,     // //=
    ShiftLeftAssign,  // <<=
    ShiftRightAssign, // >>=
    PowAssign,        // **=

    // Special
    BackSlash, // \

    // Unconditional error
    Dollar,       // $
    QuestionMark, // ?
    BackTick,     // `

    // Others
    WhiteSpace,
    Error,
    Eof,
}

impl Kind {
    pub fn is_string(&self) -> bool {
        matches!(
            self,
            Self::StringLiteral | Self::RawBytes | Self::Bytes | Self::FStringStart
        )
    }
    pub fn is_unary_op(&self) -> bool {
        matches!(self, Kind::Not | Kind::BitNot | Kind::Minus | Kind::Plus)
    }

    pub fn is_bin_arithmetic_op(&self) -> bool {
        matches!(
            self,
            Kind::Plus
                | Kind::Minus
                | Kind::Mul
                | Kind::MatrixMul
                | Kind::Div
                | Kind::Mod
                | Kind::Pow
                | Kind::IntDiv
        )
    }

    pub fn is_comparison_operator(&self) -> bool {
        match self {
        Kind::Eq
        | Kind::NotEq
        | Kind::Less
        | Kind::LessEq
        | Kind::Greater
        | Kind::GreaterEq
        | Kind::Is
        | Kind::In
        // Not is not a comparison operator, but it is used in the
        // "not in" operator
        | Kind::Not => true,
        _ => false,
    }
    }

    pub fn is_atom(&self) -> bool {
        match self {
        Kind::Identifier
        | Kind::StringLiteral
        | Kind::RawBytes
        | Kind::Bytes
        | Kind::FStringStart
        | Kind::RawFStringStart
        | Kind::Integer
        | Kind::True
        | Kind::False
        | Kind::Binary
        | Kind::Octal
        | Kind::Hexadecimal
        | Kind::PointFloat
        | Kind::ExponentFloat
        | Kind::ImaginaryInteger
        | Kind::ImaginaryPointFloat
        | Kind::ImaginaryExponentFloat
        // These might start a enclosured expression
        // https://docs.python.org/3/reference/expressions.html#atoms
        | Kind::LeftParen
        | Kind::LeftBracket
        | Kind::LeftBrace
        | Kind::Yield
        | Kind::Ellipsis
        | Kind::None => true,
        _ => false,
        }
    }

    /// Determines if the kind can be start of a python expression in grammar
    pub fn is_star_expression(&self) -> bool {
        if self.is_atom() {
            return true;
        }
        matches!(
            self,
            Kind::Await | Kind::BitNot | Kind::Minus | Kind::Plus | Kind::Mul
        )
    }
}

impl From<Kind> for &str {
    fn from(val: Kind) -> Self {
        match val {
            Kind::Hexadecimal => "Hexadecimal",
            Kind::PointFloat => "PointFloat",
            Kind::ExponentFloat => "ExponentFloat",
            Kind::ImaginaryInteger => "ImaginaryInteger",
            Kind::ImaginaryPointFloat => "ImaginaryPointFloat",
            Kind::ImaginaryExponentFloat => "ImaginaryExponentFloat",
            Kind::Integer => "Integer",
            Kind::Arrow => "->",
            Kind::AddAssign => "+=",
            Kind::SubAssign => "-=",
            Kind::MulAssign => "*=",
            Kind::DivAssign => "/=",
            Kind::ModAssign => "%=",
            Kind::MatrixMulAssign => "@=",
            Kind::BitAndAssign => "&=",
            Kind::BitOrAssign => "|=",
            Kind::BitXorAssign => "^=",
            Kind::IntDivAssign => "//=",
            Kind::ShiftLeftAssign => "<<=",
            Kind::ShiftRightAssign => ">>=",
            Kind::PowAssign => "**=",
            Kind::BackSlash => "\\",
            Kind::Dollar => "$",
            Kind::QuestionMark => "?",
            Kind::BackTick => "`",
            Kind::WhiteSpace => "WhiteSpace",
            Kind::Eof => "Eof",
            Kind::Error => "Error",
            Kind::GreaterEq => ">=",
            Kind::Eq => "==",
            Kind::NotEq => "!=",
            Kind::LessEq => "<=",
            Kind::Walrus => ":=",
            Kind::LeftParen => "(",
            Kind::RightParen => ")",
            Kind::LeftBrace => "[",
            Kind::RightBrace => "]",
            Kind::LeftBracket => "{",
            Kind::RightBracket => "}",
            Kind::Comma => ",",
            Kind::Colon => ":",
            Kind::Comment => "Comment",
            Kind::Dot => ".",
            Kind::SemiColon => ";",
            Kind::Assign => "=",
            Kind::Plus => "+",
            Kind::Minus => "-",
            Kind::Mul => "*",
            Kind::Pow => "**",
            Kind::Div => "/",
            Kind::IntDiv => "//",
            Kind::Mod => "%",
            Kind::MatrixMul => "@",
            Kind::LeftShift => "<<",
            Kind::RightShift => ">>",
            Kind::BitAnd => "&",
            Kind::BitOr => "|",
            Kind::BitXor => "^",
            Kind::BitNot => "~",
            Kind::Less => "<",
            Kind::Greater => ">",
            Kind::NewLine => "NewLine",
            Kind::Identifier => "Identifier",
            Kind::False => "False",
            Kind::None => "None",
            Kind::True => "True",
            Kind::And => "And",
            Kind::As => "As",
            Kind::Assert => "Assert",
            Kind::Async => "Async",
            Kind::Await => "Await",
            Kind::Break => "Break",
            Kind::Class => "Class",
            Kind::Continue => "Continue",
            Kind::Def => "Def",
            Kind::Del => "Del",
            Kind::Elif => "Elif",
            Kind::Else => "Else",
            Kind::Except => "Except",
            Kind::Finally => "Finally",
            Kind::For => "For",
            Kind::From => "From",
            Kind::Global => "Global",
            Kind::If => "If",
            Kind::Import => "Import",
            Kind::In => "In",
            Kind::Is => "Is",
            Kind::Lambda => "Lambda",
            Kind::Nonlocal => "Nonlocal",
            Kind::Not => "Not",
            Kind::Or => "Or",
            Kind::Pass => "Pass",
            Kind::Raise => "Raise",
            Kind::Return => "Return",
            Kind::Try => "Try",
            Kind::While => "While",
            Kind::With => "With",
            Kind::Yield => "Yield",
            Kind::StringLiteral => "StringLiteral",
            Kind::FStringStart => "FStringStart",
            Kind::FStringMiddle => "FstringMiddle",
            Kind::FStringEnd => "FStringEnd",
            Kind::Bytes => "Bytes",
            Kind::RawFStringStart => "RawFString",
            Kind::RawBytes => "RawBytes",
            Kind::Unicode => "Unicode",
            Kind::Binary => "Binary",
            Kind::Octal => "Octal",
            Kind::Indent => "Indent",
            Kind::Dedent => "Dedent",
            Kind::Ellipsis => "Ellipsis",
        }
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Into::<&str>::into(*self))
    }
}

#[derive(Debug, PartialEq, Clone, is_macro::Is)]
pub enum TokenValue {
    None,
    Number(String),
    Str(String),
    Indent(usize),
    // Soft keywords are special values
    Type,
    Match,
}

impl TokenValue {
    pub fn take_string(&mut self) -> String {
        match self {
            TokenValue::Str(s) => std::mem::take(s),
            TokenValue::Number(s) => std::mem::take(s),
            _ => {
                panic!("not a str")
            }
        }
    }
}

impl Display for TokenValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenValue::None => write!(f, "None"),
            TokenValue::Number(n) => write!(f, "{}", n),
            TokenValue::Str(s) => write!(f, "{}", s),
            TokenValue::Indent(i) => write!(f, "{}", i),
            TokenValue::Type => write!(f, "type"),
            TokenValue::Match => write!(f, "match"),
        }
    }
}

impl From<TokenValue> for &str {
    fn from(val: TokenValue) -> Self {
        match val {
            TokenValue::None => "None",
            TokenValue::Number(_) => "Number",
            TokenValue::Str(_) => "Str",
            TokenValue::Indent(_) => "Indent",
            TokenValue::Type => "type",
            TokenValue::Match => "match",
        }
    }
}
