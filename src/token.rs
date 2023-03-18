#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Kind,
    // Value might be deleted in the future
    pub value: TokenValue,
    pub start: usize,
    pub end: usize,
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
    Def,      // def
    Del,      // del
    Elif,     // elif
    Else,     // else
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
    FString, // f or F prefix
    // byte string literal
    Bytes, // b or B prefix
    // raw string literal
    RawString, // r or R prefix
    // raw f-string literal,
    RawFString, // rf or rF or Rf or RF or fr or fR or Fr or FR prefix
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
    BitNot,     // ~
    Walrus,     // :=
    Less,       // <
    Greater,    // >
    LessEq,     // <=
    GreaterEq,  // >=
    Eq,         // ==
    NotEq,      // !=

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
    Sharp,     // #
    BackSlash, // \

    // Unconditional error
    Dollar,       // $
    QuestionMark, // ?
    BackTick,     // `

    // Others
    WhiteSpace,
    Eof,
}

impl Kind {
    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub fn to_str(self) -> &'static str {
        match self {
            Kind::Hexadecimal => "Hexadecimal",
            Kind::PointFloat => "PointFloat",
            Kind::ExponentFloat => "ExponentFloat",
            Kind::ImaginaryInteger => "ImaginaryInteger",
            Kind::ImaginaryPointFloat => "ImaginaryPointFloat",
            Kind::ImaginaryExponentFloat => "ImaginaryExponentFloat",
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
            Kind::Sharp => "#",
            Kind::BackSlash => "\\",
            Kind::Dollar => "$",
            Kind::QuestionMark => "?",
            Kind::BackTick => "`",
            Kind::WhiteSpace => "WhiteSpace",
            Kind::Eof => "Eof",
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
            Kind::NewLine => "\n",
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
            Kind::FString => "FString",
            Kind::Bytes => "Bytes",
            Kind::RawString => "RawString",
            Kind::RawFString => "RawFString",
            Kind::RawBytes => "RawBytes",
            Kind::Unicode => "Unicode",
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
            Kind::And => "and",
            Kind::As => "as",
            Kind::Assert => "assert",
            Kind::Async => "async",
            Kind::Await => "await",
            Kind::Break => "break",
            Kind::Class => "class",
            Kind::Continue => "continue",
            Kind::Def => "def",
            Kind::Del => "del",
            Kind::Elif => "elif",
            Kind::Else => "else",
            Kind::Except => "except",
            Kind::Finally => "finally",
            Kind::For => "for",
            Kind::From => "from",
            Kind::Global => "global",
            Kind::If => "if",
            Kind::Import => "import",
            Kind::In => "in",
            Kind::Is => "is",
            Kind::Lambda => "lambda",
            Kind::Nonlocal => "nonlocal",
            Kind::Not => "not",
            Kind::Or => "or",
            Kind::Pass => "pass",
            Kind::Raise => "raise",
            Kind::Return => "return",
            Kind::Try => "try",
            Kind::While => "while",
            Kind::With => "with",
            Kind::Yield => "yield",
            Kind::False => "False",
            Kind::None => "None",
            Kind::True => "True",
            Kind::StringLiteral => "StringLiteral",
            Kind::FString => "FString",
            Kind::Bytes => "Bytes",
            Kind::RawString => "RawString",
            Kind::RawFString => "RawFString",
            Kind::RawBytes => "RawBytes",
            Kind::Unicode => "Unicode",
            Kind::Integer => "Integer",
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
            Kind::And => "and",
            Kind::As => "as",
            Kind::Assert => "assert",
            Kind::Async => "async",
            Kind::Await => "await",
            Kind::Break => "break",
            Kind::Class => "class",
            Kind::Continue => "continue",
            Kind::Def => "def",
            Kind::Del => "del",
            Kind::Elif => "elif",
            Kind::Else => "else",
            Kind::Except => "except",
            Kind::Finally => "finally",
            Kind::For => "for",
            Kind::From => "from",
            Kind::Global => "global",
            Kind::If => "if",
            Kind::Import => "import",
            Kind::In => "in",
            Kind::Is => "is",
            Kind::Lambda => "lambda",
            Kind::Nonlocal => "nonlocal",
            Kind::Not => "not",
            Kind::Or => "or",
            Kind::Pass => "pass",
            Kind::Raise => "raise",
            Kind::Return => "return",
            Kind::Try => "try",
            Kind::While => "while",
            Kind::With => "with",
            Kind::Yield => "yield",
            Kind::False => "False",
            Kind::None => "None",
            Kind::True => "True",
            Kind::FString
            | Kind::Bytes
            | Kind::RawString
            | Kind::RawFString
            | Kind::RawBytes
            | Kind::Unicode => "StringLiteral",
            Kind::Identifier => "Identifier",
            Kind::NewLine => "NewLine",
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
            Kind::And => "and",
            Kind::As => "as",
            Kind::Assert => "assert",
            Kind::Async => "async",
            Kind::Await => "await",
            Kind::Break => "break",
            Kind::Class => "class",
            Kind::Continue => "continue",
            Kind::Def => "def",
            Kind::Del => "del",
            Kind::Elif => "elif",
            Kind::Else => "else",
            Kind::Except => "except",
            Kind::Finally => "finally",
            Kind::For => "for",
            Kind::From => "from",
            Kind::Global => "global",
            Kind::If => "if",
            Kind::Import => "import",
            Kind::In => "in",
            Kind::Is => "is",
            Kind::Lambda => "lambda",
            Kind::Nonlocal => "nonlocal",
            Kind::Not => "not",
            Kind::Or => "or",
            Kind::Pass => "pass",
            Kind::Raise => "raise",
            Kind::Return => "return",
            Kind::Try => "try",
            Kind::While => "while",
            Kind::With => "with",
            Kind::Yield => "yield",
            Kind::False => "False",
            Kind::None => "None",
            Kind::True => "True",
            Kind::NewLine => "NewLine",
            Kind::Indent => "Indent",
            Kind::Dedent => "Dedent",
            Kind::Identifier => "Identifier",
            Kind::StringLiteral => "StringLiteral",
            Kind::FString => "FString",
            Kind::Bytes => "Bytes",
            Kind::RawString => "RawString",
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
            Kind::Indent => "Indent",
            Kind::Dedent => "Dedent",
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
            Kind::FString => "FString",
            Kind::Bytes => "Bytes",
            Kind::RawString => "RawString",
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
            Kind::And => "and",
            Kind::As => "as",
            Kind::Assert => "assert",
            Kind::Async => "async",
            Kind::Await => "await",
            Kind::Break => "break",
            Kind::Class => "class",
            Kind::Continue => "continue",
            Kind::Def => "def",
            Kind::Del => "del",
            Kind::Elif => "elif",
            Kind::Else => "else",
            Kind::Except => "except",
            Kind::Finally => "finally",
            Kind::For => "for",
            Kind::From => "from",
            Kind::Global => "global",
            Kind::If => "if",
            Kind::Import => "import",
            Kind::In => "in",
            Kind::Is => "is",
            Kind::Lambda => "lambda",
            Kind::Nonlocal => "nonlocal",
            Kind::Not => "not",
            Kind::Or => "or",
            Kind::Pass => "pass",
            Kind::Raise => "raise",
            Kind::Return => "return",
            Kind::Try => "try",
            Kind::While => "while",
            Kind::With => "with",
            Kind::Yield => "yield",
            Kind::StringLiteral => "StringLiteral",
            Kind::FString => "FString",
            Kind::Bytes => "Bytes",
            Kind::RawString => "RawString",
            Kind::RawFString => "RawFString",
            Kind::RawBytes => "RawBytes",
            Kind::Unicode => "Unicode",
            Kind::Integer => "Integer",
            Kind::Binary => "Binary",
            Kind::Octal => "Octal",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    None,
    Number(String), // TODO: String because we don't need the value yet
    Str(String),
    Indent(usize),
}

impl TokenValue {
    pub fn to_string(&self) -> String {
        match self {
            TokenValue::None => "None".to_string(),
            TokenValue::Number(n) => n.to_string(),
            TokenValue::Str(s) => s.to_string(),
            TokenValue::Indent(i) => panic!("not a string"),
        }
    }
}
