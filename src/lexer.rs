#[derive(Debug)]
pub struct Token {
    pub kind: Kind,
    pub value: TokenValue,
}

// TODO: remove this after implementing all the tokens
#[allow(dead_code)]
// https://docs.python.org/3/reference/lexical_analysis.html
#[derive(Debug, PartialEq)]
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

    // Literals
    StringLiteral, // "string"
    Integer,       // 123
    Float,         // 123.456
    Imaginary,     // 123j

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
    More,       // >
    LessEq,     // <=
    MoreEq,     // >=
    Eq,         // ==
    NotEq,      // !=

    // Delimiters
    LeftParen,           // (
    RightParen,          // )
    LeftBrace,           // [
    RightBrace,          // ]
    LeftBracket,         // {
    RightBracket,        // }
    Comma,               // ,
    Colon,               // :
    Dot,                 // .
    SemiColon,           // ;
    At,                  // @
    Assign,              // =
    Arrow,               // ->
    AddAssign,           // +=
    SubAssign,           // -=
    MulAssign,           // *=
    DivAssign,           // /=
    IntDivAssign,        // //=
    ModAssign,           // %=
    MatrixMulAssign,     // @=
    BitAndAssign,        // &=
    BitOrAssign,         // |=
    BitXorAssign,        // ^=
    BitShiftLeftAssign,  // <<=
    BitShiftRightAssign, // >>=
    PowAssign,           // **=

    // Special
    SingleQuote, // '
    DoubleQuote, // "
    Sharp,       // #
    BackSlash,   // \

    // Unconditional error
    Dollar,       // $
    QuestionMark, // ?
    BackTick,     // `

    // Others
    WhiteSpace,
    Eof,
}

#[derive(Debug, PartialEq)]
pub enum TokenValue {
    None,
    Integer(i64),
    String(String),
}

pub struct Lexer {
    source: String,
    remaining: String,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            remaining: source.to_string(),
        }
    }

    fn read_next_kind(&mut self) -> Kind {
        use Kind::*;

        while let Some(c) = self.remaining.chars().next() {
            self.remaining = self.remaining[c.len_utf8()..].to_string();

            if c.is_whitespace() {
                return Kind::WhiteSpace;
            }

            match c {
                '+' => match self.peek() {
                    Some('=') => {
                        self.remaining = self.remaining[c.len_utf8()..].to_string();
                        return Kind::AddAssign;
                    }
                    _ => return Kind::Plus,
                },
                // match a number
                '0'..='9' => return Kind::Integer,
                // match keywords
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut ident = String::new();
                    ident.push(c);
                    while let Some(c) = self.peek() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                                ident.push(c);
                                self.remaining = self.remaining[c.len_utf8()..].to_string();
                            }
                            _ => break,
                        }
                    }
                    return self.match_keyword(&ident);
                }
                '(' => return LeftParen,
                ')' => return RightParen,
                '[' => return LeftBrace,
                ']' => return RightBrace,
                '{' => return LeftBracket,
                '}' => return RightBracket,
                ',' => return Comma,
                ':' => return Colon,
                '.' => return Dot,
                ';' => return SemiColon,
                '@' => return At,
                '=' => return Assign,
                '\'' => return SingleQuote,
                '"' => return DoubleQuote,
                '#' => return Sharp,
                '\\' => return BackSlash,
                '$' => return Dollar,
                '?' => return QuestionMark,
                '`' => return BackTick,
                _ => {}
            }
        }

        Kind::Eof
    }

    pub fn read_next_token(&mut self) -> Token {
        let start = self.offset();
        let kind = self.read_next_kind();
        let end = self.offset();

        if kind == Kind::WhiteSpace {
            return self.read_next_token();
        }

        let value = match kind {
            Kind::Integer => {
                let value = self.source[start..end].parse::<i64>().unwrap();
                TokenValue::Integer(value)
            }
            Kind::Identifier => {
                let value = self.source[start..end].to_string();
                TokenValue::String(value)
            }
            _ => TokenValue::None,
        };
        Token { kind, value }
    }

    /// Get the length offset from the source text, in UTF-8 bytes
    fn offset(&self) -> usize {
        self.source.len() - self.remaining.chars().as_str().len()
    }

    fn peek(&self) -> Option<char> {
        self.remaining.chars().next()
    }

    fn match_keyword(&self, ident: &str) -> Kind {
        match ident {
            "if" => Kind::If,
            "else" => Kind::Else,
            "elif" => Kind::Elif,
            _ => Kind::Identifier,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Kind;
    use super::Lexer;
    use super::TokenValue;

    #[test]
    fn test_num_plus_num() {
        let mut lexer = Lexer::new("1+2");
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Integer);
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Plus);
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Integer);
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Eof);
    }

    #[test]
    fn test_add_assign() {
        let mut lexer = Lexer::new("+=2");
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::AddAssign);
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Integer);
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Eof);
    }

    #[test]
    fn test_assign() {
        let mut lexer = Lexer::new("xX = 2");
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Identifier);
        if let TokenValue::String(value) = token.value {
            assert_eq!(value, "xX");
        }
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Eq);
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Integer);
        if let TokenValue::Integer(value) = token.value {
            assert_eq!(value, 2);
        }
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Eof);
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("if else elif");
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::If);
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Else);
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Elif);
        let token = lexer.read_next_token();
        assert_eq!(token.kind, Kind::Eof);
    }

    #[test]
    fn test_single_delimiters() {
        let mut lexer = Lexer::new("()[]{}:.,;@='\"#\\$?`");
        let kinds = vec![
            Kind::LeftParen,
            Kind::RightParen,
            Kind::LeftBrace,
            Kind::RightBrace,
            Kind::LeftBracket,
            Kind::RightBracket,
            Kind::Colon,
            Kind::Dot,
            Kind::Comma,
            Kind::SemiColon,
            Kind::At,
            Kind::Assign,
            Kind::SingleQuote,
            Kind::DoubleQuote,
            Kind::Sharp,
            Kind::BackSlash,
            Kind::Dollar,
            Kind::QuestionMark,
            Kind::BackTick,
            Kind::Eof,
        ];
        for kind in kinds {
            let token = lexer.read_next_token();
            assert_eq!(token.kind, kind);
        }
    }
}
