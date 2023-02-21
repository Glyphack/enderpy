#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Kind,
    pub value: TokenValue,
}

// TODO: remove this after implementing all the tokens
#[allow(dead_code)]
// https://docs.python.org/3/reference/lexical_analysis.html
#[derive(Debug, PartialEq, Clone)]
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
    Assign,              // =
    Arrow,               // ->
    AddAssign,           // +=
    SubAssign,           // -=
    MulAssign,           // *=
    DivAssign,           // /=
    ModAssign,           // %=
    MatrixMulAssign,     // @=
    BitAndAssign,        // &=
    BitOrAssign,         // |=
    BitXorAssign,        // ^=
    IntDivAssign,        // //=
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
    Comment,
    WhiteSpace,
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
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
                // Numbers
                '0'..='9' => return Kind::Integer,
                // Keywords
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut ident = String::new();
                    ident.push(c);
                    while let Some(c) = self.peek() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                                ident.push(c);
                                self.next_char();
                            }
                            _ => break,
                        }
                    }
                    return self.match_keyword(&ident);
                }
                // Operators
                '+' => match self.peek() {
                    Some('=') => {
                        self.next_char();
                        return Kind::AddAssign;
                    }
                    _ => return Kind::Plus,
                },
                '-' => match self.peek() {
                    Some('>') => {
                        self.next_char();
                        return Arrow;
                    }
                    Some('=') => {
                        self.next_char();
                        return SubAssign;
                    }
                    _ => return Minus,
                },
                '*' => match self.peek() {
                    Some('=') => {
                        self.next_char();
                        return MulAssign;
                    }
                    _ => return Mul,
                },
                '/' => match self.peek() {
                    Some('/') => {
                        self.next_char();
                        return Comment;
                    }
                    Some('=') => {
                        self.next_char();
                        return DivAssign;
                    }
                    _ => return Div,
                },
                '%' => match self.peek() {
                    Some('=') => {
                        self.next_char();
                        return ModAssign;
                    }
                    _ => return Mod,
                },
                '@' => match self.peek() {
                    Some('=') => {
                        self.next_char();
                        return MatrixMulAssign;
                    }
                    _ => return MatrixMul,
                },
                '&' => match self.peek() {
                    Some('=') => {
                        self.next_char();
                        return BitAndAssign;
                    }
                    _ => return BitAnd,
                },
                '|' => match self.peek() {
                    Some('=') => {
                        self.next_char();
                        return BitOrAssign;
                    }
                    _ => return BitOr,
                },
                '^' => match self.peek() {
                    Some('=') => {
                        self.next_char();
                        return BitXorAssign;
                    }
                    _ => return BitXor,
                },
                // Delimiters
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

    fn next_char(&mut self) -> Option<char> {
        let c = self.remaining.chars().next();
        if let Some(c) = c {
            self.remaining = self.remaining[c.len_utf8()..].to_string();
        }
        c
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

    #[test]
    fn test_num_plus_num() {
        test_kinds(
            "1+2",
            vec![Kind::Integer, Kind::Plus, Kind::Integer, Kind::Eof],
        );
    }

    #[test]
    fn test_add_assign() {
        test_kinds("+=2", vec![Kind::AddAssign, Kind::Integer, Kind::Eof]);
    }

    #[test]
    fn test_assign() {
        test_kinds(
            "xX = 2",
            vec![Kind::Identifier, Kind::Assign, Kind::Integer, Kind::Eof],
        );
    }

    #[test]
    fn test_keywords() {
        test_kinds(
            "if else elif",
            vec![Kind::If, Kind::Else, Kind::Elif, Kind::Eof],
        );
    }

    #[test]
    fn test_single_delimiters() {
        test_kinds(
            "()[]{}:.,;@ ='\"#\\$?`",
            vec![
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
                Kind::MatrixMul,
                Kind::Assign,
                Kind::SingleQuote,
                Kind::DoubleQuote,
                Kind::Sharp,
                Kind::BackSlash,
                Kind::Dollar,
                Kind::QuestionMark,
                Kind::BackTick,
                Kind::Eof,
            ],
        );
    }

    #[test]
    fn test_two_char_delimiters() {
        test_kinds(
            "-> += -= *= /= %= @= &= |= ^=",
            vec![
                Kind::Arrow,
                Kind::AddAssign,
                Kind::SubAssign,
                Kind::MulAssign,
                Kind::DivAssign,
                Kind::ModAssign,
                Kind::MatrixMulAssign,
                Kind::BitAndAssign,
                Kind::BitOrAssign,
                Kind::BitXorAssign,
                Kind::Eof,
            ],
        );
    }

    fn test_kinds(source: &str, kinds: Vec<Kind>) {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        let mut token = lexer.read_next_token();
        tokens.push(token.clone());
        while token.kind != Kind::Eof {
            token = lexer.read_next_token();
            tokens.push(token.clone());
        }
        assert_eq!(
            tokens.into_iter().map(|t| t.kind).collect::<Vec<Kind>>(),
            kinds
        )
    }
}
