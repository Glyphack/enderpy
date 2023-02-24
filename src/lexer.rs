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
                                self.next();
                            }
                            _ => break,
                        }
                    }
                    return self.match_keyword(&ident);
                }
                // Operators
                '+' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Kind::AddAssign;
                    }
                    _ => return Kind::Plus,
                },
                '-' => match self.peek() {
                    Some('>') => {
                        self.next();
                        return Arrow;
                    }
                    Some('=') => {
                        self.next();
                        return SubAssign;
                    }
                    _ => return Minus,
                },
                '*' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return MulAssign;
                    }
                    Some('*') => match self.double_peek() {
                        Some('=') => {
                            self.double_next();
                            return PowAssign;
                        }
                        _ => {
                            self.next();
                            return Pow;
                        }
                    },
                    _ => return Mul,
                },
                '/' => match self.peek() {
                    Some('/') => {
                        match self.double_peek() {
                            Some('=') => {
                                self.double_next();
                                return IntDivAssign;
                            }
                            _ => {}
                        }
                        self.next();
                        return IntDiv;
                    }
                    Some('=') => {
                        self.next();
                        return DivAssign;
                    }
                    _ => return Div,
                },
                '%' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return ModAssign;
                    }
                    _ => return Mod,
                },
                '@' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return MatrixMulAssign;
                    }
                    _ => return MatrixMul,
                },
                '&' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return BitAndAssign;
                    }
                    _ => return BitAnd,
                },
                '|' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return BitOrAssign;
                    }
                    _ => return BitOr,
                },
                '^' => match self.peek() {
                    Some('=') => {
                        self.next();
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
                '<' => match self.peek() {
                    Some('<') => match self.double_peek() {
                        Some('=') => {
                            self.double_next();
                            return ShiftLeftAssign;
                        }
                        _ => {
                            self.next();
                            return LeftShift;
                        }
                    },
                    _ => {}
                },
                '>' => match self.peek() {
                    Some('>') => match self.double_peek() {
                        Some('=') => {
                            self.double_next();
                            return ShiftRightAssign;
                        }
                        _ => {
                            self.next();
                            return RightShift;
                        }
                    },
                    _ => {}
                },
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

    fn next(&mut self) -> Option<char> {
        let c = self.remaining.chars().next();
        if let Some(c) = c {
            self.remaining = self.remaining[c.len_utf8()..].to_string();
        }
        c
    }

    fn double_next(&mut self) -> Option<char> {
        self.next();
        self.next()
    }

    fn peek(&self) -> Option<char> {
        self.remaining.chars().next()
    }

    fn double_peek(&self) -> Option<char> {
        self.remaining.chars().nth(1)
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
    use insta::assert_debug_snapshot;

    fn snapshot_test_lexer(inputs: &[&str]) {
        for input in inputs.iter() {
            let mut lexer = Lexer::new(input);
            let mut tokens = vec![];
            loop {
                let token = lexer.read_next_token();
                if token.kind == Kind::Eof {
                    break;
                }
                tokens.push(token);
            }
            insta::with_settings!({
                description => input.to_string(), // the template source code
                omit_expression => true // do not include the default expression
            }, {
                    assert_debug_snapshot!(tokens);
            });
        }
    }

    #[test]
    fn test_lexer() {
        snapshot_test_lexer(&[
            "1+2",
            "a+b",
            "a + b",
            "+=2",
            "xX = 2",
            "if else elif",
            "()",
            "[]",
            "{}:",
            ".",
            ",",
            ";",
            "@",
            "=",
            "'",
            "\\",
            "\"",
            "#",
            "\\",
            "$",
            "?",
            "`",
            "->",
            "+=",
            "-=",
            "*=",
            "/=",
            "%=",
            "@=",
            "&=",
            "|=",
            "^=",
            "//=",
            "<<=",
            ">>=",
            "**=",
            "**",
            "//",
            "<<",
            ">>",
        ]);
    }
}
