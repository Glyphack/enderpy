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
        use unicode_id_start::{is_id_continue, is_id_start};
        use Kind::*;

        while let Some(c) = self.next() {
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
                            _ => {
                                // Some unicode characters are valid in identifiers
                                // https://boshen.github.io/javascript-parser-in-rust/docs/lexer/#identifiers-and-unicode
                                if is_id_start(c) & is_id_continue(c) {
                                    ident.push(c);
                                    self.next();
                                } else {
                                    break;
                                }
                            }
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
                '~' => return BitNot,
                ':' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Walrus;
                    }
                    _ => return Colon,
                },
                '!' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return NotEq;
                    }
                    _ => {}
                },
                // Delimiters
                '(' => return LeftParen,
                ')' => return RightParen,
                '[' => return LeftBrace,
                ']' => return RightBrace,
                '{' => return LeftBracket,
                '}' => return RightBracket,
                ',' => return Comma,
                '.' => return Dot,
                ';' => return SemiColon,
                '=' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Eq;
                    }
                    _ => return Assign,
                },
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
                    Some('=') => {
                        self.next();
                        return LessEq;
                    }
                    _ => return Less,
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
                    Some('=') => {
                        self.next();
                        return GreaterEq;
                    }
                    _ => return Greater,
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
            "False" => Kind::False,
            "None" => Kind::None,
            "True" => Kind::True,
            "and" => Kind::And,
            "as" => Kind::As,
            "assert" => Kind::Assert,
            "async" => Kind::Async,
            "await" => Kind::Await,
            "break" => Kind::Break,
            "class" => Kind::Class,
            "continue" => Kind::Continue,
            "def" => Kind::Def,
            "del" => Kind::Del,
            "elif" => Kind::Elif,
            "else" => Kind::Else,
            "except" => Kind::Except,
            "finally" => Kind::Finally,
            "for" => Kind::For,
            "from" => Kind::From,
            "global" => Kind::Global,
            "if" => Kind::If,
            "import" => Kind::Import,
            "in" => Kind::In,
            "is" => Kind::Is,
            "lambda" => Kind::Lambda,
            "nonlocal" => Kind::Nonlocal,
            "not" => Kind::Not,
            "or" => Kind::Or,
            "pass" => Kind::Pass,
            "raise" => Kind::Raise,
            "return" => Kind::Return,
            "try" => Kind::Try,
            "while" => Kind::While,
            "with" => Kind::With,
            "yield" => Kind::Yield,
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
            "+",
            "-",
            "*",
            "**",
            "/",
            "//",
            "%",
            "@",
            "<<",
            ">>",
            "&",
            "|",
            "^",
            "~",
            ":=",
            "<",
            ">",
            "<=",
            ">=",
            "==",
            "!=",
        ]);

        // keywords
        snapshot_test_lexer(&[
            "False None True and as assert async await",
            "break class continue def del elif else except",
            "finally for from global if import in is lambda",
            "nonlocal not or pass raise return try while with yield",
        ]);

        // Test identifiers
        snapshot_test_lexer(&["a", "a_a", "_a", "a_", "a_a_a", "a_a_", "ಠ_ಠ"]);
        // Invalid identifiers
        snapshot_test_lexer(&["🦀"])
    }
}
