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
    Comment,
    WhiteSpace,
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    None,
    Number(String), // TODO: String because we don't need the value yet
    String(String),
    Indent(usize),
}

pub struct Lexer {
    source: String,
    remaining: String,
    start_of_line: bool,
    indent_stack: Vec<usize>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            remaining: source.to_string(),
            start_of_line: true,
            indent_stack: vec![0],
        }
    }

    fn read_next_kind(&mut self) -> Kind {
        use unicode_id_start::{is_id_continue, is_id_start};
        use Kind::*;

        while let Some(c) = self.next() {
            if c.is_whitespace() && c != '\n' && c != '\r' {
                return Kind::WhiteSpace;
            }

            match c {
                // Numbers
                '0'..='9' => {
                    return self.match_numeric_literal();
                }
                // Keywords
                identifier_start @ 'a'..='z'
                | identifier_start @ 'A'..='Z'
                | identifier_start @ '_' => {
                    // Check if is start of string
                    match identifier_start {
                        'r' | 'R' => match self.peek() {
                            // check if is start of string
                            Some('b') | Some('B') => match self.double_peek() {
                                Some(str_starter @ '"') | Some(str_starter @ '\'') => {
                                    self.next();
                                    self.next();
                                    self.consume_string_from_start_char(str_starter);
                                    return Kind::RawBytes;
                                }
                                _ => {}
                            },
                            Some('f') | Some('F') => match self.double_peek() {
                                Some(str_starter @ '"') | Some(str_starter @ '\'') => {
                                    self.next();
                                    self.next();
                                    self.consume_string_from_start_char(str_starter);
                                    return Kind::RawFString;
                                }
                                _ => {}
                            },
                            Some(str_starter @ '"') | Some(str_starter @ '\'') => {
                                self.next();
                                self.consume_string_from_start_char(str_starter);
                                return Kind::RawString;
                            }
                            _ => {}
                        },
                        'b' | 'B' => match self.peek() {
                            Some('r') | Some('R') => match self.double_peek() {
                                Some(str_starter @ '"') | Some(str_starter @ '\'') => {
                                    self.next();
                                    self.next();
                                    self.consume_string_from_start_char(str_starter);
                                    return Kind::RawBytes;
                                }
                                _ => {}
                            },
                            Some(str_starter @ '"') | Some(str_starter @ '\'') => {
                                self.next();
                                self.consume_string_from_start_char(str_starter);
                                return Kind::Bytes;
                            }
                            _ => {}
                        },
                        'f' | 'F' => match self.peek() {
                            Some('r') | Some('R') => match self.double_peek() {
                                Some(str_starter @ '"') | Some(str_starter @ '\'') => {
                                    self.next();
                                    self.next();
                                    self.consume_string_from_start_char(str_starter);
                                    return Kind::RawFString;
                                }
                                _ => {}
                            },
                            Some(str_starter @ '"') | Some(str_starter @ '\'') => {
                                self.next();
                                self.consume_string_from_start_char(str_starter);
                                return Kind::FString;
                            }
                            _ => {}
                        },
                        'u' | 'U' => match self.peek() {
                            Some(str_starter @ '"') | Some(str_starter @ '\'') => {
                                self.next();
                                self.consume_string_from_start_char(str_starter);
                                return Kind::Unicode;
                            }
                            _ => {}
                        },
                        _ => {}
                    };

                    let mut ident = String::new();
                    ident.push(identifier_start);
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
                // String Literals
                str_starter @ '"' | str_starter @ '\'' => {
                    self.consume_string_from_start_char(str_starter);
                    return StringLiteral;
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
                        if let Some('=') = self.double_peek() {
                            self.double_next();
                            return IntDivAssign;
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
                '!' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        return NotEq;
                    }
                }
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
                '\n' | '\r' => {
                    return NewLine;
                }
                ' ' | '\t' => {
                    // indentation case is handled above before the match
                    // because there might be no whitespace after newline and
                    // still a Dedent token is needed
                    return WhiteSpace;
                }
                _ => {}
            }
        }

        Kind::Eof
    }

    pub fn read_next_token(&mut self) -> Token {
        // Check if we are at the start of a line
        if self.is_at_line_start() {
            if let Some(token) = self.handle_indentation() {
                self.start_of_line = false;
                return token;
            }
        }

        let start = self.offset();
        let kind = self.read_next_kind();
        let end = self.offset();

        if kind == Kind::WhiteSpace {
            return self.read_next_token();
        }

        let value = match kind {
            Kind::Integer
            | Kind::Hexadecimal
            | Kind::Binary
            | Kind::PointFloat
            | Kind::Octal
            | Kind::ExponentFloat
            | Kind::ImaginaryInteger
            | Kind::ImaginaryExponentFloat
            | Kind::ImaginaryPointFloat => {
                let value = self.source[start..end].to_string();
                TokenValue::Number(value)
            }
            Kind::Identifier => {
                let value = self.source[start..end].to_string();
                TokenValue::String(value)
            }
            Kind::StringLiteral
            | Kind::FString
            | Kind::RawBytes
            | Kind::RawString
            | Kind::RawFString
            | Kind::Bytes
            | Kind::Unicode => {
                let value = self.source[start..end].to_string();
                TokenValue::String(value)
            }
            _ => TokenValue::None,
        };

        if kind == Kind::NewLine {
            self.start_of_line = true;
        } else {
            self.start_of_line = false;
        }
        Token { kind, value }
    }

    fn is_at_line_start(&self) -> bool {
        self.start_of_line
    }

    // TODO: Make sure we don't need to scape the source

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

    fn consume_string_from_start_char(&mut self, str_starter: char) {
        // Check if string starts with triple quotes
        let mut string_terminated = false;
        let mut last_read_char = str_starter;
        if self.peek() == Some(str_starter) && self.double_peek() == Some(str_starter) {
            self.next();
            while let Some(c) = self.next() {
                if c == str_starter
                    && self.peek() == Some(str_starter)
                    && self.double_peek() == Some(str_starter)
                    && last_read_char != '\\'
                {
                    string_terminated = true;
                    self.next();
                    self.next();
                    break;
                }
                last_read_char = c;
            }
        } else {
            while let Some(c) = self.next() {
                if c == str_starter && last_read_char != '\\' {
                    string_terminated = true;
                    self.next();
                    break;
                }
                last_read_char = c;
            }
        }

        if !string_terminated {
            panic!("String not terminated");
        }
    }

    fn match_numeric_literal(&mut self) -> Kind {
        match self.peek() {
            Some('b') | Some('B') => {
                self.next();
                while let Some(c) = self.peek() {
                    match c {
                        '0' | '1' => {
                            self.next();
                        }
                        '_' => {
                            self.next();
                        }
                        _ => panic!("Binary literals must be 0 or 1"),
                    }
                }
                return Kind::Binary;
            }
            Some('o') | Some('O') => {
                self.next();
                while let Some(c) = self.peek() {
                    match c {
                        '0'..='7' => {
                            self.next();
                        }
                        '_' => {
                            self.next();
                        }
                        _ => panic!("Octal literals must be 0-7"),
                    }
                }
                return Kind::Octal;
            }
            Some('x') | Some('X') => {
                self.next();
                while let Some(c) = self.peek() {
                    match c {
                        '0'..='9' | 'a'..='f' | 'A'..='F' => {
                            self.next();
                        }
                        '_' => {
                            self.next();
                        }
                        _ => panic!("Hexadecimal literals must be 0-9, a-f, A-F"),
                    }
                }
                return Kind::Hexadecimal;
            }
            _ => {}
        }

        let mut is_imaginary = false;
        while let Some(c) = self.peek() {
            match c {
                '.' => {
                    let mut has_exponent = false;
                    self.next();
                    while let Some(c) = self.peek() {
                        match c {
                            '0'..='9' => {
                                self.next();
                            }
                            '_' => {
                                self.next();
                            }
                            'e' | 'E' => {
                                has_exponent = true;
                                self.next();
                                match self.peek() {
                                    Some('+') | Some('-') => {
                                        self.next();
                                    }
                                    _ => panic!("Exponent must be + or -"),
                                }
                            }
                            'j' | 'J' => {
                                is_imaginary = true;
                                self.next();
                                break;
                            }
                            _ => break,
                        }
                    }
                    if has_exponent {
                        if is_imaginary {
                            return Kind::ImaginaryExponentFloat;
                        }
                        return Kind::ExponentFloat;
                    }
                    return Kind::PointFloat;
                }
                'e' | 'E' => {
                    self.next();
                    match self.peek() {
                        Some('+') | Some('-') => {
                            self.next();
                        }
                        _ => {}
                    }
                    while let Some(c) = self.peek() {
                        match c {
                            '0'..='9' | '_' => {
                                self.next();
                            }
                            'j' | 'J' => {
                                is_imaginary = true;
                                self.next();
                                break;
                            }
                            _ => break,
                        }
                    }
                    if is_imaginary {
                        return Kind::ImaginaryExponentFloat;
                    }
                    return Kind::ExponentFloat;
                }
                '0'..='9' => {
                    self.next();
                }
                '_' => {
                    self.next();
                }
                'j' | 'J' => {
                    is_imaginary = true;
                    self.next();
                    break;
                }
                _ => break,
            }
        }

        if is_imaginary {
            return Kind::ImaginaryInteger;
        }

        Kind::Integer
    }

    fn handle_indentation(&mut self) -> Option<Token> {
        use std::cmp::Ordering;
        let mut spaces_count = 0;
        while let Some(c) = self.peek() {
            match c {
                '\t' => {
                    spaces_count += 4;
                    self.next();
                }
                ' ' => {
                    spaces_count += 1;
                    self.next();
                }
                _ => {
                    break;
                }
            }
        }
        if let Some(top) = self.indent_stack.last() {
            match spaces_count.cmp(top) {
                Ordering::Less => {
                    let mut de_indents = 0;
                    while let Some(top) = self.indent_stack.last() {
                        match top.cmp(&spaces_count) {
                            Ordering::Greater => {
                                self.indent_stack.pop();
                                de_indents += 1;
                            }
                            Ordering::Equal => {
                                break;
                            }
                            Ordering::Less => {
                                panic!("Invalid indentation");
                            }
                        }
                    }
                    let kind = Kind::Dedent;
                    let value = TokenValue::Indent(de_indents);
                    Some(Token { kind, value })
                }
                Ordering::Equal => None,
                Ordering::Greater => {
                    self.indent_stack.push(spaces_count);
                    let kind = Kind::Indent;
                    let value = TokenValue::Indent(1);
                    Some(Token { kind, value })
                }
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Kind;
    use super::Lexer;
    use insta::assert_debug_snapshot;

    fn snapshot_test_lexer(snap_name: &str, inputs: &[&str]) {
        for (i, test_input) in inputs.iter().enumerate() {
            let mut lexer = Lexer::new(test_input);
            let mut tokens = vec![];
            loop {
                let token = lexer.read_next_token();
                if token.kind == Kind::Eof {
                    break;
                }
                tokens.push(token);
            }
            let snap_file_name = format!("{}-{}", snap_name, i);
            insta::with_settings!({
                description => test_input.to_string(), // the template source code
                omit_expression => true // do not include the default expression
            }, {
                    assert_debug_snapshot!(snap_file_name, tokens);
            });
        }
    }

    #[test]
    fn test_lexer() {
        snapshot_test_lexer(
            "test-lexer",
            &[
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
                "\\",
                "#",
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
            ],
        );

        // keywords
        snapshot_test_lexer(
            "keywords",
            &[
                "False None True and as assert async await",
                "break class continue def del elif else except",
                "finally for from global if import in is lambda",
                "nonlocal not or pass raise return try while with yield",
            ],
        );

        // Test identifiers
        snapshot_test_lexer(
            "identifiers",
            &["a", "a_a", "_a", "a_", "a_a_a", "a_a_", "ಠ_ಠ"],
        );
        // Invalid identifiers
        snapshot_test_lexer("invalid-identifiers", &["🦀"]);

        // Test numeric literals
        // Binary
        snapshot_test_lexer(
            "numeric-binary",
            &[
                "0b0", "0b1", "0b10", "0b11", "0b100", "0b101", "0b110", "0b111",
            ],
        );

        // Octal
        snapshot_test_lexer(
            "numeric-octal",
            &["0o0", "0o1", "0o2", "0o3", "0o4", "0o5", "0o6", "0o7"],
        );

        // Hexadecimal
        snapshot_test_lexer(
            "numeric-hexadecimal",
            &[
                "0x0", "0x1", "0x2", "0x3", "0x4", "0x5", "0x6", "0x7", "0x8", "0x9", "0xa", "0xb",
                "0xc", "0xd", "0xe", "0xf", "0xA", "0xB", "0xC", "0xD", "0xE", "0xF",
            ],
        );
        //
        // Point float
        snapshot_test_lexer("numeric-floating-point", &["0.0 0.1 00.0 00.1 0.1j 0.01J"]);

        // Exponent float
        snapshot_test_lexer("numeric-floating-exponent", &["0e0 0e-1 0e+2 0e+3j 0e+3J"]);

        // Integer
        snapshot_test_lexer("numeric-integer", &["11 33 1j 1_000_000j"]);

        // Strings
        snapshot_test_lexer(
            "string-literals",
            &[
                "\"hello\"",
                "\"world\"",
                "\"\"",
                "a = \"hello\"",
                "'hello'",
                "\"\"\"hello\"\"\"",
                "'''hello'''",
            ],
        );
        // F-strings
        snapshot_test_lexer(
            "f-string-literals",
            &[
                "f\"hello\"",
                "f\"world\"",
                "f\"\"",
                "a = f\"hello\"",
                "f'hello_{var}'",
                "f\"\"\"hello\"\"\"",
                "f'''hello'''",
            ],
        );

        // Bytes
        snapshot_test_lexer(
            "bytes-literals",
            &[
                "b\"hello\"",
                "b\"world\"",
                "b\"\"",
                "a = b\"hello\"",
                "b'hello'",
                "b\"\"\"hello\"\"\"",
                "b'''hello'''",
            ],
        );

        // Raw strings
        snapshot_test_lexer(
            "raw-string-literals",
            &[
                "r\"hello\"",
                "r\"world\"",
                "r\"\"",
                "a = r\"hello\"",
                "r'hello'",
                "r\"\"\"hello\"\"\"",
                "r'''hello'''",
            ],
        );

        // Raw F-strings
        snapshot_test_lexer(
            "raw-f-string-literals",
            &[
                "rf\"hello\"",
                "rf\"world\"",
                "rf\"\"",
                "a = rf\"hello\"",
                "rf'hello_{var}'",
                "rf\"\"\"hello\"\"\"",
                "rf'''hello'''",
            ],
        );

        // Raw bytes
        snapshot_test_lexer(
            "raw-bytes-literals",
            &[
                "rb\"hello\"",
                "rb\"world\"",
                "rb\"\"",
                "a = rb\"hello\"",
                "rb'hello'",
                "rb\"\"\"hello\"\"\"",
                "rb'''hello'''",
            ],
        );

        // Unicode strings
        snapshot_test_lexer(
            "unicode-string-literals",
            &[
                "u\"hello\"",
                "u\"world\"",
                "u\"\"",
                "a = u\"hello\"",
                "u'hello'",
                "u\"\"\"hello\"\"\"",
                "u'''hello'''",
            ],
        );
    }

    #[test]
    fn test_indentation() {
        // Indentation
        snapshot_test_lexer(
            "indentation",
            &[
                "if True:
            pass",
                "if True:
    pass
else:
    pass",
                "if True:
    if True:
        pass
def",
            ],
        );
    }

    #[test]
    #[should_panic]
    fn test_unterminated_string_double_quotes() {
        let mut lexer = Lexer::new("\"hello");
        lexer.read_next_token();
    }

    #[test]
    #[should_panic]
    fn test_unterminated_string_single_quotes() {
        let mut lexer = Lexer::new("'hello");
        lexer.read_next_token();
    }
    #[test]
    #[should_panic]
    fn test_unterminated_string_triple_single_quotes() {
        let mut lexer = Lexer::new("'''hello''");
        lexer.read_next_token();
    }
    #[test]
    #[should_panic]
    fn test_unterminated_string_triple_single_quotes_2() {
        let mut lexer = Lexer::new("'''hello'");
        lexer.read_next_token();
    }

    #[test]
    #[should_panic]
    fn test_unexpected_indentation() {
        let mut lexer = Lexer::new(
            "if True:
       pass
    pass",
        );
        loop {
            let token = lexer.read_next_token();
            if token.kind == Kind::Eof {
                break;
            }
        }
    }
}
