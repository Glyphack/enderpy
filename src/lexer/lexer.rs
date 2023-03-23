use crate::token::{Kind, Token, TokenValue};
use miette::Result;
use unicode_id_start::{is_id_continue, is_id_start};

use super::diagnostics::SyntaxError;

#[derive(Debug)]
pub struct Lexer {
    /// The source code
    source: String,
    /// The current position in the source code
    current: usize,
    /// Keeps track of whether the lexer is at the start of a line
    start_of_line: bool,
    /// keeps track of the indentation level
    /// the first element is always 0
    /// because the first line is always at indentation level 0
    indent_stack: Vec<usize>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            current: 0,
            start_of_line: true,
            indent_stack: vec![0],
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        let start = self.current;
        let kind = self.next_kind()?;
        // Ignore whitespace
        if kind == Kind::WhiteSpace {
            return self.next_token();
        }
        let raw_value = self.extract_raw_token_value(start);
        let value = self.parse_token_value(kind, raw_value);
        let end = self.current;
        Ok(Token {
            kind,
            value,
            start,
            end,
        })
    }

    pub fn peek_token(&mut self) -> Result<Token> {
        let current = self.current;
        let token = self.next_token();
        self.current = current;
        token
    }

    fn next_kind(&mut self) -> Result<Kind> {
        if self.start_of_line {
            if let Some(indent_kind) = self.match_indentation() {
                self.start_of_line = false; // WHY!?
                return Ok(indent_kind);
            }
        }

        while let Some(c) = self.next() {
            self.start_of_line = false; // WHY AGAIN?!

            match c {
                // Numbers
                '0'..='9' => return Ok(self.match_numeric_literal()),
                // Identifiers & Keywords
                id_start @ 'a'..='z' | id_start @ 'A'..='Z' | id_start @ '_' => {
                    return self.match_id_keyword(id_start);
                }
                // String Literals
                str_start @ '"' | str_start @ '\'' => {
                    self.skip_to_str_end(str_start)?;
                    return Ok(Kind::StringLiteral);
                }
                // Operators
                '+' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::AddAssign);
                    }
                    _ => return Ok(Kind::Plus),
                },
                '-' => match self.peek() {
                    Some('>') => {
                        self.next();
                        return Ok(Kind::Arrow);
                    }
                    Some('=') => {
                        self.next();
                        return Ok(Kind::SubAssign);
                    }
                    _ => return Ok(Kind::Minus),
                },
                '*' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::MulAssign);
                    }
                    Some('*') => match self.double_peek() {
                        Some('=') => {
                            self.double_next();
                            return Ok(Kind::PowAssign);
                        }
                        _ => {
                            self.next();
                            return Ok(Kind::Pow);
                        }
                    },
                    _ => return Ok(Kind::Mul),
                },
                '/' => match self.peek() {
                    Some('/') => {
                        if let Some('=') = self.double_peek() {
                            self.double_next();
                            return Ok(Kind::IntDivAssign);
                        }
                        self.next();
                        return Ok(Kind::IntDiv);
                    }
                    Some('=') => {
                        self.next();
                        return Ok(Kind::DivAssign);
                    }
                    _ => return Ok(Kind::Div),
                },
                '%' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::ModAssign);
                    }
                    _ => return Ok(Kind::Mod),
                },
                '@' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::MatrixMulAssign);
                    }
                    _ => return Ok(Kind::MatrixMul),
                },
                '&' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::BitAndAssign);
                    }
                    _ => return Ok(Kind::BitAnd),
                },
                '|' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::BitOrAssign);
                    }
                    _ => return Ok(Kind::BitOr),
                },
                '^' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::BitXorAssign);
                    }
                    _ => return Ok(Kind::BitXor),
                },
                '~' => return Ok(Kind::BitNot),
                ':' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::Walrus);
                    }
                    _ => return Ok(Kind::Colon),
                },
                '!' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        return Ok(Kind::NotEq);
                    }
                }
                // Delimiters
                '(' => return Ok(Kind::LeftParen),
                ')' => return Ok(Kind::RightParen),
                '[' => return Ok(Kind::LeftBrace),
                ']' => return Ok(Kind::RightBrace),
                '{' => return Ok(Kind::LeftBracket),
                '}' => return Ok(Kind::RightBracket),
                ',' => return Ok(Kind::Comma),
                '.' => return Ok(Kind::Dot),
                ';' => return Ok(Kind::SemiColon),
                '=' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::Eq);
                    }
                    _ => return Ok(Kind::Assign),
                },
                '#' => return Ok(Kind::Sharp),
                '\\' => return Ok(Kind::BackSlash),
                '$' => return Ok(Kind::Dollar),
                '?' => return Ok(Kind::QuestionMark),
                '`' => return Ok(Kind::BackTick),
                '<' => match self.peek() {
                    Some('<') => match self.double_peek() {
                        Some('=') => {
                            self.double_next();
                            return Ok(Kind::ShiftLeftAssign);
                        }
                        _ => {
                            self.next();
                            return Ok(Kind::LeftShift);
                        }
                    },
                    Some('=') => {
                        self.next();
                        return Ok(Kind::LessEq);
                    }
                    _ => return Ok(Kind::Less),
                },
                '>' => match self.peek() {
                    Some('>') => match self.double_peek() {
                        Some('=') => {
                            self.double_next();
                            return Ok(Kind::ShiftRightAssign);
                        }
                        _ => {
                            self.next();
                            return Ok(Kind::RightShift);
                        }
                    },
                    Some('=') => {
                        self.next();
                        return Ok(Kind::GreaterEq);
                    }
                    _ => return Ok(Kind::Greater),
                },
                '\n' | '\r' => {
                    self.start_of_line = true;
                    return Ok(Kind::NewLine);
                }
                c if match_whitespace(c) => return Ok(Kind::WhiteSpace),
                _ => {}
            }
        }

        Ok(Kind::Eof)
    }

    fn match_id_keyword(&mut self, id_start: char) -> Result<Kind> {
        if let Some(str_kind) = self.match_str(id_start)? {
            return Ok(str_kind);
        }
        let id = self.extract_id(id_start);
        Ok(self.match_keyword(&id))
    }

    fn extract_id(&mut self, id_start: char) -> String {
        let mut id = String::new();
        id.push(id_start);
        while let Some(c) = self.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    id.push(c);
                    self.next();
                }
                _ => {
                    // Some unicode characters are valid in identifiers
                    // https://boshen.github.io/javascript-parser-in-rust/docs/lexer/#identifiers-and-unicode
                    if is_id_start(c) & is_id_continue(c) {
                        id.push(c);
                        self.next();
                    } else {
                        break;
                    }
                }
            }
        }
        id
    }

    fn match_str(&mut self, id_start: char) -> Result<Option<Kind>> {
        match id_start {
            'r' | 'R' => match self.peek() {
                // check if is start of string
                Some('b') | Some('B') => match self.double_peek() {
                    Some(str_start @ '"') | Some(str_start @ '\'') => {
                        self.double_next();
                        self.skip_to_str_end(str_start)?;
                        return Ok(Some(Kind::RawBytes));
                    }
                    _ => {}
                },
                Some('f') | Some('F') => match self.double_peek() {
                    Some(str_start @ '"') | Some(str_start @ '\'') => {
                        self.double_next();
                        self.skip_to_str_end(str_start)?;
                        return Ok(Some(Kind::RawFString));
                    }
                    _ => {}
                },
                Some(str_start @ '"') | Some(str_start @ '\'') => {
                    self.next();
                    self.skip_to_str_end(str_start)?;
                    return Ok(Some(Kind::RawString));
                }
                _ => {}
            },
            'b' | 'B' => match self.peek() {
                Some('r') | Some('R') => match self.double_peek() {
                    Some(str_start @ '"') | Some(str_start @ '\'') => {
                        self.double_next();
                        self.skip_to_str_end(str_start)?;
                        return Ok(Some(Kind::RawBytes));
                    }
                    _ => {}
                },
                Some(str_start @ '"') | Some(str_start @ '\'') => {
                    self.next();
                    self.skip_to_str_end(str_start)?;
                    return Ok(Some(Kind::Bytes));
                }
                _ => {}
            },
            'f' | 'F' => match self.peek() {
                Some('r') | Some('R') => match self.double_peek() {
                    Some(str_start @ '"') | Some(str_start @ '\'') => {
                        self.double_next();
                        self.skip_to_str_end(str_start)?;
                        return Ok(Some(Kind::RawFString));
                    }
                    _ => {}
                },
                Some(str_start @ '"') | Some(str_start @ '\'') => {
                    self.next();
                    self.skip_to_str_end(str_start)?;
                    return Ok(Some(Kind::FString));
                }
                _ => {}
            },
            'u' | 'U' => match self.peek() {
                Some(str_start @ '"') | Some(str_start @ '\'') => {
                    self.next();
                    self.skip_to_str_end(str_start)?;
                    return Ok(Some(Kind::Unicode));
                }
                _ => {}
            },
            _ => {}
        };
        Ok(None)
    }

    fn extract_raw_token_value(&mut self, start: usize) -> String {
        self.source[start..self.current].to_string()
    }

    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        if let Some(c) = c {
            self.current += c.len_utf8();
        }
        c
    }

    fn double_next(&mut self) -> Option<char> {
        self.next();
        self.next()
    }

    fn peek(&self) -> Option<char> {
        self.source[self.current..].chars().next()
    }

    fn double_peek(&self) -> Option<char> {
        self.source[self.current..].chars().nth(1)
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

    fn skip_to_str_end(&mut self, str_start: char) -> Result<()> {
        // Check if string starts with triple quotes
        let mut string_terminated = false;
        let mut last_read_char = str_start;
        if self.peek() == Some(str_start) && self.double_peek() == Some(str_start) {
            self.next();
            while let Some(c) = self.next() {
                if c == str_start
                    && self.peek() == Some(str_start)
                    && self.double_peek() == Some(str_start)
                    && last_read_char != '\\'
                {
                    string_terminated = true;
                    self.double_next();
                    break;
                }
                last_read_char = c;
            }
        } else {
            while let Some(c) = self.next() {
                if c == str_start && last_read_char != '\\' {
                    string_terminated = true;
                    self.next();
                    break;
                }
                last_read_char = c;
            }
        }

        if !string_terminated {
            Err(SyntaxError("String not terminated").into())
        } else {
            Ok(())
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
                    if is_imaginary {
                        return Kind::ImaginaryPointFloat;
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

    fn match_indentation(&mut self) -> Option<Kind> {
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
                Ordering::Less => Some(Kind::Dedent),
                Ordering::Equal => None,
                Ordering::Greater => {
                    self.indent_stack.push(spaces_count);
                    Some(Kind::Indent)
                }
            }
        } else {
            None
        }
    }

    fn parse_token_value(&mut self, kind: Kind, kind_value: String) -> TokenValue {
        use std::cmp::Ordering;
        match kind {
            Kind::Integer
            | Kind::Hexadecimal
            | Kind::Binary
            | Kind::PointFloat
            | Kind::Octal
            | Kind::ExponentFloat
            | Kind::ImaginaryInteger
            | Kind::ImaginaryExponentFloat
            | Kind::ImaginaryPointFloat => TokenValue::Number(kind_value),
            Kind::Identifier => TokenValue::Str(kind_value),
            Kind::StringLiteral
            | Kind::FString
            | Kind::RawBytes
            | Kind::RawString
            | Kind::RawFString
            | Kind::Bytes
            | Kind::Unicode => TokenValue::Str(kind_value),
            Kind::Dedent => {
                let mut spaces_count = 0;
                for c in kind_value.chars() {
                    match c {
                        '\t' => {
                            spaces_count += 4;
                        }
                        ' ' => {
                            spaces_count += 1;
                        }
                        _ => {
                            break;
                        }
                    }
                }
                let mut de_indents = 0;
                while let Some(top) = self.indent_stack.last() {
                    match top.cmp(&spaces_count) {
                        Ordering::Greater => {
                            self.indent_stack.pop();
                            de_indents += 1;
                        }
                        Ordering::Equal => break,
                        Ordering::Less => panic!("Invalid indentation"),
                    }
                }
                TokenValue::Indent(de_indents)
            }
            Kind::Indent => TokenValue::Indent(1),
            _ => TokenValue::None,
        }
    }
}

fn match_whitespace(c: char) -> bool {
    c.is_whitespace() && c != '\n' && c != '\r'
}

#[cfg(test)]
mod tests {
    use super::Kind;
    use super::Lexer;
    use insta::assert_debug_snapshot;
    use miette::Result;

    fn snapshot_test_lexer(snap_name: &str, inputs: &[&str]) -> Result<()> {
        for (i, test_input) in inputs.iter().enumerate() {
            let mut lexer = Lexer::new(test_input);
            let mut tokens = vec![];
            loop {
                let token = lexer.next_token()?;
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
        Ok(())
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
        )
        .unwrap();

        // keywords
        snapshot_test_lexer(
            "keywords",
            &[
                "False None True and as assert async await",
                "break class continue def del elif else except",
                "finally for from global if import in is lambda",
                "nonlocal not or pass raise return try while with yield",
            ],
        )
        .unwrap();

        // Test identifiers
        snapshot_test_lexer(
            "identifiers",
            &["a", "a_a", "_a", "a_", "a_a_a", "a_a_", "ಠ_ಠ"],
        )
        .unwrap();
        // Invalid identifiers
        snapshot_test_lexer("invalid-identifiers", &["🦀"]).unwrap();

        // Test numeric literals
        // Binary
        snapshot_test_lexer(
            "numeric-binary",
            &[
                "0b0", "0b1", "0b10", "0b11", "0b100", "0b101", "0b110", "0b111",
            ],
        )
        .unwrap();

        // Octal
        snapshot_test_lexer(
            "numeric-octal",
            &["0o0", "0o1", "0o2", "0o3", "0o4", "0o5", "0o6", "0o7"],
        )
        .unwrap();

        // Hexadecimal
        snapshot_test_lexer(
            "numeric-hexadecimal",
            &[
                "0x0", "0x1", "0x2", "0x3", "0x4", "0x5", "0x6", "0x7", "0x8", "0x9", "0xa", "0xb",
                "0xc", "0xd", "0xe", "0xf", "0xA", "0xB", "0xC", "0xD", "0xE", "0xF",
            ],
        )
        .unwrap();
        //
        // Point float
        snapshot_test_lexer("numeric-floating-point", &["0.0 0.1 00.0 00.1 0.1j 0.01J"]).unwrap();

        // Exponent float
        snapshot_test_lexer("numeric-floating-exponent", &["0e0 0e-1 0e+2 0e+3j 0e+3J"]).unwrap();

        // Integer
        snapshot_test_lexer("numeric-integer", &["11 33 1j 1_000_000j"]).unwrap();

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
        )
        .unwrap();
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
        )
        .unwrap();

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
        )
        .unwrap();

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
        )
        .unwrap();

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
        )
        .unwrap();

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
        )
        .unwrap();

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
        )
        .unwrap();

        snapshot_test_lexer(
            "import",
            &["import a", "import a.b", "import a.b.c", "import a from b"],
        )
        .unwrap();
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
        )
        .unwrap();
    }

    #[test]
    #[should_panic]
    fn test_unterminated_string_double_quotes() {
        let mut lexer = Lexer::new("\"hello");
        lexer.next_token().unwrap();
    }

    #[test]
    #[should_panic]
    fn test_unterminated_string_single_quotes() {
        let mut lexer = Lexer::new("'hello");
        lexer.next_token().unwrap();
    }
    #[test]
    #[should_panic]
    fn test_unterminated_string_triple_single_quotes() {
        let mut lexer = Lexer::new("'''hello''");
        lexer.next_token().unwrap();
    }
    #[test]
    #[should_panic]
    fn test_unterminated_string_triple_single_quotes_2() {
        let mut lexer = Lexer::new("'''hello'");
        lexer.next_token().unwrap();
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
            let token = lexer.next_token().unwrap();
            if token.kind == Kind::Eof {
                break;
            }
        }
    }
}
