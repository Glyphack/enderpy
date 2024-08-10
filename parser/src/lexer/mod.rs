pub mod compat;

use unicode_id_start::{is_id_continue, is_id_start};

use crate::{
    error::LexError,
    token::{Kind, Token, TokenValue},
};

#[derive(Debug, Clone, Copy)]
enum StringQuotation {
    Single,
    Double,
    TripleSingle,
    TripleDouble,
}

impl StringQuotation {
    pub fn new(quote: char, count: u8) -> Self {
        if quote == '\'' {
            match count {
                1 => return Self::Single,
                3 => return Self::TripleSingle,
                _ => panic!("Invalid quotation string: {}", quote),
            }
        } else if quote == '"' {
            match count {
                1 => return Self::Double,
                3 => return Self::TripleDouble,
                _ => panic!("Invalid quotation string: {}", quote),
            }
        }
        panic!("Invalid quotation string: {}", quote);
    }
}

#[derive(Debug, Clone, Copy)]
enum TokenizationMode {
    Fstring((u8, StringQuotation)),
    FstringFormatSpecifier,
    PythonWithinFstring(u8),
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    /// The source code
    source: &'a str,
    /// The current position in the source code
    current: u32,
    current_line: u16,
    /// Keeps track of whether the lexer is at the start of a line
    start_of_line: bool,
    /// keeps track of the indentation level
    /// the first element is always 0
    /// because the first line is always at indentation level 0
    indent_stack: Vec<usize>,
    nesting: u8,
    tokenization_mode_stack: Vec<TokenizationMode>,
    // When not zero lexer is in de indent mode
    next_token_is_dedent: u8,
    /// Array of all line starts offsets. Starts from line 0
    pub line_starts: Vec<u32>,
    peak_mode: bool,

    /// Previous token was a Newline token
    non_logical_line_state: bool,
    /// Cursor at position after the indentation in line
    indented: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            current: 0,
            current_line: 1,
            start_of_line: true,
            indent_stack: vec![0],
            nesting: 0,
            tokenization_mode_stack: vec![],
            next_token_is_dedent: 0,
            line_starts: vec![0],
            peak_mode: false,
            non_logical_line_state: true,
            indented: false,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        loop {
            let token = self.next_token();
            if token.kind == Kind::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }

    pub fn next_token(&mut self) -> Token {
        if self.next_token_is_dedent > 0 {
            self.next_token_is_dedent -= 1;
            return Token {
                kind: Kind::Dedent,
                value: TokenValue::None,
                start: self.current,
                end: self.current,
            };
        }

        let start = self.current;
        let kind = match self.next_kind() {
            Ok(kind) => kind,
            Err(e) => {
                panic!("Invalid token {e}");
            }
        };

        if matches!(
            kind,
            Kind::RightParen | Kind::RightBrace | Kind::RightBracket
        ) {
            self.nesting -= 1;
        } else if matches!(kind, Kind::LeftParen | Kind::LeftBrace | Kind::LeftBracket) {
            self.nesting += 1;
        }

        // Ignore whitespace
        if kind == Kind::WhiteSpace {
            return self.next_token();
        }

        if kind != Kind::Comment && kind != Kind::NL && kind != Kind::Dedent {
            self.non_logical_line_state = kind == Kind::NewLine;
        }
        let value = self.parse_token_value(kind, start);
        let end = self.current;

        if kind == Kind::NewLine || kind == Kind::NL {
            self.line_starts.push(self.current);
        }

        Token {
            kind,
            value,
            start,
            end,
        }
    }

    // peek_token is a side-effect free version of next_token
    pub fn peek_token(&mut self) -> Token {
        let current = self.current;
        let current_line = self.current_line;
        let nesting = self.nesting;
        let start_of_line = self.start_of_line;
        let next_token_is_dedent = self.next_token_is_dedent;
        let prev_token_newline = self.non_logical_line_state;
        let indented = self.indented;
        self.peak_mode = true;
        let token = self.next_token();
        self.indented = indented;
        self.non_logical_line_state = prev_token_newline;
        self.peak_mode = false;
        self.current = current;
        self.current_line = current_line;
        self.nesting = nesting;
        self.start_of_line = start_of_line;
        self.next_token_is_dedent = next_token_is_dedent;
        token
    }

    // https://peps.python.org/pep-0701/#how-to-produce-these-new-tokens
    fn next_fstring_token(&mut self, str_finisher: StringQuotation, _fstring_nesting: u8) -> Kind {
        let mut read_chars = false;
        loop {
            let peeked_char = self.peek();
            let double_peek = self.double_peek();
            if peeked_char == Some('{') && peeked_char == double_peek {
                self.next();
                self.next();
                read_chars = true;
                continue;
            }
            if peeked_char == Some('{') && peeked_char != double_peek {
                if read_chars {
                    if !self.peak_mode {
                        self.tokenization_mode_stack
                            .push(TokenizationMode::PythonWithinFstring(self.nesting + 1));
                    }
                    return Kind::FStringMiddle;
                } else {
                    if !self.peak_mode {
                        self.tokenization_mode_stack
                            .push(TokenizationMode::PythonWithinFstring(self.nesting + 1));
                    }
                    self.next();
                    return Kind::LeftBracket;
                }
            }

            let Some(curr) = self.next() else {
                panic!("eof while parsing fstring")
            };
            read_chars = true;

            match str_finisher {
                StringQuotation::Single => {
                    if self.peek() == Some('\'') {
                        return Kind::FStringMiddle;
                    }
                    if curr == '\'' {
                        if !self.peak_mode {
                            let last = self.tokenization_mode_stack.pop();
                            assert!(matches!(last, Some(TokenizationMode::Fstring(_))))
                        }
                        return Kind::FStringEnd;
                    }
                }
                StringQuotation::Double => {
                    if self.peek() == Some('"') {
                        return Kind::FStringMiddle;
                    }
                    if curr == '"' {
                        if !self.peak_mode {
                            let last = self.tokenization_mode_stack.pop();
                            assert!(matches!(last, Some(TokenizationMode::Fstring(_))))
                        }
                        return Kind::FStringEnd;
                    }
                }
                StringQuotation::TripleSingle => {
                    if self.peek() == Some('\'')
                        && self.peek() == self.double_peek()
                        && self.peek() == self.triple_peek()
                    {
                        return Kind::FStringMiddle;
                    }

                    if curr == '\''
                        && self.peek() == Some(curr)
                        && self.peek() == self.double_peek()
                    {
                        if !self.peak_mode {
                            let last = self.tokenization_mode_stack.pop();
                            assert!(matches!(last, Some(TokenizationMode::Fstring(_))))
                        }
                        self.double_next();
                        return Kind::FStringEnd;
                    }
                }
                StringQuotation::TripleDouble => {
                    if self.peek() == Some('\"')
                        && self.peek() == self.double_peek()
                        && self.peek() == self.triple_peek()
                    {
                        return Kind::FStringMiddle;
                    }
                    if curr == '"' && self.peek() == Some(curr) && self.peek() == self.double_peek()
                    {
                        if !self.peak_mode {
                            let last = self.tokenization_mode_stack.pop();
                            assert!(matches!(last, Some(TokenizationMode::Fstring(_))))
                        }
                        self.double_next();
                        return Kind::FStringEnd;
                    }
                }
            }
        }
    }

    fn next_kind(&mut self) -> Result<Kind, LexError> {
        if self.start_of_line && self.nesting == 0 {
            if let Some(indent_kind) = self.match_indentation()? {
                self.start_of_line = false;
                return Ok(indent_kind);
            }
        }

        if let Some(mode) = self.tokenization_mode_stack.last() {
            match mode {
                TokenizationMode::Fstring((fstring_nesting, fstrin_ending)) => {
                    return Ok(self.next_fstring_token(*fstrin_ending, *fstring_nesting))
                }
                TokenizationMode::FstringFormatSpecifier => {
                    let mut read_chars = 0;
                    while self.peek() != Some('}')
                        && self.peek() != Some('{')
                        && self.peek() != Some('\n')
                    {
                        self.next();
                        read_chars += 1;
                    }
                    if read_chars > 0 {
                        return Ok(Kind::FStringMiddle);
                    } else if !self.peak_mode {
                        self.tokenization_mode_stack.pop();
                    }
                }
                _ => (),
            }
        }

        while let Some(c) = self.next() {
            self.start_of_line = false; // WHY AGAIN?!

            match c {
                // Numbers
                '0'..='9' => return self.match_numeric_literal(),
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
                    _ => {
                        if let Some(TokenizationMode::PythonWithinFstring(i)) =
                            self.tokenization_mode_stack.last()
                        {
                            if self.nesting == *i && !self.peak_mode {
                                self.tokenization_mode_stack
                                    .push(TokenizationMode::FstringFormatSpecifier);
                            }
                        }
                        return Ok(Kind::Colon);
                    }
                },
                '!' => {
                    if let Some('=') = self.peek() {
                        self.next();
                        return Ok(Kind::NotEq);
                    }
                }
                // Delimiters
                '(' => {
                    return Ok(Kind::LeftParen);
                }
                ')' => {
                    return Ok(Kind::RightParen);
                }
                '[' => {
                    return Ok(Kind::LeftBrace);
                }
                ']' => {
                    return Ok(Kind::RightBrace);
                }
                '{' => {
                    return Ok(Kind::LeftBracket);
                }
                '}' => {
                    if self.peek() != Some('}') {
                        if let Some(mode) = self.tokenization_mode_stack.last() {
                            if matches!(mode, TokenizationMode::PythonWithinFstring(_)) {
                                if !self.peak_mode {
                                    self.tokenization_mode_stack.pop();
                                }
                                return Ok(Kind::RightBracket);
                            }
                        }
                    }
                    return Ok(Kind::RightBracket);
                }
                ',' => return Ok(Kind::Comma),
                '.' => {
                    if let Some('.') = self.peek() {
                        if let Some('.') = self.double_peek() {
                            self.double_next();
                            return Ok(Kind::Ellipsis);
                        }
                    }
                    return Ok(Kind::Dot);
                }
                ';' => return Ok(Kind::SemiColon),
                '=' => match self.peek() {
                    Some('=') => {
                        self.next();
                        return Ok(Kind::Eq);
                    }
                    _ => return Ok(Kind::Assign),
                },
                '#' => {
                    // consume until new line
                    while let Some(c) = self.peek() {
                        if c == '\n' || c == '\r' {
                            break;
                        }
                        self.next();
                    }
                    return Ok(Kind::Comment);
                }
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
                    self.current_line += 1;
                    self.start_of_line = true;
                    if self.nesting == 0 && (!self.non_logical_line_state) {
                        return Ok(Kind::NewLine);
                    } else {
                        return Ok(Kind::NL);
                    }
                }
                c if match_whitespace(c) => return Ok(Kind::WhiteSpace),
                _ => {}
            }
        }

        Ok(Kind::Eof)
    }

    fn match_id_keyword(&mut self, id_start: char) -> Result<Kind, LexError> {
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

    fn match_str(&mut self, id_start: char) -> Result<Option<Kind>, LexError> {
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
                        let fstring_start = self.f_string_quote_count(str_start);
                        if !self.peak_mode {
                            self.tokenization_mode_stack
                                .push(TokenizationMode::Fstring((
                                    self.nesting,
                                    StringQuotation::new(str_start, fstring_start),
                                )));
                        }
                        return Ok(Some(Kind::RawFStringStart));
                    }
                    _ => {}
                },
                Some(str_start @ '"') | Some(str_start @ '\'') => {
                    self.next();
                    self.skip_to_str_end(str_start)?;
                    return Ok(Some(Kind::StringLiteral));
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
                        let fstring_start = self.f_string_quote_count(str_start);
                        if !self.peak_mode {
                            self.tokenization_mode_stack
                                .push(TokenizationMode::Fstring((
                                    self.nesting,
                                    StringQuotation::new(str_start, fstring_start),
                                )));
                        }
                        return Ok(Some(Kind::RawFStringStart));
                    }
                    _ => {}
                },
                Some(str_start @ '"') | Some(str_start @ '\'') => {
                    self.next();
                    let fstring_start = self.f_string_quote_count(str_start);
                    if !self.peak_mode {
                        self.tokenization_mode_stack
                            .push(TokenizationMode::Fstring((
                                self.nesting,
                                StringQuotation::new(str_start, fstring_start),
                            )));
                    }
                    return Ok(Some(Kind::FStringStart));
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

    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        if let Some(c) = c {
            self.current += c.len_utf8() as u32;
        }
        c
    }

    fn double_next(&mut self) -> Option<char> {
        self.next();
        self.next()
    }

    fn peek(&self) -> Option<char> {
        self.source[self.current as usize..].chars().next()
    }

    fn double_peek(&self) -> Option<char> {
        self.source[self.current as usize..].chars().nth(1)
    }

    fn triple_peek(&self) -> Option<char> {
        self.source[self.current as usize..].chars().nth(2)
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

    fn skip_to_str_end(&mut self, str_start: char) -> Result<(), LexError> {
        // string start position is current position - 1 because we already consumed the
        // quote
        let _str_start_pos = self.current - 1;
        let mut string_terminated = false;
        let mut last_read_char = str_start;
        // Check if string starts with triple quotes
        // if string started with triple quotes, we need to read 3 characters at a time
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
                    break;
                }
                last_read_char = c;
            }
        }

        if !string_terminated {
            Err(LexError::StringNotTerminated)
        } else {
            Ok(())
        }
    }

    fn match_numeric_literal(&mut self) -> Result<Kind, LexError> {
        // first number is already consumed
        let _numeric_literal_start = self.current - 1;
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
                        _ => break,
                    }
                }
                return Ok(Kind::Binary);
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
                        _ => break,
                    }
                }
                return Ok(Kind::Octal);
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
                        _ => break,
                    }
                }
                return Ok(Kind::Hexadecimal);
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
                                    Some('0'..='9') => {
                                        self.next();
                                    }
                                    _ => break,
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
                            return Ok(Kind::ImaginaryExponentFloat);
                        }
                        return Ok(Kind::ExponentFloat);
                    }
                    if is_imaginary {
                        return Ok(Kind::ImaginaryPointFloat);
                    }
                    return Ok(Kind::PointFloat);
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
                        return Ok(Kind::ImaginaryExponentFloat);
                    }
                    return Ok(Kind::ExponentFloat);
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
            return Ok(Kind::ImaginaryInteger);
        }

        Ok(Kind::Integer)
    }

    fn match_indentation(&mut self) -> Result<Option<Kind>, LexError> {
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
        if self.nesting > 0 {
            // Don't add indent/dedent tokens when in nested context.
            // For example, in the following example
            // ```
            // a = (
            //   1
            // )
            // ```
            // the indentation of "1" is completely inconsequential. To be technically correct,
            // we'll return a whiteSpace token if any amount of whitespace was found.
            if spaces_count > 0 {
                return Ok(Some(Kind::WhiteSpace));
            } else {
                return Ok(None);
            }
        }
        if spaces_count == 0 {
            // When there are no spaces and only a new line
            // like the following
            // if True:
            //
            //   print("Hello")
            //
            // We should not consider that new line as dedent
            //
            // But also if this part is the last part of the file
            // we should not consider it as dedent
            // Thanks python
            if self.peek() == Some('\n') && self.double_peek().is_some() {
                return Ok(None);
            }
        }
        if let Some(top) = self.indent_stack.last() {
            match spaces_count.cmp(top) {
                Ordering::Less => {
                    // loop over indent stack from the top and check if this element matches the
                    // new indentation level if nothing matches then it is an error
                    // do not pop the element from the stack
                    let mut indentation_matches_outer_level = false;
                    for top in self.indent_stack.iter().rev() {
                        if top == &spaces_count {
                            indentation_matches_outer_level = true;
                            break;
                        }
                    }
                    if !indentation_matches_outer_level {
                        return Err(LexError::UnindentDoesNotMatchAnyOuterIndentationLevel);
                    }
                    Ok(Some(Kind::Dedent))
                }
                // Returning whitespace to ignore these spaces
                Ordering::Equal => Ok(Some(Kind::WhiteSpace)),
                Ordering::Greater => {
                    if !self.peak_mode {
                        self.indent_stack.push(spaces_count);
                    }
                    Ok(Some(Kind::Indent))
                }
            }
        } else {
            Ok(None)
        }
    }

    fn parse_token_value(&mut self, kind: Kind, start: u32) -> TokenValue {
        let kind_value = &self.source[start as usize..self.current as usize];
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
            | Kind::ImaginaryPointFloat => TokenValue::Number(kind_value.to_string()),
            Kind::Identifier => match kind_value {
                "type" => TokenValue::Type,
                "match" => TokenValue::Match,
                _ => TokenValue::Str(kind_value.to_string()),
            },
            Kind::StringLiteral
            | Kind::FStringStart
            | Kind::FStringMiddle
            | Kind::FStringEnd
            | Kind::RawBytes
            | Kind::RawFStringStart
            | Kind::Bytes
            | Kind::Unicode
            | Kind::Comment => TokenValue::Str(kind_value.to_string()),
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
                // TODO: This is not correct. But since we don't use the value inside parser it's
                // it's okay to do.
                // The reason for doing this is that we don't want to modify the indent_stack in
                // the peak mode which alters lexer state.
                if self.peak_mode {
                    return TokenValue::Indent(0);
                }
                while let Some(top) = self.indent_stack.last() {
                    match top.cmp(&spaces_count) {
                        Ordering::Greater => {
                            self.indent_stack.pop();
                            de_indents += 1;
                        }
                        Ordering::Equal => break,
                        // We only see a Kind::Dedent when the indentation level is less than the
                        // top of the stack. So this should never happen and if it happens it's a
                        // bug in code not an error for the user
                        Ordering::Less => {
                            unreachable!()
                        }
                    }
                }
                if de_indents != 1 {
                    // minus 1 because the dedent with actual Indent value is already added
                    // This is super hacky and I don't like it
                    self.next_token_is_dedent += de_indents - 1;
                }
                TokenValue::Indent(de_indents.into())
            }
            Kind::Indent => TokenValue::Indent(1),
            Kind::Error => TokenValue::Str(kind_value.to_string()),
            _ => TokenValue::None,
        }
    }

    fn f_string_quote_count(&mut self, str_start: char) -> u8 {
        let mut count = 1;
        if self.peek() == Some(str_start) && self.double_peek() == Some(str_start) {
            count = 3;
            self.double_next();
        }
        count
    }
}

fn match_whitespace(c: char) -> bool {
    c.is_whitespace() && c != '\n' && c != '\r'
}

#[cfg(test)]
mod tests {
    use std::fs;

    use insta::glob;

    use super::{Kind, Lexer};
    use crate::error::LexError;

    fn snapshot_test_lexer_and_errors(test_case: &str) {
        let mut lexer = Lexer::new(test_case);
        let mut tokens = vec![];
        let mut snapshot = String::from("");
        loop {
            let token = lexer.next_token();
            if token.kind == Kind::Eof {
                break;
            }
            snapshot += format!("{}\n", token).as_str();
            tokens.push(token);
        }
        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_path("../../test_data/output/");
        settings.set_description(test_case.to_string());
        settings.set_omit_expression(true);
        settings.bind(|| {
            insta::assert_snapshot!(snapshot);
        });
    }
    fn snapshot_test_lexer(snap_name: &str, inputs: &[&str]) -> Result<(), LexError> {
        for (i, test_input) in inputs.iter().enumerate() {
            let mut lexer = Lexer::new(test_input);
            let mut tokens = vec![];
            let mut snapshot = String::from("");
            loop {
                let token = lexer.next_token();
                if token.kind == Kind::Eof {
                    break;
                }
                snapshot += format!("{}\n", token).as_str();
                tokens.push(token);
            }
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_suffix(format!("{snap_name}-{i}"));
            settings.set_snapshot_path("../../test_data/output/");
            settings.set_description(test_input.to_string());
            settings.set_omit_expression(true);
            settings.bind(|| {
                insta::assert_snapshot!(snapshot);
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
                "\"hello\"  ",
                "\"world\"",
                "\"\"",
                "a = \"hello\"",
                "'hello'",
                "\"\"\"hello\"\"\"",
                "'''hello'''",
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

        snapshot_test_lexer(
            "newline-in-nested-structure",
            &["(a,

)"],
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
            pass\n",
                "if True:
    pass
else:
    pass",
                "if True:
    if True:
        pass
def",
                "def f(x):
    y = z

    print(y)
",
                "if a:

    f = c

    # Path: test_local.py
",
            ],
        )
        .unwrap();
    }

    #[test]
    fn test_fstring() {
        // F-strings
        snapshot_test_lexer(
            "f-string-literals",
            &[
                "f\"hello\"",
                "f'hello_{var}'",
                "f\"world\"",
                "f\"\"",
                "a = f\"hello\"",
                "f\"\"\"hello\"\"\"",
                "f'''hello'''",
                "f\"{{hey}}\"",
                "f\"oh_{{hey}}\"",
                "f'a' 'c'",
                "f'hello_{f'''{a}'''}'",
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
    }

    #[test]
    fn test_ellipsis() {
        snapshot_test_lexer(
            "ellipsis",
            &[
                "...",
                "def a():
    ...",
            ],
        )
        .unwrap();
    }

    #[test]
    #[should_panic]
    fn test_unterminated_string_double_quotes() {
        snapshot_test_lexer(
            "unterminated-string",
            &["\"hello", "'hello", "'''hello''", "'''hello'"],
        )
        .unwrap();
    }

    #[test]
    fn test_complete() {
        glob!("../../test_data", "inputs/*.py", |path| {
            let test_case = fs::read_to_string(path).unwrap();
            println!("testing {:?}", path);
            snapshot_test_lexer_and_errors(&test_case);
        })
    }

    #[test]
    fn test_peek_not_changing() {
        glob!("../../test_data", "inputs/*.py", |path| {
            let test_case = fs::read_to_string(path).unwrap();
            let mut lexer = Lexer::new(&test_case);
            loop {
                let peeked_first = lexer.peek_token();
                let peeked_second = lexer.peek_token();
                if peeked_first != peeked_second {
                    assert_eq!(
                        peeked_first, peeked_second,
                        "peek was not consistent, file {:?}",
                        path
                    );
                }
                let next = lexer.next_token();
                if next.kind == Kind::Eof {
                    break;
                }
            }
        })
    }
}
