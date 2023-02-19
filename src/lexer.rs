#[derive(Debug)]
pub struct Token {
    pub kind: Kind,
    pub value: TokenValue,
}

#[derive(Debug, PartialEq)]
pub enum Kind {
    Plus,
    Eq,
    AddAssign,
    Integer,

    If,
    Else,
    Elif,

    Identifier,

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
                '=' => return Kind::Eq,
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
}
