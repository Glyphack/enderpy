use crate::lexer::Lexer;
use crate::token::{Kind, Token, TokenValue};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)] // #[serde(tag = "type")]
pub struct Node {
    /// Start offset in source
    pub start: usize,

    /// End offset in source
    pub end: usize,
}

impl Node {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

// The following structs are used to represent the AST
// https://docs.python.org/3/library/ast.html#abstract-grammar
#[derive(Debug)]
pub struct Module {
    pub node: Node,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    AssignStatement(Assign),
    ExpressionStatement(Expression),
}

#[derive(Debug)]
pub struct Assign {
    pub node: Node,
    pub targets: Vec<Expression>,
    pub value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Constant(Box<Constant>),
    List(Box<List>),
    Tuple(Box<Tuple>),
    Name(Box<Name>),
}

// https://docs.python.org/3/reference/expressions.html#atom-identifiers
#[derive(Debug)]
pub struct Name {
    pub node: Node,
    pub id: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constant {
    pub node: Node,
    pub value: ConstantValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantValue {
    None,
    Bool(bool),
    Str(String),
    Bytes(Vec<u8>),
    Tuple(Vec<Constant>),
    // Numbers are string because we don't care about the value rn.
    Int(String),
    Float(String),
    Complex { real: String, imaginary: String },
}

#[derive(Debug)]
pub struct List {
    pub node: Node,
    pub elements: Vec<Expression>,
}

#[derive(Debug)]
pub struct Tuple {
    pub node: Node,
    pub elements: Vec<Expression>,
}

#[derive(Debug)]
pub struct Parser {
    source: String,
    lexer: Lexer,
    cur_token: Token,
    prev_token_end: usize,
}

impl Parser {
    pub fn new(source: String) -> Self {
        let mut lexer = Lexer::new(&source);
        let cur_token = lexer.next_token();
        let prev_token_end = 0;

        Self {
            source,
            lexer,
            cur_token,
            prev_token_end,
        }
    }

    pub fn parse(&mut self) -> Module {
        let node = self.start_node();
        let mut body = vec![];
        while self.cur_kind() != Kind::Eof {
            let stmt = self.parse_statement();
            body.push(stmt);
        }

        Module {
            node: self.finish_node(node),
            body,
        }
    }

    fn start_node(&self) -> Node {
        let token = self.cur_token();
        Node::new(token.start, 0)
    }

    fn finish_node(&self, node: Node) -> Node {
        Node::new(node.start, self.prev_token_end)
    }
    fn cur_token(&self) -> &Token {
        &self.cur_token
    }

    fn cur_kind(&self) -> Kind {
        self.cur_token.kind
    }

    /// Checks if the current index has token `Kind`
    fn at(&self, kind: Kind) -> bool {
        self.cur_kind() == kind
    }

    /// Advance if we are at `Kind`
    fn bump(&mut self, kind: Kind) {
        if self.at(kind) {
            self.advance();
        }
    }

    /// Advance any token
    fn bump_any(&mut self) {
        self.advance();
    }

    /// Advance and return true if we are at `Kind`, return false otherwise
    fn eat(&mut self, kind: Kind) -> bool {
        if self.at(kind) {
            self.advance();
            return true;
        }
        false
    }

    /// Move to the next token
    fn advance(&mut self) {
        let token = self.lexer.next_token();
        self.prev_token_end = self.cur_token.end;
        self.cur_token = token;
    }

    fn parse_statement(&mut self) -> Statement {
        match self.cur_kind() {
            Kind::Identifier => self.parse_identifier_statement(),
            _ => panic!("Not implemented {:?}", self.cur_token),
        }
    }

    // Parses an statement which starts with an identifier
    fn parse_identifier_statement(&mut self) -> Statement {
        let start = self.start_node();
        let lhs = self.parse_expression();
        if self.eat(Kind::Assign) {
            let rhs = self.parse_expression();
            return Statement::AssignStatement(Assign {
                node: self.finish_node(start),
                targets: vec![lhs],
                value: rhs,
            });
        } else {
            panic!("Not implemented {:?}", self.cur_token);
        }
    }

    fn parse_expression(&mut self) -> Expression {
        let start = self.start_node();
        let consumed_token = self.cur_token().clone();
        self.bump_any();
        let mut expr = match consumed_token.kind {
            Kind::Identifier => Expression::Name(Box::new(Name {
                node: self.finish_node(start),
                id: match consumed_token.value.clone() {
                    TokenValue::Str(val) => val,
                    _ => panic!("Identifier must be string {:?}", self.cur_token()),
                },
            })),
            Kind::Integer => match consumed_token.value.clone() {
                TokenValue::Number(val) => Expression::Constant(Box::new(Constant {
                    node: self.finish_node(start),
                    value: ConstantValue::Int(val),
                })),
                _ => panic!("Integer number value must be a number {:?}", consumed_token),
            },
            Kind::None => Expression::Constant(Box::new(Constant {
                node: self.finish_node(start),
                value: ConstantValue::None,
            })),
            Kind::True => Expression::Constant(Box::new(Constant {
                node: self.finish_node(start),
                value: ConstantValue::Bool(true),
            })),
            Kind::False => Expression::Constant(Box::new(Constant {
                node: self.finish_node(start),
                value: ConstantValue::Bool(false),
            })),
            Kind::ImaginaryInteger => match consumed_token.value.clone() {
                TokenValue::Number(val) => Expression::Constant(Box::new(Constant {
                    node: self.finish_node(start),
                    value: ConstantValue::Complex {
                        real: "0".to_string(),
                        imaginary: val,
                    },
                })),
                _ => panic!(
                    "Imaginary integer number value must be a number {:?}",
                    consumed_token
                ),
            },
            Kind::Bytes => match consumed_token.value.clone() {
                TokenValue::Str(val) => {
                    let bytes_val = self
                        .extract_string_inside(
                            val.strip_prefix("b")
                                .expect("bytes literal must start with b")
                                .to_string(),
                        )
                        .into_bytes();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Bytes(bytes_val),
                    }))
                }
                _ => panic!("Bytes value must be a bytes {:?}", self.cur_token()),
            },
            Kind::StringLiteral => match consumed_token.value.clone() {
                TokenValue::Str(val) => {
                    let string_val = self.extract_string_inside(val);
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Str(string_val),
                    }))
                }
                _ => panic!("String value must be a string {:?}", self.cur_token()),
            },
            Kind::RawString => match consumed_token.value.clone() {
                TokenValue::Str(val) => {
                    let string_val =
                        self.extract_string_inside(val.chars().skip(1).collect::<String>());
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Str(string_val),
                    }))
                }
                _ => panic!("String value must be a string {:?}", self.cur_token()),
            },
            Kind::RawBytes => match consumed_token.value.clone() {
                TokenValue::Str(val) => {
                    // rb or br appear in the beginning of raw bytes
                    let bytes_val = self
                        .extract_string_inside(val.chars().skip(2).collect::<String>())
                        .into_bytes();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Bytes(bytes_val),
                    }))
                }
                _ => panic!("Bytes value must be a bytes {:?}", self.cur_token()),
            },
            _ => panic!("Not implemented {:?}", self.cur_token()),
        };
        while self.eat(Kind::Comma) {
            let next_elm = self.parse_expression();
            let node = self.start_node();
            expr = Expression::Tuple(Box::new(Tuple {
                node: self.finish_node(node),
                elements: vec![expr, next_elm],
            }));
        }
        expr
    }

    fn extract_string_inside(&self, val: String) -> String {
        if let Some(val) = val.strip_prefix("\"\"\"") {
            val.strip_suffix("\"\"\"")
                .expect("String must be enclosed with \"\"\"")
                .to_string()
        } else if let Some(val) = val.strip_prefix("\"") {
            val.strip_suffix("\"")
                .expect("String must be enclosed with \"")
                .to_string()
        } else if let Some(val) = val.strip_prefix("'''") {
            val.strip_suffix("'''")
                .expect("String must be enclosed with '''")
                .to_string()
        } else if let Some(val) = val.strip_prefix("'") {
            val.strip_suffix("'")
                .expect("String must be enclosed with '")
                .to_string()
        } else {
            panic!("String must be enclosed in \"\"\", \"', ''' or '");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn test_parse_name() {
        for test_case in &[
            "a = 1",
            "a = None",
            "a = True",
            "a = False",
            "a = 1j",
            "a = b'1'",
            "a = rb'1'",
            "a = br'1'",
            "a = \"a\"",
            "a = '''a'''",
            "a = \"\"\"a\"\"\"",
            "a = 'a'",
        ] {
            let mut parser = Parser::new(test_case.to_string());
            let program = parser.parse();

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }
}
