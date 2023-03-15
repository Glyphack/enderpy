use crate::lexer::Lexer;
use crate::token::{Kind, Token, TokenValue};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
// #[serde(tag = "type")]
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

#[derive(Debug)]
pub struct Constant {
    pub node: Node,
    pub value: String,
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

    fn parse(&mut self) -> Module {
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
        let start = self.start_node();
        let left = self.parse_expression();
        if self.eat(Kind::Assign) {
            let right = self.parse_expression();
            return Statement::AssignStatement(Assign {
                node: self.finish_node(start),
                targets: vec![left],
                value: right,
            });
        } else {
            panic!("Not implemented {:?}", self.cur_token);
        }
    }

    fn parse_expression(&mut self) -> Expression {
        let start = self.start_node();
        let consumed_token = self.cur_token().clone();
        self.bump_any();
        let expr = match consumed_token.kind {
            Kind::Identifier => Expression::Name(Box::new(Name {
                node: self.finish_node(start),
                id: match consumed_token.value.clone() {
                    TokenValue::Str(val) => val,
                    _ => panic!("Identifier not a string but {:?}", self.cur_token()),
                },
            })),
            Kind::Integer => Expression::Constant(Box::new(Constant {
                node: self.finish_node(start),
                value: match consumed_token.value.clone() {
                    TokenValue::Number(val) => val,
                    _ => panic!("Integer not a number but {:?}", self.cur_token()),
                },
            })),
            _ => panic!("Not implemented"),
        };
        expr
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn test_parse_name() {
        let source = "a = 1".to_string();
        let mut parser = Parser::new(source);
        let program = parser.parse();

        insta::with_settings!({
                description => "a = 1", // the template source code
                omit_expression => true // do not include the default expression
            }, {
                assert_debug_snapshot!(program);
        });
    }
}
