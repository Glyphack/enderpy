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

#[derive(Debug)]
pub struct Module {
    pub node: Node,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    AssignStatement(Assign),
}

#[derive(Debug)]
pub struct Assign {
    pub node: Node,
    pub targets: Vec<BindingIdentifier>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct BindingIdentifier {
    pub node: Node,
    pub name: String,
}

#[derive(Debug)]
pub enum Expression {
    Constant(Box<Constant>),
}

#[derive(Debug)]
pub struct Constant {
    pub node: Node,
    pub value: String,
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
        let cur_token = lexer.read_next_token();
        let prev_token_end = 0;

        Self {
            source,
            lexer,
            cur_token,
            prev_token_end,
        }
    }

    fn parse(&mut self) -> Module {
        let stmt = self.parse_assign_statement();
        let body = vec![stmt];
        return Module {
            node: Node::new(0, self.source.len()),
            body,
        };
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
        let token = self.lexer.read_next_token();
        self.prev_token_end = self.cur_token.end;
        self.cur_token = token;
    }

    // TODO: This is a dummy function to parse an assignment statement
    fn parse_assign_statement(&mut self) -> Statement {
        let start = self.start_node();
        let targets = self.parse_binding_identifier_list();
        self.bump(Kind::Assign);
        let value = self.parse_expression();
        let end = self.finish_node(start);
        Statement::AssignStatement(Assign {
            node: end,
            targets,
            value,
        })
    }

    fn parse_binding_identifier_list(&mut self) -> Vec<BindingIdentifier> {
        let mut list = vec![];
        loop {
            let start = self.start_node();
            let val = self.cur_token().value.clone();
            self.bump(Kind::Identifier);
            list.push(BindingIdentifier {
                node: self.finish_node(start),
                name: match val {
                    TokenValue::Str(val) => val,
                    _ => "".to_string(),
                },
            });

            if self.eat(Kind::Comma) {
                continue;
            }
            break;
        }
        list
    }

    // TODO: This is a dummy function to parse an expression
    fn parse_expression(&mut self) -> Expression {
        let start = self.start_node();
        let val = self.cur_token().value.clone();
        self.bump(Kind::Integer);
        Expression::Constant(Box::new(Constant {
            node: self.finish_node(start),
            value: match val {
                TokenValue::Number(val) => val.to_string(),
                _ => "".to_string(),
            },
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn test_parse() {
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
