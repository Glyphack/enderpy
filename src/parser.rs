use crate::lexer::{Kind, Lexer, Token, TokenValue};

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
pub struct Program {
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
    pub targets: Vec<Name>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Name {
    pub node: Node,
    pub id: BindingIdentifier,
}

#[derive(Debug)]
pub struct BindingIdentifier {
    pub node: Node,
    pub name: String,
}

#[derive(Debug)]
pub enum Expression {
    BooleanOperation(Box<BooleanOperation>),
}

#[derive(Debug)]
pub struct BooleanOperation {
    pub node: Node,
    pub op: BooleanOperator,
    pub values: Vec<Expression>,
}

#[derive(Debug)]
pub enum BooleanOperator {
    And,
    Or,
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

    fn parse(&mut self) -> Program {
        let stmt = self.parse_assign_statement();
        let body = vec![stmt];
        return Program {
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

    fn parse_assign_statement(&mut self) -> Statement {
        let start = self.start_node();
        self.bump(Kind::Identifier);
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

    fn parse_binding_identifier_list(&mut self) -> Vec<Name> {
        let mut list = vec![];
        loop {
            let start = self.start_node();
            self.bump(Kind::Identifier);
            let end = self.finish_node(start);

            match self.cur_token().value {
                TokenValue::Str(ref s) => {
                    list.push(Name {
                        node: end,
                        id: BindingIdentifier {
                            node: end,
                            name: s.clone(),
                        },
                    });
                }
                _ => {
                    panic!("Unexpected token value: {:?}", self.cur_token().value)
                }
            }

            if self.eat(Kind::Comma) {
                continue;
            }
            break;
        }
        list
    }

    fn parse_expression(&mut self) -> Expression {
        let start = self.start_node();
        let op = self.parse_boolean_operator();
        let values = self.parse_boolean_values();
        let end = self.finish_node(start);
        Expression::BooleanOperation(Box::new(BooleanOperation {
            node: end,
            op,
            values,
        }))
    }

    fn parse_boolean_operator(&mut self) -> BooleanOperator {
        if self.eat(Kind::And) {
            BooleanOperator::And
        } else if self.eat(Kind::Or) {
            BooleanOperator::Or
        } else {
            panic!("Unexpected token: {:?}", self.cur_token())
        }
    }

    fn parse_boolean_values(&mut self) -> Vec<Expression> {
        let mut values = vec![];
        loop {
            let value = self.parse_boolean_value();
            values.push(value);
            if self.eat(Kind::Comma) {
                continue;
            }
            break;
        }
        values
    }

    fn parse_boolean_value(&mut self) -> Expression {
        if self.eat(Kind::True) {
            Expression::BooleanOperation(Box::new(BooleanOperation {
                node: Node::new(0, 0),
                op: BooleanOperator::And,
                values: vec![],
            }))
        } else if self.eat(Kind::False) {
            Expression::BooleanOperation(Box::new(BooleanOperation {
                node: Node::new(0, 0),
                op: BooleanOperator::Or,
                values: vec![],
            }))
        } else {
            panic!("Unexpected token: {:?}", self.cur_token())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn test_parse() {
        let source = "a = true".to_string();
        let mut parser = Parser::new(source);
        let program = parser.parse();
        assert_debug_snapshot!(program);
    }
}
