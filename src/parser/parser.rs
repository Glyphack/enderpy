use std::panic;

use crate::lexer::lexer::Lexer;
use crate::parser::ast::*;
use crate::parser::operator::{is_bool_op, is_unary_op, map_binary_operator};
use crate::parser::string::extract_string_inside;
use crate::token::{Kind, Token, TokenValue};

use super::expression::is_atom;
use super::operator::map_unary_operator;

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

    fn peek_token(&mut self) -> Token {
        self.lexer.peek_token()
    }

    fn peek_kind(&mut self) -> Kind {
        self.lexer.peek_token().kind
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
            _ => Statement::ExpressionStatement(self.parse_expression()),
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
        }
        return Statement::ExpressionStatement(lhs);
    }

    // https://docs.python.org/3/library/ast.html#expressions
    fn parse_expression(&mut self) -> Expression {
        let unary_expr = if is_unary_op(&self.cur_kind()) {
            let unary_node = self.start_node();
            let op = map_unary_operator(&self.cur_kind());
            self.bump_any();
            let operand = self.parse_expression();
            Some(Expression::UnaryOp(Box::new(UnaryOperation {
                node: self.finish_node(unary_node),
                op,
                operand: Box::new(operand),
            })))
        } else {
            None
        };

        let atom = if is_atom(&self.cur_kind()) {
            let expr = Some(self.parse_atom_expression());
            expr
        } else {
            None
        };

        // refactor to if let and smaller function
        let mut expr = if let Some(atom) = atom {
            atom
        } else if let Some(unary_expr) = unary_expr {
            unary_expr
        } else {
            panic!("Expected expression {:?}", self.cur_token)
        };

        let current_kind = self.cur_kind();
        if is_bool_op(&current_kind) {
            let op = map_binary_operator(&current_kind);
            self.bump_any();
            let rhs = self.parse_expression();
            // TODO: Check if rhs is BoolOp then we need to flatten it
            // e.g. a or b or c can be BoolOp(or, [a, b, c])
            let node = self.start_node();
            expr = Expression::BoolOp(Box::new(BoolOperation {
                node: self.finish_node(node),
                op,
                values: vec![expr, rhs],
            }))
        }

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

    fn parse_atom_expression(&mut self) -> Expression {
        let start = self.start_node();
        let prev_token = self.cur_token().clone();
        self.bump_any();
        let atom = match prev_token.kind {
            Kind::Identifier => Expression::Name(Box::new(Name {
                node: self.finish_node(start),
                id: self.unwrap_token_value_str(&prev_token.value),
            })),
            Kind::Integer => Expression::Constant(Box::new(Constant {
                node: self.finish_node(start),
                value: ConstantValue::Int(self.unwrap_token_value_number(&prev_token.value)),
            })),
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
            Kind::ImaginaryInteger => Expression::Constant(Box::new(Constant {
                node: self.finish_node(start),
                value: ConstantValue::Complex {
                    real: "0".to_string(),
                    imaginary: self.unwrap_token_value_number(&prev_token.value),
                },
            })),
            Kind::Bytes => {
                let bytes_val = extract_string_inside(
                    self.unwrap_token_value_str(&prev_token.value)
                        .strip_prefix("b")
                        .expect("bytes literal must start with b")
                        .to_string(),
                )
                .into_bytes();
                Expression::Constant(Box::new(Constant {
                    node: self.finish_node(start),
                    value: ConstantValue::Bytes(bytes_val),
                }))
            }
            Kind::StringLiteral => {
                let string_val =
                    extract_string_inside(self.unwrap_token_value_str(&prev_token.value));
                Expression::Constant(Box::new(Constant {
                    node: self.finish_node(start),
                    value: ConstantValue::Str(string_val),
                }))
            }
            Kind::RawString => {
                let string_val = extract_string_inside(
                    self.unwrap_token_value_str(&prev_token.value)
                        .chars()
                        .skip(1)
                        .collect::<String>(),
                );
                Expression::Constant(Box::new(Constant {
                    node: self.finish_node(start),
                    value: ConstantValue::Str(string_val),
                }))
            }
            Kind::RawBytes => {
                // rb or br appear in the beginning of raw bytes
                let bytes_val = extract_string_inside(
                    self.unwrap_token_value_str(&prev_token.value)
                        .chars()
                        .skip(2)
                        .collect::<String>(),
                )
                .into_bytes();
                Expression::Constant(Box::new(Constant {
                    node: self.finish_node(start),
                    value: ConstantValue::Bytes(bytes_val),
                }))
            }
            _ => panic!("Expected atom expression"),
        };
        atom
    }

    pub fn unwrap_token_value_str(&self, value: &TokenValue) -> String {
        match value {
            TokenValue::Str(ref val) => val.to_string(),
            _ => panic!("Expected value of token {:?} to be a string", value),
        }
    }

    fn unwrap_token_value_number(&self, value: &TokenValue) -> String {
        match value {
            TokenValue::Number(ref val) => val.to_string(),
            _ => panic!("Expected value of token {:?} to be a number", value),
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
    #[test]
    fn test_parse_bool_op() {
        for test_case in &["a or b", "a and b", "a or b or c", "a and b or c"] {
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

    #[test]
    fn test_parse_unary_op() {
        for test_case in &["not a", "+ a", "~ a", "-a"] {
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
