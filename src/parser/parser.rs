use std::panic;

use crate::lexer::lexer::Lexer;
use crate::parser::ast::*;
use crate::parser::operator::{is_bool_op, is_unary_op, map_boolean_operator};
use crate::parser::string::extract_string_inside;
use crate::token::{Kind, Token, TokenValue};
use miette::Result;

use super::diagnostics;
use super::expression::is_atom;
use super::operator::{is_bin_op, map_binary_operator, map_unary_operator};

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
            if stmt.is_ok() {
                body.push(stmt.unwrap());
            } else {
                println!("Error: {:?}", stmt.err());
                self.bump_any();
            }
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

    /// Expect a `Kind` or return error
    pub fn expect(&mut self, kind: Kind) -> Result<()> {
        if !self.at(kind) {
            let range = self.start_node();
            return Err(
                diagnostics::ExpectToken(kind.to_str(), self.cur_kind().to_str(), range).into(),
            );
        }
        self.advance();
        Ok(())
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let stmt = match self.cur_kind() {
            Kind::Identifier => self.parse_identifier_statement(),
            Kind::LeftParen => self.parse_paren_statement(),
            _ => Ok(Statement::ExpressionStatement(
                self.parse_expression().unwrap(),
            )),
        };

        stmt
    }

    // Parses an statement which starts with an identifier
    fn parse_identifier_statement(&mut self) -> Result<Statement> {
        let start = self.start_node();
        let lhs = self.parse_expression().unwrap();
        if self.eat(Kind::Assign) {
            let rhs = self.parse_expression()?;
            return Ok(Statement::AssignStatement(Assign {
                node: self.finish_node(start),
                targets: vec![lhs],
                value: rhs,
            }));
        }
        return Ok(Statement::ExpressionStatement(lhs));
    }

    // Parses an statement which starts with a left parenthesis
    fn parse_paren_statement(&mut self) -> Result<Statement> {
        let start = self.start_node();
        self.bump(Kind::LeftParen);
        let expr = self.parse_expression()?;
        match expr {
            Expression::NamedExpr(_) => {
                self.bump(Kind::RightParen);
                Ok(Statement::ExpressionStatement(expr))
            }
            _ => Err(diagnostics::InvalidSyntax("", start).into()),
        }
    }

    // https://docs.python.org/3/library/ast.html#expressions
    fn parse_expression(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let unary_expr = if is_unary_op(&self.cur_kind()) {
            let unary_node = self.start_node();
            let op = map_unary_operator(&self.cur_kind());
            self.bump_any();
            let operand = self.parse_expression()?;
            Some(Expression::UnaryOp(Box::new(UnaryOperation {
                node: self.finish_node(unary_node),
                op,
                operand: Box::new(operand),
            })))
        } else {
            None
        };

        if self.at(Kind::Identifier) && self.peek_kind() == Kind::Walrus {
            let mut identifier_node = self.start_node();
            let identifier = self.cur_token().value.to_string();
            self.bump(Kind::Identifier);
            identifier_node = self.finish_node(identifier_node);
            self.bump(Kind::Walrus);
            let value = self.parse_expression()?;
            return Ok(Expression::NamedExpr(Box::new(NamedExpression {
                node: self.finish_node(node),
                target: Box::new(Expression::Name(Box::new(Name {
                    node: identifier_node,
                    id: identifier,
                }))),
                value: Box::new(value),
            })));
        }

        let atom = if is_atom(&self.cur_kind()) {
            // value must be cloned to be assigned to the node
            let token_value = self.cur_token().value.clone();
            let token_kind = self.cur_kind();
            // bump needs to happen before creating the atom node
            // otherwise the end would be the same as the start
            self.bump_any();
            let expr = Some(self.map_to_atom(node, &token_kind, token_value));
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
            return Err(
                diagnostics::UnexpectedToken(self.cur_kind().to_str(), self.start_node()).into(),
            );
        };

        let current_kind = self.cur_kind();
        if is_bool_op(&current_kind) {
            let op = map_boolean_operator(&current_kind);
            self.bump_any();
            let rhs = self.parse_expression()?;
            // TODO: Check if rhs is BoolOp then we need to flatten it
            // e.g. a or b or c can be BoolOp(or, [a, b, c])
            expr = Expression::BoolOp(Box::new(BoolOperation {
                node: self.finish_node(node),
                op,
                values: vec![expr, rhs],
            }))
        }

        if is_bin_op(&current_kind) {
            let op = map_binary_operator(&current_kind);
            self.bump_any();
            let rhs = self.parse_expression()?;
            expr = Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op,
                left: Box::new(expr),
                right: Box::new(rhs),
            }))
        }

        while self.eat(Kind::Comma) {
            let node = self.start_node();
            let next_elm = self.parse_expression();
            match next_elm {
                Ok(next_elm) => {
                    self.bump_any();
                    expr = Expression::Tuple(Box::new(Tuple {
                        node: self.finish_node(node),
                        elements: vec![expr, next_elm],
                    }));
                }
                Err(_) => {
                    expr = Expression::Tuple(Box::new(Tuple {
                        node: self.finish_node(node),
                        elements: vec![expr],
                    }))
                }
            }
        }
        Ok(expr)
    }

    fn map_to_atom(&self, start: Node, kind: &Kind, value: TokenValue) -> Expression {
        let atom = match kind {
            Kind::Identifier => Expression::Name(Box::new(Name {
                node: self.finish_node(start),
                id: value.to_string(),
            })),
            Kind::Integer => Expression::Constant(Box::new(Constant {
                node: self.finish_node(start),
                value: ConstantValue::Int(value.to_string()),
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
                    imaginary: value.to_string(),
                },
            })),
            Kind::Bytes => {
                let bytes_val = extract_string_inside(
                    value
                        .to_string()
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
                let string_val = extract_string_inside(value.to_string());
                Expression::Constant(Box::new(Constant {
                    node: self.finish_node(start),
                    value: ConstantValue::Str(string_val),
                }))
            }
            Kind::RawString => {
                let string_val =
                    extract_string_inside(value.to_string().chars().skip(1).collect::<String>());
                Expression::Constant(Box::new(Constant {
                    node: self.finish_node(start),
                    value: ConstantValue::Str(string_val),
                }))
            }
            Kind::RawBytes => {
                // rb or br appear in the beginning of raw bytes
                let bytes_val =
                    extract_string_inside(value.to_string().chars().skip(2).collect::<String>())
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

    #[test]
    fn test_binary_op() {
        for test_case in &[
            "a + b", "a - b", "a * b", "a / b", "a // b", "a % b", "a ** b", "a << b", "a >> b",
            "a & b", "a ^ b", "a | b", "a @ b",
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
    fn test_named_expression() {
        for test_case in &["(a := b)"] {
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
