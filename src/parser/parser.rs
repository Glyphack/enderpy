use std::panic;

use crate::lexer::lexer::Lexer;
use crate::parser::ast::*;
use crate::parser::operator::{is_bool_op, is_unary_op, map_boolean_operator};
use crate::parser::string::extract_string_inside;
use crate::token::{Kind, Token, TokenValue};
use miette::Result;

use super::diagnostics;
use super::expression::{is_atom, is_iterable, is_primary};
use super::operator::{is_bin_op, map_binary_operator, map_unary_operator};

#[derive(Debug)]
pub struct Parser {
    source: String,
    lexer: Lexer,
    cur_token: Token,
    prev_token_end: usize,
    // This var keeps track of how many levels deep we are in a list, tuple or set
    // expression. This is used to determine if we should parse comma separated
    // expressions as tuple or not.
    // This is incremented when we see an opening bracket and decremented when we
    // see a closing bracket.
    nested_expression_list: usize,
}

impl Parser {
    pub fn new(source: String) -> Self {
        let mut lexer = Lexer::new(&source);
        let cur_token = lexer.next_token().unwrap();
        let prev_token_end = 0;

        Self {
            source,
            lexer,
            cur_token,
            prev_token_end,
            nested_expression_list: 0,
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

    fn peek_token(&mut self) -> Result<Token> {
        self.lexer.peek_token()
    }

    fn peek_kind(&mut self) -> Result<Kind> {
        let token = self.lexer.peek_token()?;
        Ok(token.kind)
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
        match token {
            Err(err) => {
                println!("Error: {:?}", err);
                self.bump_any();
            }
            Ok(token) => {
                self.prev_token_end = self.cur_token.end;
                self.cur_token = token;
            }
        }
    }

    /// Expect a `Kind` or return error
    pub fn expect(&mut self, kind: Kind) -> Result<()> {
        if !self.at(kind) {
            let range = self.finish_node(self.start_node());
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
            _ => Ok(Statement::ExpressionStatement(self.parse_expression()?)),
        };

        stmt
    }

    // Parses an statement which starts with an identifier
    fn parse_identifier_statement(&mut self) -> Result<Statement> {
        let start = self.start_node();
        let lhs = self.parse_expression()?;
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

    // https://docs.python.org/3/library/ast.html#expressions
    fn parse_expression(&mut self) -> Result<Expression> {
        if self.at(Kind::LeftBrace) {
            self.nested_expression_list += 1;
            let list_expr = self.parse_list();
            self.nested_expression_list -= 1;
            return list_expr;
        }
        if self.at(Kind::LeftBracket) {
            self.nested_expression_list += 1;
            let dict_or_set_expr = self.parse_dict_or_set();
            self.nested_expression_list -= 1;
            return dict_or_set_expr;
        }
        if self.at(Kind::LeftParen) {
            self.nested_expression_list += 1;
            let tuple_or_named_expr = self.parse_tuple_or_named_expr();
            self.nested_expression_list -= 1;
            return tuple_or_named_expr;
        }
        if self.at(Kind::Mul) {
            return self.parse_starred();
        }
        if self.at(Kind::Await) {
            return self.parse_await();
        }
        let node = self.start_node();

        if self.at(Kind::Yield) {
            let yield_node = self.start_node();
            self.bump(Kind::Yield);
            if self.at(Kind::From) {
                self.bump(Kind::From);
                let value = self.parse_expression()?;
                return Ok(Expression::YieldFrom(Box::new(YieldFrom {
                    node: self.finish_node(yield_node),
                    value: Box::new(value),
                })));
            }
            let value = match self.parse_expression() {
                Ok(expr) => Some(Box::new(expr)),
                _ => None,
            };
            return Ok(Expression::Yield(Box::new(Yield {
                node: self.finish_node(yield_node),
                value,
            })));
        }

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

        if self.at(Kind::Identifier) && matches!(self.peek_kind(), Ok(Kind::Walrus)) {
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
            return Err(diagnostics::UnexpectedToken(
                self.cur_kind().to_str(),
                self.finish_node(self.start_node()),
            )
            .into());
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

        if self.at(Kind::Comma) && self.nested_expression_list == 0 {
            self.nested_expression_list += 1;
            let mut elements = vec![expr];
            while self.eat(Kind::Comma) && !self.at(Kind::NewLine) {
                // not all expressions are allowed in a tuple
                // TODO: check if the expression is allowed
                elements.push(self.parse_expression()?);
            }
            self.nested_expression_list -= 1;
            expr = Expression::Tuple(Box::new(Tuple {
                node: self.finish_node(node),
                elements,
            }))
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

    fn parse_list(&mut self) -> Result<Expression> {
        let node = self.start_node();
        self.bump(Kind::LeftBrace);
        let mut elements = vec![];
        while !self.eat(Kind::RightBrace) {
            let expr = self.parse_expression()?;
            elements.push(expr);
            self.eat(Kind::Comma);
        }
        Ok(Expression::List(Box::new(List {
            node: self.finish_node(node),
            elements,
        })))
    }

    fn parse_tuple_or_named_expr(&mut self) -> Result<Expression> {
        let node = self.start_node();
        self.bump(Kind::LeftParen);
        let mut elements = vec![];
        while !self.eat(Kind::RightParen) {
            let expr = self.parse_expression()?;
            if let Expression::NamedExpr(_) = expr {
                self.bump(Kind::RightParen);
                return Ok(expr);
            }

            elements.push(expr);
            if !self.eat(Kind::Comma) && !self.at(Kind::RightParen) {
                return Err(diagnostics::ExpectToken(
                    Kind::Comma.to_str(),
                    self.cur_kind().to_str(),
                    self.finish_node(node),
                )
                .into());
            }
        }
        Ok(Expression::Tuple(Box::new(Tuple {
            node: self.finish_node(node),
            elements,
        })))
    }

    fn parse_dict_or_set(&mut self) -> Result<Expression> {
        let node = self.start_node();
        self.bump(Kind::LeftBracket);
        if matches!(self.peek_kind(), Ok(Kind::Comma)) {
            self.parse_set(node)
        } else {
            self.parse_dict(node)
        }
    }

    fn parse_set(&mut self, node: Node) -> Result<Expression> {
        let mut elements = vec![];
        while !self.eat(Kind::RightBracket) {
            let expr = self.parse_expression()?;
            elements.push(expr);
            if !self.eat(Kind::Comma) && !self.at(Kind::RightBracket) {
                return Err(diagnostics::ExpectToken(
                    Kind::Comma.to_str(),
                    self.cur_kind().to_str(),
                    self.finish_node(node),
                )
                .into());
            }
        }
        Ok(Expression::Set(Box::new(Set {
            node: self.finish_node(node),
            elements,
        })))
    }

    fn parse_dict(&mut self, node: Node) -> Result<Expression> {
        let mut keys = vec![];
        let mut values = vec![];
        while !self.eat(Kind::RightBracket) {
            let key = self.parse_expression()?;
            self.expect(Kind::Colon)?;
            let value = self.parse_expression()?;
            keys.push(key);
            values.push(value);
            if !self.eat(Kind::Comma) && !self.at(Kind::RightBracket) {
                return Err(diagnostics::ExpectToken(
                    Kind::Comma.to_str(),
                    self.cur_kind().to_str(),
                    node,
                )
                .into());
            }
        }
        Ok(Expression::Dict(Box::new(Dict {
            node: self.finish_node(node),
            keys,
            values,
        })))
    }

    fn parse_starred(&mut self) -> Result<Expression> {
        let mut node = self.start_node();
        self.bump(Kind::Mul);
        let starred_value_kind = self.cur_kind().clone();
        let expr = self.parse_expression()?;
        node = self.finish_node(node);
        if !is_iterable(&expr) {
            return Err(diagnostics::UnexpectedToken(starred_value_kind.to_str(), node).into());
        }
        Ok(Expression::Starred(Box::new(Starred {
            node: self.finish_node(node),
            value: expr,
        })))
    }

    fn parse_await(&mut self) -> Result<Expression> {
        let node = self.start_node();
        self.bump(Kind::Await);
        // we clone the token here to inform user in case
        // the expression is not a primary expression
        // it's more clear to say which token is unexpected
        // instead of saying some expression is unexpected
        let await_value_token = self.cur_token().clone();
        let await_value = self.parse_expression()?;
        if !is_primary(&await_value) {
            return Err(diagnostics::UnexpectedToken(
                await_value_token.kind.to_str(),
                self.finish_node(node),
            )
            .into());
        }
        Ok(Expression::Await(Box::new(Await {
            node: self.finish_node(node),
            value: Box::new(await_value),
        })))
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

    #[test]
    fn test_list() {
        for test_case in &["[a, b, c]"] {
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
    fn test_tuple() {
        for test_case in &["(a, b, c)"] {
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
    fn test_dict() {
        for test_case in &["{a: b, c: d}"] {
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
    fn parse_set() {
        for test_case in &["{a, b, c}"] {
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
    fn test_yield_expression() {
        for test_case in &["yield", "yield a", "yield from a"] {
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
    fn test_starred() {
        for test_case in &["*a"] {
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
    fn test_await_expression() {
        for test_case in &["await a"] {
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
    fn test_expressions_list() {
        for test_test in &["a, b, c"] {
            let mut parser = Parser::new(test_test.to_string());
            let program = parser.parse();

            insta::with_settings!({
                    description => test_test.to_string(), // the template source code
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }
}
