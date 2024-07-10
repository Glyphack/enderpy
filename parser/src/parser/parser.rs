use core::panic;

/// Some functions in this file have misleading names.
/// For example star expressions are defined slightly differently in python grammar and references.
/// So there might be duplicates of both. Try to migrate the wrong names to how they are called in:
/// https://docs.python.org/3/reference/grammar.html
use std::{sync::Arc, vec};

use miette::Result;

use super::{concat_string_exprs, is_at_compound_statement, is_iterable, map_unary_operator};
use crate::{
    error::ParsingError,
    lexer::Lexer,
    parser::{ast::*, extract_string_inside},
    token::{Kind, Token, TokenValue},
};

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Parser<'a> {
    pub identifiers_start_offset: Vec<(u32, u32, String)>,
    pub source: &'a str,
    lexer: Lexer<'a>,
    cur_token: Token,
    prev_token_end: u32,
    prev_nonwhitespace_token_end: u32,
    // This var keeps track of how many levels deep we are in a list, tuple or set
    // expression. This is used to determine if we should parse comma separated
    // expressions as tuple or not.
    // This is incremented when we see an opening bracket and decremented when we
    // see a closing bracket.
    nested_expression_list: u32,
    curr_line_string: String,
    path: &'a str,
}

#[allow(unused)]
impl<'a> Parser<'a> {
    pub fn line_starts(&self) -> Vec<u32> {
        self.lexer.line_starts.clone()
    }
    pub fn new(source: &'a str, path: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let cur_token = lexer.next_token();

        let mut nested_expression_list = 0;
        match cur_token.kind {
            Kind::LeftParen | Kind::LeftBrace | Kind::LeftBracket => nested_expression_list += 1,
            Kind::RightParen | Kind::RightBrace | Kind::RightBracket => nested_expression_list -= 1,
            _ => {}
        }
        let identifiers_offset = if cur_token.kind == Kind::Identifier {
            vec![(cur_token.start, cur_token.end, cur_token.value.to_string())]
        } else {
            vec![]
        };
        let prev_token_end = 0;

        Self {
            source,
            lexer,
            cur_token,
            prev_token_end,
            prev_nonwhitespace_token_end: prev_token_end,
            nested_expression_list,
            curr_line_string: String::new(),
            path,
            identifiers_start_offset: identifiers_offset,
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParsingError> {
        let node = self.start_node();
        let mut body = vec![];
        while self.cur_kind() != Kind::Eof {
            if self.consume_whitespace_and_comments() {
                continue;
            }
            let stmt = if is_at_compound_statement(self.cur_token()) {
                self.parse_compound_statement()
            } else {
                self.parse_simple_statement()
            };
            match stmt {
                Ok(stmt) => body.push(stmt),
                Err(err) => return Err(err),
            }
        }

        Ok(Module {
            node: self.finish_node(node),
            body,
        })
    }

    fn start_node(&self) -> Node {
        let token = self.cur_token();
        Node::new(token.start, 0)
    }

    fn finish_node(&self, node: Node) -> Node {
        Node::new(node.start, self.prev_token_end)
    }

    fn finish_node_chomped(&self, node: Node) -> Node {
        Node::new(node.start, self.prev_nonwhitespace_token_end)
    }

    pub(crate) fn cur_token(&self) -> &Token {
        &self.cur_token
    }

    fn cur_kind(&self) -> Kind {
        self.cur_token.kind
    }

    fn peek_token(&mut self) -> Result<Token, ParsingError> {
        let token = self.lexer.peek_token();
        if matches!(token.kind, Kind::Error) {
            panic!("Next token is Error {token:?}");
        }
        Ok(token)
    }

    fn peek_kind(&mut self) -> Result<Kind, ParsingError> {
        let token = self.peek_token()?;
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
        if token.kind == Kind::Identifier {
            self.identifiers_start_offset
                .push((token.start, token.end, token.value.to_string()));
        }

        match token.kind {
            Kind::LeftParen | Kind::LeftBrace | Kind::LeftBracket => {
                self.nested_expression_list += 1
            }
            Kind::RightParen | Kind::RightBrace | Kind::RightBracket => {
                self.nested_expression_list -= 1
            }
            _ => {}
        }

        self.prev_token_end = self.cur_token.end;
        if !matches!(self.cur_token.kind, Kind::WhiteSpace | Kind::NewLine | Kind::Dedent) {
            self.prev_nonwhitespace_token_end = self.prev_token_end;
        }
        self.cur_token = token;
        if matches!(self.cur_kind(), Kind::Comment | Kind::NL) {
            self.advance();
        }
        if self.nested_expression_list > 0 {
            self.consume_whitespace_and_newline();
        }
    }

    /// Expect a `Kind` or return error
    pub fn expect(&mut self, kind: Kind) -> Result<(), ParsingError> {
        if !self.at(kind) {
            let found = &self.cur_token;
            let file = &self.path;
            let line = &self.get_offset_line_number(found.start);
            panic!(
                "Expected {:?} but found {:?} {file} line: {line:}",
                kind, found
            );
        }
        self.bump_any();
        Ok(())
    }

    /// Expect any of `Kinds` or return error
    pub fn expect_any(&mut self, kind: Vec<Kind>) -> Result<(), ParsingError> {
        if !kind.contains(&self.cur_token.kind) {
            let mut expected = String::new();
            for kind in kind {
                expected.push_str(&format!("{:?}, ", kind));
            }
            let found = self.cur_token.kind;
            panic!("Expected one of {:?} but found {:?}", expected, found);
        }
        self.bump_any();
        Ok(())
    }

    fn unexpected_token_new(&mut self, node: Node, kinds: Vec<Kind>, advice: &str) -> ParsingError {
        let token = self.cur_token();
        panic!("Unexpected token {token:?}");
    }

    fn get_offset_line_number(&self, pos: u32) -> u32 {
        match self.lexer.line_starts.binary_search(&pos) {
            Ok(line) => line as u32 + 1,
            Err(line) => line as u32,
        }
    }

    fn parse_simple_statement(&mut self) -> Result<Statement, ParsingError> {
        let stmt = match self.cur_kind() {
            Kind::Assert => self.parse_assert_statement(),
            Kind::Pass => self.parse_pass_statement(),
            Kind::Del => self.parse_del_statement(),
            Kind::Return => self.parse_return_statement(),
            // https://docs.python.org/3/reference/simple_stmts.html#the-yield-statement
            Kind::Yield => Ok(Statement::ExpressionStatement(Box::new(
                self.parse_yield_expression()?,
            ))),
            Kind::Raise => self.parse_raise_statement(),
            Kind::Break => self.parse_break_statement(),
            Kind::Continue => self.parse_continue_statement(),
            Kind::Import => self.parse_import_statement(),
            Kind::From => self.parse_from_import_statement(),
            Kind::Global => self.parse_global_statement(),
            Kind::Nonlocal => self.parse_nonlocal_statement(),
            _ => {
                if self.cur_kind() == Kind::Identifier
                    && self.cur_token().value == TokenValue::Type
                    && self.peek_kind()? == Kind::Identifier
                {
                    self.parse_type_alias_statement()
                } else if self.cur_kind() == Kind::Indent {
                    let node = self.start_node();
                    let kind = self.cur_kind();
                    return Err(self.unexpected_token_new(
                        node,
                        vec![
                            Kind::Assert,
                            Kind::Pass,
                            Kind::Del,
                            Kind::Return,
                            Kind::Yield,
                            Kind::Raise,
                            Kind::Break,
                            Kind::Continue,
                            Kind::Import,
                            Kind::From,
                            Kind::Global,
                            Kind::Nonlocal,
                        ],
                        "Unexpted indent",
                    ));
                } else {
                    self.parse_assignment_or_expression_statement()
                }
            }
        }?;

        self.err_if_statement_not_ending_in_new_line_or_semicolon(stmt.get_node(), &stmt);

        Ok(stmt)
    }

    fn parse_compound_statement(&mut self) -> Result<Statement, ParsingError> {
        let stmt = match self.cur_kind() {
            Kind::If => self.parse_if_statement(),
            Kind::While => self.parse_while_statement(),
            Kind::For => self.parse_for_statement(),
            Kind::Try => self.parse_try_statement(),
            Kind::With => self.parse_with_statement(),
            Kind::Def => {
                let node = self.start_node();
                self.bump_any();
                self.parse_function_definition(node, vec![], false)
            }
            Kind::MatrixMul => self.parse_decorated_function_def_or_class_def(),
            Kind::Class => self.parse_class_definition(vec![], None),
            // match is a soft keyword
            // https://docs.python.org/3/reference/lexical_analysis.html#soft-keywords
            Kind::Identifier if self.cur_token().value == TokenValue::Match => {
                self.parse_match_statement()
            }
            Kind::Async => {
                if matches!(self.peek_kind(), Ok(Kind::Def)) {
                    let node = self.start_node();
                    self.bump_any();
                    self.bump_any();
                    self.parse_function_definition(node, vec![], true)
                } else if matches!(self.peek_kind(), Ok(Kind::For)) {
                    self.parse_for_statement()
                } else if matches!(self.peek_kind(), Ok(Kind::With)) {
                    self.parse_with_statement()
                } else {
                    let node = self.start_node();
                    let kind = self.cur_kind();
                    self.bump_any();
                    panic!("");
                }
            }
            _ => {
                let range = self.finish_node(self.start_node());
                panic!("Expected compound statement");
            }
        };

        stmt
    }

    fn err_if_statement_not_ending_in_new_line_or_semicolon(
        &mut self,
        node: Node,
        stmt: &Statement,
    ) {
        while self.eat(Kind::WhiteSpace) || self.eat(Kind::Comment) {}

        if !matches!(
            self.cur_kind(),
            Kind::NewLine | Kind::NL | Kind::SemiColon | Kind::Eof
        ) {
            panic!("Statement does not end in new line or semicolon {:?}", stmt);
        }
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.expect(Kind::If)?;
        let test = self.parse_named_expression()?;
        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;
        let mut orelse: Option<If> = None;
        while self.at(Kind::Elif) {
            let elif_node = self.start_node();
            self.bump(Kind::Elif);
            let elif_test = self.parse_named_expression()?;
            self.expect(Kind::Colon)?;
            let body = self.parse_suite()?;
            let if_value = If {
                node: self.finish_node(elif_node),
                test: elif_test,
                body,
                orelse: vec![],
            };
            if let Some(val) = &mut orelse {
                val.update_orelse(vec![Statement::IfStatement(Box::new(if_value))]);
            } else {
                orelse = Some(if_value);
            }
        }

        let mut single_else_body: Option<Vec<Statement>> = None;
        if self.eat(Kind::Else) {
            self.expect(Kind::Colon)?;
            let else_body = self.parse_suite()?;
            if let Some(val) = &mut orelse {
                val.update_orelse(else_body);
            } else {
                single_else_body = Some(else_body);
            }
        };

        // if we had any else or elif statements, we need to wrap them in a vec
        // otherwise we just return an empty vec as the else block of the if statement
        let or_else_vec = if let Some(val) = orelse {
            vec![Statement::IfStatement(Box::new(val))]
        } else {
            match single_else_body {
                Some(val) => val,
                None => vec![],
            }
        };

        Ok(Statement::IfStatement(Box::new(If {
            node: self.finish_node(node),
            test,
            body,
            orelse: or_else_vec,
        })))
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::While);
        let test = self.parse_named_expression()?;
        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;
        let orelse = if self.at(Kind::Else) {
            self.bump(Kind::Else);
            self.expect(Kind::Colon)?;
            self.parse_suite()?
        } else {
            vec![]
        };

        Ok(Statement::WhileStatement(Box::new(While {
            node: self.finish_node_chomped(node),
            test,
            body,
            orelse,
        })))
    }

    fn parse_for_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        let is_async = self.eat(Kind::Async);
        self.bump(Kind::For);
        let target = self.parse_target_list()?;
        self.expect(Kind::In)?;
        let iter_list = self.parse_starred_list(Kind::Colon)?;
        let iter = match iter_list.len() {
            0 => {
                return Err(self.unexpected_token_new(node, vec![], "Expected expression"));
            }
            1 => iter_list.into_iter().next().unwrap(),
            _ => Expression::Tuple(Box::new(Tuple {
                node: self.finish_node(node),
                elements: iter_list,
            })),
        };

        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;
        let orelse = if self.eat(Kind::Else) {
            self.expect(Kind::Colon)?;
            self.parse_suite()?
        } else {
            vec![]
        };

        if is_async {
            Ok(Statement::AsyncForStatement(Box::new(AsyncFor {
                node: self.finish_node(node),
                target,
                iter,
                body,
                orelse,
            })))
        } else {
            Ok(Statement::ForStatement(Box::new(For {
                node: self.finish_node(node),
                target,
                iter,
                body,
                orelse,
            })))
        }
    }

    fn parse_with_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        let is_async = self.eat(Kind::Async);
        self.bump(Kind::With);
        let items = self.parse_with_items()?;
        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;

        if is_async {
            Ok(Statement::AsyncWithStatement(Box::new(AsyncWith {
                node: self.finish_node_chomped(node),
                items,
                body,
            })))
        } else {
            Ok(Statement::WithStatement(Box::new(With {
                node: self.finish_node_chomped(node),
                items,
                body,
            })))
        }
    }

    fn parse_with_items(&mut self) -> Result<Vec<WithItem>, ParsingError> {
        let mut items = vec![];

        if self.eat(Kind::LeftParen) {
            items.push(self.parse_with_item()?);
            while self.eat(Kind::Comma) & !self.at(Kind::RightParen) {
                items.push(self.parse_with_item()?);
            }
            self.expect(Kind::RightParen)?;
            return Ok(items);
        }
        items.push(self.parse_with_item()?);
        while self.eat(Kind::Comma) & !self.at(Kind::Colon) {
            items.push(self.parse_with_item()?);
        }

        Ok(items)
    }

    fn parse_with_item(&mut self) -> Result<WithItem, ParsingError> {
        let node = self.start_node();
        let context_expr = self.parse_expression()?;
        let optional_vars = if self.eat(Kind::As) {
            Some(self.parse_target()?)
        } else {
            None
        };

        Ok(WithItem {
            node: self.finish_node(node),
            context_expr,
            optional_vars,
        })
    }

    fn parse_try_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        let mut is_try_star = false;
        self.expect(Kind::Try);
        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;
        let handlers = if self.at(Kind::Except) {
            if matches!(self.peek_kind(), Ok(Kind::Mul)) {
                is_try_star = true;
            }
            self.parse_except_clauses()?
        } else {
            vec![]
        };
        let orelse = if self.eat(Kind::Else) {
            self.expect(Kind::Colon)?;
            self.parse_suite()?
        } else {
            vec![]
        };

        let finalbody = if self.eat(Kind::Finally) {
            self.expect(Kind::Colon)?;
            self.parse_suite()?
        } else {
            vec![]
        };

        if is_try_star {
            Ok(Statement::TryStarStatement(Box::new(TryStar {
                node: self.finish_node_chomped(node),
                body,
                handlers,
                orelse,
                finalbody,
            })))
        } else {
            Ok(Statement::TryStatement(Box::new(Try {
                node: self.finish_node(node),
                body,
                handlers,
                orelse,
                finalbody,
            })))
        }
    }

    fn parse_except_clauses(&mut self) -> Result<Vec<ExceptHandler>, ParsingError> {
        let mut handlers = vec![];
        while self.at(Kind::Except) {
            let node = self.start_node();
            self.bump(Kind::Except);
            self.bump(Kind::Mul);
            let typ = if !self.at(Kind::Colon) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            let name = if self.eat(Kind::As) {
                let val = Some(self.cur_token().value.to_string());
                self.bump(Kind::Identifier);
                val
            } else {
                None
            };

            self.expect(Kind::Colon)?;
            let body = self.parse_suite()?;

            handlers.push(ExceptHandler {
                node: self.finish_node_chomped(node),
                typ,
                name,
                body,
            });
        }

        Ok(handlers)
    }

    fn parse_function_definition(
        &mut self,
        node: Node,
        decorators: Vec<Expression>,
        is_async: bool,
    ) -> Result<Statement, ParsingError> {
        let name = self.cur_token().value.to_string();
        self.expect(Kind::Identifier)?;
        let type_params = if self.at(Kind::LeftBrace) {
            self.parse_type_parameters()?
        } else {
            vec![]
        };
        self.expect(Kind::LeftParen)?;
        let args = self.parse_parameters(false)?;
        self.expect(Kind::RightParen)?;

        let return_type = if self.eat(Kind::Arrow) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;
        if is_async {
            Ok(Statement::AsyncFunctionDef(Box::new(AsyncFunctionDef {
                node: self.finish_node_chomped(node),
                name,
                args,
                body,
                decorator_list: decorators,
                returns: return_type,
                type_comment: None,
                type_params,
            })))
        } else {
            Ok(Statement::FunctionDef(Arc::new(FunctionDef {
                node: self.finish_node_chomped(node),
                name,
                args,
                body,
                decorator_list: decorators,
                returns: return_type,
                // TODO: type comment
                type_comment: None,
                type_params,
            })))
        }
    }

    fn parse_decorated_function_def_or_class_def(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        let mut decorators = vec![];
        while self.eat(Kind::MatrixMul) {
            let name = self.parse_named_expression()?;
            decorators.push(name);
            self.consume_whitespace_and_comments();
        }
        match self.cur_kind() {
            Kind::Def => {
                self.bump_any();
                self.parse_function_definition(node, decorators, false)
            }
            Kind::Async => {
                self.bump_any();
                self.expect(Kind::Def);
                self.parse_function_definition(node, decorators, true)
            }
            _ => self.parse_class_definition(decorators, Some(node)),
        }
    }

    fn parse_class_definition(
        &mut self,
        decorators: Vec<Expression>,
        node: Option<Node>,
    ) -> Result<Statement, ParsingError> {
        let node = if let Some(node) = node {
            node
        } else {
            self.start_node()
        };
        self.expect(Kind::Class)?;
        let name = self.cur_token().value.to_string();
        self.expect(Kind::Identifier)?;
        let type_params = if self.at(Kind::LeftBrace) {
            self.parse_type_parameters()?
        } else {
            vec![]
        };
        let (bases, keywords) = if self.eat(Kind::LeftParen) {
            let (bases, keywords) = self.parse_argument_list()?;
            self.expect(Kind::RightParen)?;
            (bases, keywords)
        } else {
            (vec![], vec![])
        };
        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;

        Ok(Statement::ClassDef(Arc::new(ClassDef {
            node: self.finish_node(node),
            name,
            bases,
            keywords,
            body,
            decorator_list: decorators,
            type_params,
        })))
    }

    // https://peps.python.org/pep-0622/#appendix-a-full-grammar
    fn parse_match_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        // This identifier is match word
        // match is a soft keyword
        self.bump(Kind::Identifier);
        let subject = self.parse_subject()?;
        self.expect(Kind::Colon)?;
        self.expect(Kind::NewLine)?;
        self.expect(Kind::Indent)?;
        let cases = self.parse_cases()?;
        self.expect_any(vec![Kind::Dedent, Kind::Eof])?;

        Ok(Statement::Match(Box::new(Match {
            node: self.finish_node(node),
            subject,
            cases,
        })))
    }

    // This is inaccuracy, but I don't know how
    // the grammar should be
    fn parse_subject(&mut self) -> Result<Expression, ParsingError> {
        self.parse_star_named_expressions()
    }

    // star named expressions is similar to starred expression
    // but it does not accept expression as a value
    // https://docs.python.org/3/reference/grammar.html
    fn parse_star_named_expression(&mut self) -> Result<Expression, ParsingError> {
        if self.at(Kind::Mul) {
            self.parse_or_expr()
        } else {
            self.parse_named_expression()
        }
    }

    fn parse_star_named_expressions(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut exprs = vec![self.parse_star_named_expression()?];
        loop {
            if !self.eat(Kind::Comma) {
                break;
            }
            exprs.push(self.parse_star_named_expression()?);
        }

        if exprs.len() == 1 {
            Ok(exprs.remove(0))
        } else {
            Ok(Expression::Tuple(Box::new(Tuple {
                node: self.finish_node(node),
                elements: exprs,
            })))
        }
    }

    fn parse_cases(&mut self) -> Result<Vec<MatchCase>, ParsingError> {
        let mut cases = vec![];
        loop {
            if self.at(Kind::Dedent) || self.at(Kind::Eof) {
                break;
            }
            let node = self.start_node();

            // This identifier is case word
            self.expect(Kind::Identifier)?;
            let pattern = self.parse_patterns()?;
            let guard = if self.at(Kind::If) {
                Some(self.parse_guard()?)
            } else {
                None
            };
            self.expect(Kind::Colon)?;
            let body = self.parse_suite()?;
            cases.push(MatchCase {
                node: self.finish_node(node),
                pattern,
                guard,
                body,
            });
        }

        Ok(cases)
    }

    fn parse_guard(&mut self) -> Result<Expression, ParsingError> {
        self.expect(Kind::If)?;
        self.parse_named_expression()
    }

    // https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-patterns
    // The open sequence pattern is either a pattern or maybe star pattern
    // Here we expect at least one ( pattern or maybe star pattern )
    fn parse_patterns(&mut self) -> Result<MatchPattern, ParsingError> {
        let mut patterns = self.parse_open_sequence_pattern()?;

        if patterns.len() == 1 {
            Ok(patterns.remove(0))
        } else {
            Ok(MatchPattern::MatchSequence(patterns))
        }
    }

    fn parse_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        let or_pattern = self.parse_or_pattern()?;

        if self.at(Kind::As) {
            let node = self.start_node();
            let name = Some(self.cur_token().value.to_string());
            self.bump(Kind::As);
            Ok(MatchPattern::MatchAs(Box::new(MatchAs {
                node: self.finish_node(node),
                pattern: Some(or_pattern),
                name,
            })))
        } else {
            Ok(or_pattern)
        }
    }

    fn parse_or_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        let mut patterns = vec![];
        patterns.push(self.parse_closed_pattern()?);
        loop {
            if !self.eat(Kind::BitOr) {
                break;
            }
            patterns.push(self.parse_closed_pattern()?);
        }
        if patterns.len() == 1 {
            Ok(patterns.remove(0))
        } else {
            Ok(MatchPattern::MatchOr(patterns))
        }
    }

    fn parse_closed_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        match self.cur_kind() {
            Kind::LeftParen => self.parse_sequence_pattern(),
            Kind::LeftBrace => self.parse_sequence_pattern(),
            Kind::LeftBracket => self.parse_mapping_pattern(),
            Kind::Identifier => {
                if matches!(self.peek_kind(), Ok(Kind::Dot)) {
                    let node = self.start_node();
                    let value = self.parse_attr()?;
                    if self.at(Kind::LeftParen) {
                        self.parse_class_pattern(value)
                    } else {
                        self.parse_value_pattern(value, node)
                    }
                } else if matches!(self.peek_kind(), Ok(Kind::LeftParen)) {
                    let value = self.parse_attr()?;
                    self.parse_class_pattern(value)
                } else {
                    self.parse_capture_or_wildcard_pattern()
                }
        },
            Kind::Integer
            | Kind::Binary
            | Kind::Octal
            | Kind::Hexadecimal
            | Kind::PointFloat
            | Kind::ExponentFloat
            | Kind::ImaginaryInteger
            | Kind::ImaginaryPointFloat
            | Kind::ImaginaryExponentFloat
            | Kind::None
            | Kind::True
            | Kind::False
            | Kind::StringLiteral | Kind::RawBytes | Kind::Bytes
            // The signed numbers are also allowed
            | Kind::Minus | Kind::Plus => {
                self.parse_literal_pattern()

                },
            _ => {
                let node = self.start_node();
                Err(self.unexpected_token_new(
                    node,
                    vec![
                        Kind::LeftParen,
                        Kind::LeftBrace,
                        Kind::LeftBracket,
                        Kind::Identifier,
                        Kind::Integer,
                        Kind::Binary,
                        Kind::Octal,
                        Kind::Hexadecimal,
                        Kind::PointFloat,
                        Kind::ExponentFloat,
                        Kind::ImaginaryInteger,
                        Kind::ImaginaryPointFloat,
                        Kind::ImaginaryExponentFloat,
                        Kind::None,
                        Kind::True,
                        Kind::False,
                        Kind::StringLiteral,
                        Kind::RawBytes,
                        Kind::Bytes,
                        Kind::Minus,
                        Kind::Plus,
                    ],
                    "A match pattern starts with these characters",
                ))
            },
        }
    }

    // https://docs.python.org/3/reference/compound_stmts.html#literal-patterns
    fn parse_literal_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        let node = self.start_node();
        let value = self.parse_binary_arithmetic_operation()?;
        Ok(MatchPattern::MatchValue(MatchValue {
            node: self.finish_node(node),
            value,
        }))
    }

    fn parse_capture_or_wildcard_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        let capture_value = self.cur_token().value.to_string();
        let node = self.start_node();
        self.expect(Kind::Identifier)?;
        // TODO: should also accept as?

        if capture_value == "_" {
            Ok(MatchPattern::MatchAs(Box::new(MatchAs {
                node: self.finish_node(node),
                name: None,
                pattern: None,
            })))
        } else {
            Ok(MatchPattern::MatchAs(Box::new(MatchAs {
                node: self.finish_node(node),
                name: Some(capture_value),
                pattern: None,
            })))
        }
    }

    // https://docs.python.org/3/reference/compound_stmts.html#value-patterns
    // This pattern shares the value logic with class pattern
    // so we pass that part to this method
    fn parse_value_pattern(
        &mut self,
        value: Expression,
        node: Node,
    ) -> Result<MatchPattern, ParsingError> {
        Ok(MatchPattern::MatchValue(MatchValue {
            node: self.finish_node(node),
            value,
        }))
    }

    // This parse attr does not allow anything other than names
    // in contrast to attribute parsing in primary expression
    fn parse_attr(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let id = self.cur_token.value.take_string();
        let mut expr = Ok(Expression::Name(Box::new(Name {
            node: self.finish_node(node),
            id,
            parenthesized: false,
        })));
        self.expect(Kind::Identifier);
        while self.eat(Kind::Dot) {
            let attr_val = self.cur_token().value.to_string();
            self.expect(Kind::Identifier)?;
            expr = Ok(Expression::Attribute(Box::new(Attribute {
                node: self.finish_node(node),
                value: expr?,
                attr: attr_val,
            })));
        }
        expr
    }

    // TODO: This has precedence over sequence pattern but I'm not sure
    // what is the right way to use it.
    fn parse_group_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        self.expect(Kind::LeftParen)?;
        let pattern = self.parse_pattern()?;
        self.expect(Kind::RightParen)?;
        Ok(pattern)
    }

    fn parse_mapping_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        let node = self.start_node();
        self.expect(Kind::LeftBracket)?;
        let mut keys = vec![];
        let mut patterns = vec![];
        let mut rest = None;
        loop {
            if self.eat(Kind::RightBracket) {
                break;
            }
            if self.eat(Kind::Pow) {
                rest = Some(self.cur_token().value.to_string());
                self.bump(Kind::Identifier);
                // consume the trailing comma
                self.bump(Kind::Comma);
                // rest is the last element so we expect the closing bracket
                self.expect(Kind::RightBracket)?;
                break;
            } else {
                // TODO: here we cannot accept all primary expressions
                // but python docs do not have the full list of what is allowed
                // so we just do this for now
                keys.push(self.parse_primary(None)?);
                self.expect(Kind::Colon)?;
                patterns.push(self.parse_pattern()?);
            }

            if !self.at(Kind::RightBracket) {
                self.expect(Kind::Comma)?;
            }
        }

        Ok(MatchPattern::MatchMapping(MatchMapping {
            node: self.finish_node(node),
            keys,
            patterns,
            rest,
        }))
    }

    fn parse_literal_or_value_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        if self.cur_kind() == Kind::Identifier && !matches!(self.peek_kind(), Ok(Kind::Colon)) {
            let node = self.start_node();
            let value = self.parse_attr()?;
            self.parse_value_pattern(value, node)
        } else {
            self.parse_literal_pattern()
        }
    }

    fn parse_class_pattern(
        &mut self,
        class_name: Expression,
    ) -> Result<MatchPattern, ParsingError> {
        let node = self.start_node();
        let class = class_name;
        self.expect(Kind::LeftParen)?;
        let mut patterns = vec![];
        let mut kwd_attrs = vec![];
        let mut kwd_patterns = vec![];
        let mut seen_keyword_pattern = false;
        loop {
            if self.eat(Kind::RightParen) {
                break;
            }

            if self.at(Kind::Identifier) && matches!(self.peek_kind(), Ok(Kind::Assign)) {
                seen_keyword_pattern = true;
                kwd_attrs.push(self.cur_token().value.to_string());
                self.bump(Kind::Identifier);
                self.bump(Kind::Assign);
                kwd_patterns.push(self.parse_pattern()?);
            } else {
                if seen_keyword_pattern {
                    panic!("Positional arguments cannot come after keyword arguments.");
                }
                patterns.push(self.parse_pattern()?);
            }
            if !self.at(Kind::RightParen) {
                self.expect(Kind::Comma)?;
            }
        }
        Ok(MatchPattern::MatchClass(MatchClass {
            node: self.finish_node(node),
            cls: class,
            patterns,
            kwd_attrs,
            kwd_patterns,
        }))
    }

    fn parse_sequence_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        let node = self.start_node();
        if self.eat(Kind::LeftBrace) {
            let pattern = self.parse_maybe_sequence_pattern()?;
            self.expect(Kind::RightBrace)?;
            Ok(MatchPattern::MatchSequence(pattern))
        } else if self.eat(Kind::LeftParen) {
            let pattern = self.parse_open_sequence_pattern()?;
            self.expect(Kind::RightParen)?;
            Ok(MatchPattern::MatchSequence(pattern))
        } else {
            Err(self.unexpected_token_new(
                node,
                vec![Kind::LeftBrace, Kind::LeftParen],
                "Write a sequence pattern here",
            ))
        }
    }

    fn parse_open_sequence_pattern(&mut self) -> Result<Vec<MatchPattern>, ParsingError> {
        let mut patterns = vec![];
        patterns.push(self.parse_maybe_star_pattern()?);
        loop {
            if !self.eat(Kind::Comma) {
                break;
            }
            patterns.push(self.parse_maybe_star_pattern()?);
        }
        Ok(patterns)
    }

    fn parse_maybe_sequence_pattern(&mut self) -> Result<Vec<MatchPattern>, ParsingError> {
        let mut patterns = vec![];
        loop {
            if self.at(Kind::RightBrace) {
                break;
            }
            patterns.push(self.parse_maybe_star_pattern()?);
            if !self.at(Kind::RightBrace) {
                self.expect(Kind::Comma)?;
            }
        }
        Ok(patterns)
    }
    fn parse_maybe_star_pattern(&mut self) -> Result<MatchPattern, ParsingError> {
        if self.eat(Kind::Mul) {
            self.parse_capture_or_wildcard_pattern()
        } else {
            self.parse_pattern()
        }
    }

    fn parse_assignment_or_expression_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        let lhs = self.parse_expressions()?;

        if self.cur_kind() == Kind::Assign {
            self.parse_assignment_statement(node, lhs)
        } else if let Some(op) = self.parse_aug_assign_op() {
            self.parse_aug_assignment_statement(node, lhs, op)
        } else if self.at(Kind::Colon) {
            self.parse_ann_assign_statement(node, lhs)
        } else {
            Ok(Statement::ExpressionStatement(Box::new(lhs)))
        }
    }

    // https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-suite
    fn parse_suite(&mut self) -> Result<Vec<Statement>, ParsingError> {
        if self.eat(Kind::NewLine) {
            self.consume_whitespace_and_newline();
            self.expect(Kind::Indent)?;
            let mut stmts = vec![];
            while !self.eat(Kind::Dedent) && !self.at(Kind::Eof) {
                if self.eat(Kind::Comment) || self.consume_whitespace_and_newline() {
                    continue;
                }
                let stmt = self.parse_statement()?;
                stmts.extend(stmt);
            }
            Ok(stmts)
        } else {
            let stmt = self.parse_statement_list()?;
            Ok(stmt)
        }
    }

    // https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-statement
    fn parse_statement(&mut self) -> Result<Vec<Statement>, ParsingError> {
        if is_at_compound_statement(self.cur_token()) {
            let comp_stmt = self.parse_compound_statement()?;
            Ok(vec![comp_stmt])
        } else {
            self.parse_statement_list()
        }
    }

    // https://docs.python.org/3/reference/simple_stmts.html#grammar-token-python-grammar-stmt-list
    fn parse_statement_list(&mut self) -> Result<Vec<Statement>, ParsingError> {
        let mut stmts = vec![];
        let stmt = self.parse_simple_statement()?;
        stmts.push(stmt);
        while self.eat(Kind::SemiColon) {
            let stmt = self.parse_simple_statement()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn parse_del_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::Del);
        let expr = self.parse_target_list()?;
        // target list can return a tuple for a,b
        // but in delete a,b the a,b part is not a tuple
        // It's a list of targets
        // so we need to unwrap the tuple if it's a tuple
        let expr = match expr {
            Expression::Tuple(tuple) => tuple.elements,
            _ => vec![expr],
        };
        Ok(Statement::Delete(Box::new(Delete {
            node: self.finish_node(node),
            targets: expr,
        })))
    }

    fn parse_assignment_statement(
        &mut self,
        start: Node,
        lhs: Expression,
    ) -> Result<Statement, ParsingError> {
        let mut targets = vec![lhs];
        self.bump(Kind::Assign);
        let value = loop {
            let rhs = self.parse_expressions()?;
            // if there's an assign after the expression we have multiple targets
            // like a = b = 1
            // so we add the rhs to the targets and continue parsing
            // otherwise we break and return the rhs as the value
            if self.eat(Kind::Assign) {
                targets.push(rhs);
            } else {
                break rhs;
            }
        };
        Ok(Statement::AssignStatement(Box::new(Assign {
            node: self.finish_node(start),
            targets,
            value,
        })))
    }

    fn parse_aug_assignment_statement(
        &mut self,
        start: Node,
        lhs: Expression,
        op: AugAssignOp,
    ) -> Result<Statement, ParsingError> {
        let value = self.parse_assignment_value()?;

        Ok(Statement::AugAssignStatement(Box::new(AugAssign {
            node: self.finish_node(start),
            target: lhs,
            op,
            value,
        })))
    }

    fn parse_ann_assign_statement(
        &mut self,
        start: Node,
        lhs: Expression,
    ) -> Result<Statement, ParsingError> {
        self.bump(Kind::Colon);
        let annotation = self.parse_expression()?;
        let value = if self.eat(Kind::Assign) {
            Some(self.parse_assignment_value()?)
        } else {
            None
        };
        let simple = if let Expression::Name(name) = &lhs {
            !name.parenthesized
        } else {
            false
        };
        Ok(Statement::AnnAssignStatement(Box::new(AnnAssign {
            node: self.finish_node(start),
            target: lhs,
            annotation,
            value,
            simple,
        })))
    }

    // The value is either expression list or yield expression
    // https://docs.python.org/3/reference/simple_stmts.html#assignment-statements
    fn parse_assignment_value(&mut self) -> Result<Expression, ParsingError> {
        if self.cur_kind() == Kind::Yield {
            return self.parse_yield_expression();
        }
        self.parse_expression_list()
    }

    fn parse_aug_assign_op(&mut self) -> Option<AugAssignOp> {
        let op = match self.cur_kind() {
            Kind::AddAssign => AugAssignOp::Add,
            Kind::SubAssign => AugAssignOp::Sub,
            Kind::MulAssign => AugAssignOp::Mult,
            Kind::DivAssign => AugAssignOp::Div,
            Kind::IntDivAssign => AugAssignOp::FloorDiv,
            Kind::ModAssign => AugAssignOp::Mod,
            Kind::PowAssign => AugAssignOp::Pow,
            Kind::BitAndAssign => AugAssignOp::BitAnd,
            Kind::BitOrAssign => AugAssignOp::BitOr,
            Kind::BitXorAssign => AugAssignOp::BitXor,
            Kind::ShiftLeftAssign => AugAssignOp::LShift,
            Kind::ShiftRightAssign => AugAssignOp::RShift,
            _ => return None,
        };
        self.bump_any();
        Some(op)
    }

    fn parse_assert_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.expect(Kind::Assert)?;
        let test = self.parse_expression()?;
        let msg = if self.eat(Kind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(Statement::Assert(Box::new(Assert {
            node: self.finish_node(node),
            test,
            msg,
        })))
    }

    fn parse_pass_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::Pass);
        Ok(Statement::Pass(Box::new(Pass {
            node: self.finish_node(node),
        })))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::Return);
        let value = if self.at(Kind::NewLine) {
            None
        } else {
            Some(self.parse_expression_list()?)
        };
        Ok(Statement::Return(Box::new(Return {
            node: self.finish_node(node),
            value,
        })))
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement
    fn parse_raise_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::Raise);
        let exc = if matches!(self.cur_kind(), Kind::NewLine | Kind::Eof) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let cause = if self.eat(Kind::From) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok(Statement::Raise(Box::new(Raise {
            node: self.finish_node(node),
            exc,
            cause,
        })))
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-break-statement
    fn parse_break_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::Break);
        Ok(Statement::Break(Box::new(Break {
            node: self.finish_node(node),
        })))
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-continue-statement
    fn parse_continue_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::Continue);
        Ok(Statement::Continue(Box::new(Continue {
            node: self.finish_node(node),
        })))
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-global-statement
    fn parse_global_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::Global);
        let mut names = vec![];
        while self.at(Kind::Identifier) {
            let name = self.cur_token().value.to_string();
            names.push(name);
            self.bump(Kind::Identifier);
            if !self.eat(Kind::Comma) {
                break;
            }
        }
        Ok(Statement::Global(Box::new(Global {
            node: self.finish_node(node),
            names,
        })))
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-nonlocal-statement
    fn parse_nonlocal_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::Nonlocal);
        let mut names = vec![];
        while self.at(Kind::Identifier) {
            let name = self.cur_token().value.to_string();
            names.push(name);
            self.bump(Kind::Identifier);
            if !self.eat(Kind::Comma) {
                break;
            }
        }
        Ok(Statement::Nonlocal(Box::new(Nonlocal {
            node: self.finish_node(node),
            names,
        })))
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-import-statement
    fn parse_import_statement(&mut self) -> Result<Statement, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::Import);
        let mut aliases = vec![];
        while self.at(Kind::Identifier) {
            let node = self.start_node();
            let (module, _) = self.parse_module_name()?;
            let alias = self.parse_alias(module, node);
            aliases.push(alias);

            if !self.eat(Kind::Comma) {
                break;
            }
        }
        Ok(Statement::Import(Box::new(Import {
            node: self.finish_node(node),
            names: aliases,
        })))
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-from-import-statement
    fn parse_from_import_statement(&mut self) -> Result<Statement, ParsingError> {
        let import_node = self.start_node();
        self.bump(Kind::From);
        let (module, level) = self.parse_module_name()?;
        self.bump(Kind::Import);
        let mut aliases = vec![];
        if self.eat(Kind::LeftParen) {
            while self.at(Kind::Identifier) {
                let alias_name = self.start_node();
                let name = self.cur_token().value.to_string();
                self.bump(Kind::Identifier);
                let asname = self.parse_alias(name, alias_name);
                aliases.push(asname);
                if !self.eat(Kind::Comma) {
                    break;
                }
            }
            self.expect(Kind::RightParen)?;
        } else if self.at(Kind::Identifier) {
            while self.at(Kind::Identifier) {
                let alias_name = self.start_node();
                let name = self.cur_token().value.to_string();
                self.bump(Kind::Identifier);
                let asname = self.parse_alias(name, alias_name);
                aliases.push(asname);
                if !self.eat(Kind::Comma) {
                    break;
                }
            }
        } else if self.at(Kind::Mul) {
            let node = self.start_node();
            self.bump_any();
            aliases.push(self.parse_alias("*".to_string(), node));
        } else {
            return Err(self.unexpected_token_new(
               import_node,
               vec![Kind::Identifier, Kind::Mul, Kind::LeftParen],
               "Use * for importing everything or use () to specify names to import or specify the name you want to import"
           ));
        }
        Ok(Statement::ImportFrom(Box::new(ImportFrom {
            node: self.finish_node(import_node),
            module,
            names: aliases,
            level,
        })))
    }

    fn parse_alias(&mut self, name: String, node: Node) -> Alias {
        let asname = if self.eat(Kind::As) {
            let alias_name = self.cur_token().value.to_string();
            self.bump(Kind::Identifier);
            Some(alias_name)
        } else {
            None
        };
        Alias {
            node: self.finish_node(node),
            name,
            asname,
        }
    }

    fn parse_module_name(&mut self) -> Result<(String, usize), ParsingError> {
        let mut level = 0;
        while self.at(Kind::Dot) | self.at(Kind::Ellipsis) {
            match self.cur_kind() {
                Kind::Dot => {
                    level += 1;
                }
                Kind::Ellipsis => {
                    level += 3;
                }
                _ => {
                    panic!("lol");
                }
            }
            self.bump_any();
        }
        let mut module = String::from("");
        if self.at(Kind::Identifier) {
            module += &self.cur_token().value.to_string();
            self.bump_any();
            while self.eat(Kind::Dot) {
                module.push('.');
                module.push_str(self.cur_token().value.to_string().as_str());
                self.expect(Kind::Identifier);
            }
        }
        Ok((module, level))
    }

    // https://docs.python.org/3/library/ast.html#ast.Expr
    fn parse_expressions(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let expr = self.parse_expression()?;

        let mut exprs = vec![];
        if self.at(Kind::Comma) {
            exprs.push(expr);
            while self.eat(Kind::Comma) {
                // expressions start with same tokens as star
                // expressions
                if self.at(Kind::Eof) || !self.cur_kind().is_star_expression() {
                    break;
                }
                exprs.push(self.parse_expression()?);
            }
        } else {
            return Ok(expr);
        }

        Ok(Expression::Tuple(Box::new(Tuple {
            node: self.finish_node(node),
            elements: exprs,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#conditional-expressions
    fn parse_conditional_expression(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let or_test = self.parse_or_test();
        if self.eat(Kind::If) {
            let test = self.parse_or_test()?;
            self.expect(Kind::Else)?;
            let or_else = self.parse_expression()?;
            return Ok(Expression::IfExp(Box::new(IfExp {
                node: self.finish_node(node),
                test,
                body: or_test?,
                orelse: or_else,
            })));
        }

        or_test
    }

    // https://docs.python.org/3/reference/expressions.html#assignment-expressions
    fn parse_named_expression(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        if self.at(Kind::Identifier) && matches!(self.peek_kind()?, Kind::Walrus) {
            let identifier = self.cur_token().value.to_string();
            let mut identifier_node = self.start_node();
            identifier_node = self.finish_node(identifier_node);
            self.expect(Kind::Identifier)?;
            identifier_node = self.finish_node(identifier_node);
            if self.eat(Kind::Walrus) {
                let value = self.parse_expression()?;
                return Ok(Expression::NamedExpr(Box::new(NamedExpression {
                    node: self.finish_node(node),
                    target: Expression::Name(Box::new(Name {
                        node: identifier_node,
                        id: identifier,
                        parenthesized: false,
                    })),
                    value,
                })));
            }
            return Ok(Expression::Name(Box::new(Name {
                node: identifier_node,
                id: identifier,
                parenthesized: false,
            })));
        }

        self.parse_expression()
    }

    // https://docs.python.org/3/reference/expressions.html#list-displays
    fn parse_list(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::LeftBrace);
        self.consume_whitespace_and_newline();
        if self.eat(Kind::RightBrace) {
            return Ok(Expression::List(Box::new(List {
                node: self.finish_node(node),
                elements: vec![],
            })));
        }
        let started_with_star = self.at(Kind::Mul);
        let first_elm = self.parse_star_named_expression()?;
        if !started_with_star
            && self.at(Kind::For)
            && (self.at(Kind::For)
                || self.at(Kind::Async) && matches!(self.peek_kind(), Ok(Kind::For)))
        {
            let generators = self.parse_comp_for()?;
            self.expect(Kind::RightBrace)?;
            return Ok(Expression::ListComp(Box::new(ListComp {
                node: self.finish_node(node),
                element: first_elm,
                generators,
            })));
        }
        self.bump(Kind::Comma);
        let rest = self.parse_starred_list(Kind::RightBrace)?;
        let elements = vec![first_elm];
        let elements = elements.into_iter().chain(rest).collect();
        self.expect(Kind::RightBrace)?;
        Ok(Expression::List(Box::new(List {
            node: self.finish_node(node),
            elements,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#parenthesized-forms
    fn parse_paren_form_or_generator(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        self.expect(Kind::LeftParen)?;
        if self.at(Kind::RightParen) {
            return Ok(Expression::Tuple(Box::new(Tuple {
                node: self.finish_node(node),
                elements: vec![],
            })));
        }
        // paren form starts with either an expression or a star expression
        // Generator starts with an expression
        // we need to first check if we have a generator or a paren form
        // we do this by checking if the next expression is assignment expression
        // if not we consume the expression and check if the next token is a for
        // if not we have a paren form
        // The first expression we consume have three cases
        // Either an starred item https://docs.python.org/3/reference/expressions.html#grammar-token-python-grammar-starred_expression
        // or an assignment expression
        let first_expr =
            if self.at(Kind::Identifier) && matches!(self.peek_kind(), Ok(Kind::Walrus)) {
                self.parse_named_expression()?
            } else if self.eat(Kind::Mul) {
                let expr = self.parse_or_expr()?;
                Expression::Starred(Box::new(Starred {
                    node: self.finish_node(node),
                    value: expr,
                }))
            } else {
                self.parse_expression()?
            };

        if matches!(self.cur_kind(), Kind::For) || matches!(self.peek_kind(), Ok(Kind::For)) {
            let generators = self.parse_comp_for()?;
            self.expect(Kind::RightParen)?;
            return Ok(Expression::Generator(Box::new(Generator {
                node: self.finish_node(node),
                element: first_expr,
                generators,
            })));
        }

        let expr = self.parse_starred_expression(node, first_expr)?;
        Ok(expr)
    }

    // https://docs.python.org/3/reference/expressions.html#displays-for-lists-sets-and-dictionaries
    fn parse_comp_for(&mut self) -> Result<Vec<Comprehension>, ParsingError> {
        // if current token is async
        let node = self.start_node();
        let is_async = self.eat(Kind::Async);

        let mut generators = vec![];
        loop {
            self.expect(Kind::For)?;
            let target = self.parse_target_list()?;
            self.expect(Kind::In)?;
            let iter = self.parse_or_test()?;
            let ifs = if self.eat(Kind::If) {
                let mut ifs = vec![];
                loop {
                    ifs.push(self.parse_or_test()?);
                    if !self.eat(Kind::If) {
                        break;
                    }
                }
                ifs
            } else {
                vec![]
            };
            generators.push(Comprehension {
                node: self.finish_node(node),
                target,
                iter,
                ifs,
                is_async,
            });
            if !matches!(self.cur_kind(), Kind::For) {
                break;
            }
        }
        Ok(generators)
    }

    // https://docs.python.org/3/reference/simple_stmts.html#grammar-token-python-grammar-target_list
    fn parse_target_list(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut targets = vec![];
        loop {
            targets.push(self.parse_target()?);
            if !self.eat(Kind::Comma) || matches!(self.cur_kind(), Kind::NewLine | Kind::Eof) {
                break;
            }
        }
        if targets.len() == 1 {
            Ok(targets.remove(0))
        } else {
            Ok(Expression::Tuple(Box::new(Tuple {
                node: self.finish_node(node),
                elements: targets,
            })))
        }
    }

    // https://docs.python.org/3/reference/simple_stmts.html#grammar-token-python-grammar-target
    fn parse_target(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut targets = vec![];
        let target = match self.cur_kind() {
            Kind::Identifier => match self.peek_kind() {
                Ok(Kind::LeftBrace) => {
                    let atom = self.parse_atom()?;
                    self.parse_subscript(node, atom)?
                }
                Ok(Kind::Dot) => {
                    let atom = self.parse_atom()?;
                    self.parse_attribute_ref(node, atom)?
                }
                _ => {
                    let identifier = self.cur_token().value.to_string();
                    let mut identifier_node = self.start_node();
                    identifier_node = self.finish_node(identifier_node);
                    self.expect(Kind::Identifier)?;
                    identifier_node = self.finish_node(identifier_node);
                    return Ok(Expression::Name(Box::new(Name {
                        node: identifier_node,
                        id: identifier,
                        parenthesized: false,
                    })));
                }
            },
            Kind::LeftBrace => {
                let mut elements = vec![];
                self.bump(Kind::LeftBrace);
                loop {
                    elements.push(self.parse_target()?);
                    if !self.eat(Kind::Comma) {
                        break;
                    }
                }
                self.expect(Kind::RightBrace)?;
                Expression::List(Box::new(List {
                    node: self.finish_node(node),
                    elements,
                }))
            }
            Kind::LeftParen => {
                let mut targets = vec![];
                self.bump(Kind::LeftParen);
                loop {
                    targets.push(self.parse_target()?);
                    if !self.eat(Kind::Comma) {
                        break;
                    }
                }
                self.expect(Kind::RightParen)?;
                if targets.len() == 1 {
                    targets.pop().unwrap()
                } else {
                    Expression::Tuple(Box::new(Tuple {
                        node: self.finish_node(node),
                        elements: targets,
                    }))
                }
            }
            Kind::Mul => Expression::Starred(Box::new(Starred {
                node: self.finish_node(node),
                value: self.parse_target()?,
            })),
            _ => panic!("invalid target"),
        };
        targets.push(target);
        while self.eat(Kind::Comma) {
            // check if current kind can be start of a target
            if !matches!(
                self.cur_kind(),
                Kind::Identifier | Kind::LeftBrace | Kind::LeftParen | Kind::Mul
            ) {
                break;
            }
            let next_target = self.parse_target()?;
            targets.push(next_target);
        }

        if targets.len() == 1 {
            Ok(targets.remove(0))
        } else {
            Ok(Expression::Tuple(Box::new(Tuple {
                node: self.finish_node(node),
                elements: targets,
            })))
        }
    }

    fn parse_dict_or_set(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::LeftBracket);
        if self.eat(Kind::RightBracket) {
            return Ok(Expression::Dict(Box::new(Dict {
                node: self.finish_node(node),
                keys: vec![],
                values: vec![],
            })));
        }
        if self.at(Kind::Pow) {
            // key must be None
            let (key, value) = self.parse_double_starred_kv_pair()?;
            return self.parse_dict(node, key, value);
        }
        let first_key_or_element = self.parse_star_named_expression()?;
        if matches!(
            self.cur_kind(),
            Kind::Comma | Kind::RightBracket | Kind::Async | Kind::For | Kind::Walrus
        ) {
            self.parse_set(node, first_key_or_element)
        } else {
            self.expect(Kind::Colon)?;
            let first_value = self.parse_expression()?;
            self.parse_dict(node, Some(first_key_or_element), first_value)
        }
    }

    // https://docs.python.org/3/reference/expressions.html#set-displays
    fn parse_set(&mut self, node: Node, first_elm: Expression) -> Result<Expression, ParsingError> {
        if !matches!(first_elm, Expression::Starred(_))
            && self.at(Kind::For)
            && (self.at(Kind::For)
                || self.at(Kind::Async) && matches!(self.peek_kind(), Ok(Kind::For)))
        {
            let generators = self.parse_comp_for()?;
            self.consume_whitespace_and_newline();
            self.expect(Kind::RightBracket)?;
            return Ok(Expression::SetComp(Box::new(SetComp {
                node: self.finish_node(node),
                element: first_elm,
                generators,
            })));
        }
        self.bump(Kind::Comma);
        let rest = self.parse_starred_list(Kind::RightBracket)?;
        let mut elements = vec![first_elm];
        elements.extend(rest);
        self.expect(Kind::RightBracket)?;
        Ok(Expression::Set(Box::new(Set {
            node: self.finish_node(node),
            elements,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#dictionary-displays
    fn parse_dict(
        &mut self,
        node: Node,
        first_key: Option<Expression>,
        first_value: Expression,
    ) -> Result<Expression, ParsingError> {
        if self.at(Kind::For) || self.at(Kind::Async) && matches!(self.peek_kind(), Ok(Kind::For)) {
            let Some(key) = first_key else {
                panic!("cannot use ** in dict comprehension");
            };

            // make sure the first key is some
            let generators = self.parse_comp_for()?;
            self.expect(Kind::RightBracket)?;
            Ok(Expression::DictComp(Box::new(DictComp {
                node: self.finish_node(node),
                key,
                value: first_value,
                generators,
            })))
        } else {
            // we already consumed the first pair
            // so if there are more pairs we need to consume the comma
            if !self.at(Kind::RightBracket) {
                self.expect(Kind::Comma)?;
                self.consume_whitespace_and_newline();
            }
            let mut keys = match first_key {
                Some(k) => vec![k],
                None => vec![],
            };
            let mut values = vec![first_value];
            while !self.eat(Kind::RightBracket) {
                let (key, value) = self.parse_double_starred_kv_pair()?;

                if let Some(k) = key {
                    keys.push(k)
                }

                values.push(value);
                if !self.at(Kind::RightBracket) {
                    self.expect(Kind::Comma)?;
                    self.consume_whitespace_and_newline();
                }
            }
            Ok(Expression::Dict(Box::new(Dict {
                node: self.finish_node(node),
                keys,
                values,
            })))
        }
    }

    fn parse_double_starred_kv_pair(
        &mut self,
    ) -> Result<(Option<Expression>, Expression), ParsingError> {
        if self.eat(Kind::Pow) {
            let value = self.parse_expression()?;
            Ok((None, value))
        } else {
            let key = self.parse_expression()?;
            self.expect(Kind::Colon)?;
            let value = self.parse_expression()?;
            Ok((Some(key), value))
        }
    }

    fn consume_whitespace_and_newline(&mut self) -> bool {
        let mut consumed = false;
        while matches!(self.cur_kind(), Kind::WhiteSpace | Kind::NewLine | Kind::NL) {
            self.advance();
            consumed = true;
        }
        consumed
    }

    fn consume_whitespace_and_comments(&mut self) -> bool {
        let mut consumed = false;
        while matches!(
            self.cur_kind(),
            Kind::WhiteSpace | Kind::NewLine | Kind::Comment | Kind::NL
        ) {
            self.bump(self.cur_kind());
            consumed = true;
        }
        consumed
    }

    // https://docs.python.org/3/reference/expressions.html#expression-lists
    // termination_kind is used to know when to stop parsing the list
    // for example to parse a tuple the termination_kind is Kind::RightParen
    // caller is responsible to consume the first & last occurrence of the
    // termination_kind
    fn parse_starred_list(
        &mut self,
        termination_kind: Kind,
    ) -> Result<Vec<Expression>, ParsingError> {
        let mut expressions = vec![];
        while !self.at(Kind::Eof) && !self.at(termination_kind) {
            if self.eat(Kind::Comment) || self.consume_whitespace_and_newline() {
                continue;
            }
            let expr = self.parse_starred_item()?;
            if !self.at(Kind::Eof) && !self.at(termination_kind) {
                self.expect(Kind::Comma)?;
            }
            expressions.push(expr);
        }
        Ok(expressions)
    }

    // https://docs.python.org/3/reference/expressions.html#expression-lists
    fn parse_starred_item(&mut self) -> Result<Expression, ParsingError> {
        let mut node = self.start_node();
        if self.eat(Kind::Mul) {
            let starred_value_kind = self.cur_kind();
            let expr = self.parse_or_expr()?;
            node = self.finish_node(node);
            if !is_iterable(&expr) {
                // self.unepxted_token(node, starred_value_kind);
                panic!();
            }
            return Ok(Expression::Starred(Box::new(Starred {
                node: self.finish_node(node),
                value: expr,
            })));
        }
        self.parse_named_expression()
    }

    // https://docs.python.org/3/reference/expressions.html#conditional-expressions
    fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        // This is a hack to make this function parse  () as tuple
        if self.at(Kind::LeftParen) && matches!(self.peek_kind(), Ok(Kind::RightParen)) {
            self.bump_any();
            self.bump_any();
            return Ok(Expression::Tuple(Box::new(Tuple {
                node: self.finish_node(node),
                elements: vec![],
            })));
        }
        if self.eat(Kind::Lambda) {
            let params_list = self.parse_parameters(true).expect("lambda params");
            self.expect(Kind::Colon)?;
            let expr = self.parse_expression()?;

            return Ok(Expression::Lambda(Box::new(Lambda {
                node: self.finish_node(node),
                args: params_list,
                body: expr,
            })));
        }
        self.parse_conditional_expression()
    }

    // https://docs.python.org/3/reference/expressions.html#boolean-operations
    fn parse_or_test(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut expr = self.parse_and_test()?;
        if self.eat(Kind::Or) {
            let rhs = self.parse_or_test()?;
            expr = Expression::BoolOp(Box::new(BoolOperation {
                node: self.finish_node(node),
                op: BooleanOperator::Or,
                values: vec![expr, rhs],
            }));
        }
        Ok(expr)
    }

    // https://docs.python.org/3/reference/expressions.html#boolean-operations
    fn parse_and_test(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut expr = self.parse_not_test()?;
        while self.at(Kind::And) {
            self.bump_any();
            let rhs = self.parse_not_test()?;
            expr = Expression::BoolOp(Box::new(BoolOperation {
                node: self.finish_node(node),
                op: BooleanOperator::And,
                values: vec![expr, rhs],
            }));
        }
        Ok(expr)
    }

    // https://docs.python.org/3/reference/expressions.html#boolean-operations
    fn parse_not_test(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        if self.at(Kind::Not) {
            self.bump(Kind::Not);
            let operand = self.parse_not_test()?;
            return Ok(Expression::UnaryOp(Box::new(UnaryOperation {
                node: self.finish_node(node),
                op: UnaryOperator::Not,
                operand,
            })));
        }
        self.parse_comparison()
    }

    // https://docs.python.org/3/reference/expressions.html#comparisons
    fn parse_comparison(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let or_expr = self.parse_or_expr()?;
        let mut ops = vec![];
        let mut comparators = vec![];
        while self.cur_kind().is_comparison_operator() {
            ops.push(self.parse_comp_operator()?);
            comparators.push(self.parse_or_expr()?);
        }

        if !ops.is_empty() {
            return Ok(Expression::Compare(Box::new(Compare {
                node: self.finish_node(node),
                left: or_expr,
                ops,
                comparators,
            })));
        }

        Ok(or_expr)
    }

    // Binary bitwise operations
    // https://docs.python.org/3/reference/expressions.html#binary-bitwise-operations
    fn parse_or_expr(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut lhs = self.parse_xor_expr()?;
        while self.eat(Kind::BitOr) {
            let rhs = self.parse_xor_expr()?;
            lhs = Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op: BinaryOperator::BitOr,
                left: lhs,
                right: rhs,
            }));
        }
        Ok(lhs)
    }

    // https://docs.python.org/3/reference/expressions.html#binary-bitwise-operations
    fn parse_xor_expr(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut and_expr = self.parse_and_expr()?;
        if self.eat(Kind::BitXor) {
            let rhs = self.parse_and_expr()?;
            and_expr = Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op: BinaryOperator::BitXor,
                left: and_expr,
                right: rhs,
            }));
        }
        Ok(and_expr)
    }

    // https://docs.python.org/3/reference/expressions.html#binary-bitwise-operations
    fn parse_and_expr(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut shift_expr = self.parse_shift_expr()?;

        while self.eat(Kind::BitAnd) {
            let rhs = self.parse_shift_expr()?;
            shift_expr = Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op: BinaryOperator::BitAnd,
                left: shift_expr,
                right: rhs,
            }));
        }
        Ok(shift_expr)
    }

    // https://docs.python.org/3/reference/expressions.html#shifting-operations
    fn parse_shift_expr(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut arith_expr = self.parse_binary_arithmetic_operation()?;
        if self.at(Kind::LeftShift) || self.at(Kind::RightShift) {
            let op = if self.eat(Kind::LeftShift) {
                BinaryOperator::LShift
            } else {
                self.bump(Kind::RightShift);
                BinaryOperator::RShift
            };
            let lhs = self.parse_binary_arithmetic_operation()?;
            arith_expr = Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op,
                left: arith_expr,
                right: lhs,
            }));
        }
        Ok(arith_expr)
    }

    // https://docs.python.org/3/reference/expressions.html#binary-arithmetic-operations
    fn parse_binary_arithmetic_operation(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut lhs = self.parse_unary_arithmetic_operation()?;
        while self.cur_kind().is_bin_arithmetic_op() {
            let op = self.parse_bin_arithmetic_op()?;
            let rhs = self.parse_unary_arithmetic_operation()?;
            lhs = Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op,
                left: lhs,
                right: rhs,
            }));
        }
        Ok(lhs)
    }

    // https://docs.python.org/3/reference/expressions.html#unary-arithmetic-and-bitwise-operations
    fn parse_unary_arithmetic_operation(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        if self.cur_kind().is_unary_op() {
            let op = map_unary_operator(&self.cur_kind());
            self.bump_any();
            let operand = self.parse_unary_arithmetic_operation()?;
            return Ok(Expression::UnaryOp(Box::new(UnaryOperation {
                node: self.finish_node(node),
                op,
                operand,
            })));
        }
        self.parse_power_expression()
    }

    // https://docs.python.org/3/reference/expressions.html#the-power-operator
    fn parse_power_expression(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let base = if self.at(Kind::Await) {
            self.bump(Kind::Await);
            let value = self.parse_primary(None)?;
            Ok(Expression::Await(Box::new(Await {
                node: self.finish_node(node),
                value,
            })))
        } else {
            self.parse_primary(None)
        };
        if self.eat(Kind::Pow) {
            let exponent = self.parse_unary_arithmetic_operation()?;
            return Ok(Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op: BinaryOperator::Pow,
                left: base?,
                right: exponent,
            })));
        }

        base
    }

    // https://docs.python.org/3/reference/expressions.html#primaries
    // primaries can be chained together, when they are chained the base is the
    // previous primary
    fn parse_primary(&mut self, base: Option<Expression>) -> Result<Expression, ParsingError> {
        let next = self.peek_token();
        let node = self.start_node();
        let call_node = self.start_node();
        let mut atom_or_primary = if let Some(base) = base {
            base
        } else if self.cur_kind().is_atom() {
            self.parse_atom()?
        } else {
            let path = &self.path;
            panic!("not a primary {} {path:?}", self.cur_token());
        };

        let mut primary = if self.at(Kind::Dot) {
            self.parse_attribute_ref(node, atom_or_primary)
        } else if self.at(Kind::LeftBrace) {
            // https://docs.python.org/3/reference/expressions.html#slicings
            self.parse_subscript(node, atom_or_primary)
        } else if self.eat(Kind::LeftParen) {
            // parse call
            // https://docs.python.org/3/reference/expressions.html#calls
            let mut positional_args = vec![];
            let mut keyword_args = vec![];
            let mut seen_keyword = false;

            loop {
                if self.at(Kind::RightParen) {
                    break;
                }
                if self.at(Kind::Identifier) && matches!(self.peek_kind(), Ok(Kind::Assign)) {
                    seen_keyword = true;
                    let keyword_arg = self.parse_keyword_item()?;
                    keyword_args.push(keyword_arg);
                } else if self.at(Kind::Mul) {
                    let star_arg_node = self.start_node();
                    self.bump(Kind::Mul);
                    let expr = self.parse_expression()?;
                    let star_arg = Expression::Starred(Box::new(Starred {
                        node: self.finish_node(star_arg_node),
                        value: expr,
                    }));
                    positional_args.push(star_arg);
                } else if self.at(Kind::Pow) {
                    let kwarg_node = self.start_node();
                    self.bump(Kind::Pow);
                    seen_keyword = true;
                    let expr = self.parse_expression()?;
                    let kwarg = Keyword {
                        node: self.finish_node(kwarg_node),
                        arg: None,
                        value: expr,
                    };
                    keyword_args.push(kwarg);
                } else {
                    if seen_keyword {
                        let node_end = self.finish_node(node);
                        panic!("Positional arguments cannot come after keyword arguments.");
                    }
                    let arg = self.parse_named_expression()?;
                    positional_args.push(arg);
                }
                if !self.eat(Kind::Comma) {
                    break;
                }
            }

            self.bump(Kind::Comma);
            if self.at(Kind::Async) || self.at(Kind::For) {
                let comprehension = self.parse_comp_for()?;
                let arg = Expression::Generator(Box::new(Generator {
                    node: self.finish_node(node),
                    element: positional_args
                        .into_iter()
                        .next()
                        .expect("generator passed to function does not have an element"),
                    generators: comprehension,
                }));
                self.expect(Kind::RightParen)?;

                Ok(Expression::Call(Box::new(Call {
                    node,
                    func: atom_or_primary,
                    args: vec![arg],
                    keywords: vec![],
                    starargs: None,
                    kwargs: None,
                })))
            } else {
                self.expect(Kind::RightParen)?;

                Ok(Expression::Call(Box::new(Call {
                    node: Node {
                        start: atom_or_primary.get_node().start,
                        end: self.finish_node(call_node).end,
                    },
                    func: atom_or_primary,
                    args: positional_args,
                    keywords: keyword_args,
                    starargs: None,
                    kwargs: None,
                })))
            }
        } else {
            Ok(atom_or_primary)
        };

        if matches!(
            self.cur_kind(),
            Kind::LeftBrace | Kind::LeftParen | Kind::Dot
        ) {
            primary = self.parse_primary(Some(primary?));
        }

        primary
    }

    // https://docs.python.org/3/reference/expressions.html#grammar-token-python-grammar-argument_list
    // returns args, keywords
    fn parse_argument_list(&mut self) -> Result<(Vec<Expression>, Vec<Keyword>), ParsingError> {
        let mut seen_keyword = false;
        let mut positional_args = vec![];
        let mut keyword_args = vec![];
        loop {
            let node = self.start_node();
            if self.at(Kind::RightParen) {
                break;
            }
            if self.at(Kind::Identifier) && matches!(self.peek_kind(), Ok(Kind::Assign)) {
                seen_keyword = true;
                let keyword_arg = self.parse_keyword_item()?;
                keyword_args.push(keyword_arg);
            } else if self.at(Kind::Mul) {
                let star_arg_node = self.start_node();
                self.bump(Kind::Mul);
                let star_arg = Expression::Starred(Box::new(Starred {
                    node: self.finish_node(star_arg_node),
                    value: self.parse_expression()?,
                }));
                positional_args.push(star_arg);
            } else if self.at(Kind::Pow) {
                let kwarg_node = self.start_node();
                self.bump(Kind::Pow);
                seen_keyword = true;
                let kwarg = Keyword {
                    node: self.finish_node(kwarg_node),
                    arg: None,
                    value: self.parse_expression()?,
                };
                keyword_args.push(kwarg);
            } else {
                if seen_keyword {
                    let node_end = self.finish_node(node);
                    panic!("Positional arguments cannot come after keyword arguments.");
                }
                let arg = self.parse_named_expression()?;
                positional_args.push(arg);
            }
            if !self.eat(Kind::Comma) {
                break;
            }
        }
        Ok((positional_args, keyword_args))
    }

    fn parse_attribute_ref(
        &mut self,
        node: Node,
        value: Expression,
    ) -> Result<Expression, ParsingError> {
        let mut expr = Ok(value);
        while self.eat(Kind::Dot) {
            let attr_val = self.cur_token().value.to_string();
            self.expect(Kind::Identifier)?;
            expr = Ok(Expression::Attribute(Box::new(Attribute {
                node: self.finish_node(node),
                value: expr?,
                attr: attr_val,
            })));
        }
        expr
    }

    fn parse_subscript(
        &mut self,
        node: Node,
        value: Expression,
    ) -> Result<Expression, ParsingError> {
        let mut expr = Ok(value);
        // To handle cases like a[1][2]...
        while self.eat(Kind::LeftBrace) {
            let slice = self.parse_slices()?;
            expr = Ok(Expression::Subscript(Box::new(Subscript {
                node: self.finish_node(node),
                value: expr?,
                slice,
            })));
        }
        expr
    }

    // https://docs.python.org/3/reference/expressions.html#atoms
    fn parse_atom(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        if self.at(Kind::Yield) {
            self.parse_yield_expression()
        } else if self.at(Kind::LeftBrace) {
            let list_expr = self.parse_list();
            return list_expr;
        } else if self.at(Kind::LeftBracket) {
            let dict_or_set_expr = self.parse_dict_or_set();
            return dict_or_set_expr;
        } else if self.at(Kind::LeftParen) {
            let tuple_or_named_expr = self.parse_paren_form_or_generator();
            return tuple_or_named_expr;
        } else if self.at(Kind::Identifier) {
            return self.parse_identifier();
        // Try to map to one of atoms like: number, string, ...
        } else {
            // value must be cloned to be assigned to the node
            let atom_is_string = self.cur_kind().is_string();
            let start = self.start_node();
            let mut expr = match self.cur_kind() {
                Kind::Identifier => {
                    let val = self.cur_token.value.take_string();
                    self.bump_any();
                    Expression::Name(Box::new(Name {
                        node: self.finish_node(start),
                        id: val,
                        parenthesized: false,
                    }))
                }
                Kind::Integer => {
                    let val = self.cur_token.value.take_string();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Int(val),
                    }))
                }
                Kind::None => {
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::None,
                    }))
                }
                Kind::True => {
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Bool(true),
                    }))
                }
                Kind::False => {
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Bool(false),
                    }))
                }
                Kind::ImaginaryInteger => {
                    let val = self.cur_token.value.take_string();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Complex {
                            real: "0".to_string(),
                            imaginary: val,
                        },
                    }))
                }
                Kind::Bytes => {
                    let val = self.cur_token.value.take_string();
                    let bytes_val = extract_string_inside(
                        val.to_string()
                            .strip_prefix('b')
                            .expect("bytes literal must start with b")
                            .to_string(),
                    )
                    .into_bytes();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Bytes(bytes_val),
                    }))
                }
                Kind::StringLiteral => {
                    let val = self.cur_token.value.take_string();
                    let string_val = extract_string_inside(val.to_string());
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Str(string_val),
                    }))
                }

                Kind::RawBytes => {
                    let val = self.cur_token.value.take_string();
                    // rb or br appear in the beginning of raw bytes
                    let bytes_val =
                        extract_string_inside(val.to_string().chars().skip(2).collect::<String>())
                            .into_bytes();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Bytes(bytes_val),
                    }))
                }
                Kind::FStringStart => {
                    self.bump_any();
                    let fstring = self.parse_fstring(start)?;
                    Expression::JoinedStr(Box::new(JoinedStr {
                        node: self.finish_node(start),
                        values: fstring,
                    }))
                }
                Kind::PointFloat => {
                    let val = self.cur_token.value.take_string();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Float(val),
                    }))
                }
                Kind::ExponentFloat => {
                    let val = self.cur_token.value.take_string();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Float(val),
                    }))
                }
                Kind::ImaginaryPointFloat => {
                    let val = self.cur_token.value.take_string();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Complex {
                            real: "0".to_string(),
                            imaginary: val,
                        },
                    }))
                }
                Kind::ImaginaryExponentFloat => {
                    let val = self.cur_token.value.take_string();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Complex {
                            real: "0".to_string(),
                            imaginary: val,
                        },
                    }))
                }
                Kind::Ellipsis => {
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Ellipsis,
                    }))
                }
                // TODO: is there something for octal and Hexadecimal?
                Kind::Octal => {
                    let val = self.cur_token.value.take_string();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Int(val),
                    }))
                }
                Kind::Hexadecimal => {
                    let val = self.cur_token.value.take_string();
                    self.bump_any();
                    Expression::Constant(Box::new(Constant {
                        node: self.finish_node(start),
                        value: ConstantValue::Int(val),
                    }))
                }
                _ => {
                    let line = self.get_offset_line_number(start.start);
                    let kind = self.cur_kind();
                    let value = self.cur_token().value.to_string();
                    panic!("token {kind:?} {value:?} {start:?} is not atom");
                }
            };

            // If the current token is a string, we need to check if there are more strings
            // and concat them
            // https://docs.python.org/3/reference/lexical_analysis.html#string-literal-concatenation
            if atom_is_string {
                loop {
                    if self.cur_kind().is_string() {
                        // TODO: duplicate code
                        let start = self.start_node();
                        let next_str = match self.cur_kind() {
                            Kind::Identifier => {
                                let val = self.cur_token().value.to_string();
                                self.bump_any();
                                Expression::Name(Box::new(Name {
                                    node: self.finish_node(start),
                                    id: val,
                                    parenthesized: false,
                                }))
                            }
                            Kind::Integer => {
                                let val = self.cur_token().value.to_string();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Int(val),
                                }))
                            }
                            Kind::None => {
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::None,
                                }))
                            }
                            Kind::True => {
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Bool(true),
                                }))
                            }
                            Kind::False => {
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Bool(false),
                                }))
                            }
                            Kind::ImaginaryInteger => {
                                let val = self.cur_token().value.to_string();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Complex {
                                        real: "0".to_string(),
                                        imaginary: val,
                                    },
                                }))
                            }
                            Kind::Bytes => {
                                let val = &self.cur_token().value;
                                let bytes_val = extract_string_inside(
                                    val.to_string()
                                        .strip_prefix('b')
                                        .expect("bytes literal must start with b")
                                        .to_string(),
                                )
                                .into_bytes();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Bytes(bytes_val),
                                }))
                            }
                            Kind::StringLiteral => {
                                let val = &self.cur_token().value;
                                let string_val = extract_string_inside(val.to_string());
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Str(string_val),
                                }))
                            }

                            Kind::RawBytes => {
                                let val = &self.cur_token().value;
                                // rb or br appear in the beginning of raw bytes
                                let bytes_val = extract_string_inside(
                                    val.to_string().chars().skip(2).collect::<String>(),
                                )
                                .into_bytes();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Bytes(bytes_val),
                                }))
                            }
                            Kind::FStringStart => {
                                self.bump_any();
                                let fstring = self.parse_fstring(start)?;
                                Expression::JoinedStr(Box::new(JoinedStr {
                                    node: self.finish_node(start),
                                    values: fstring,
                                }))
                            }
                            Kind::PointFloat => {
                                let val = self.cur_token().value.to_string();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Float(val),
                                }))
                            }
                            Kind::ExponentFloat => {
                                let val = self.cur_token().value.to_string();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Float(val),
                                }))
                            }
                            Kind::ImaginaryPointFloat => {
                                let val = self.cur_token().value.to_string();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Complex {
                                        real: "0".to_string(),
                                        imaginary: val,
                                    },
                                }))
                            }
                            Kind::ImaginaryExponentFloat => {
                                let val = self.cur_token().value.to_string();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Complex {
                                        real: "0".to_string(),
                                        imaginary: val,
                                    },
                                }))
                            }
                            Kind::Ellipsis => {
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Ellipsis,
                                }))
                            }
                            // TODO: is there something for octal and Hexadecimal?
                            Kind::Octal => {
                                let val = self.cur_token().value.to_string();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Int(val),
                                }))
                            }
                            Kind::Hexadecimal => {
                                let val = self.cur_token().value.to_string();
                                self.bump_any();
                                Expression::Constant(Box::new(Constant {
                                    node: self.finish_node(start),
                                    value: ConstantValue::Int(val),
                                }))
                            }
                            _ => {
                                let line = self.get_offset_line_number(start.start);
                                let kind = self.cur_kind();
                                let value = self.cur_token().value.to_string();
                                panic!("token {kind:?} {value:?} {start:?} is not atom");
                            }
                        };

                        expr = concat_string_exprs(expr, next_str)?;
                    } else if self.eat(Kind::WhiteSpace) {
                        continue;
                    } else if self.at(Kind::Indent)
                        || self.at(Kind::NewLine)
                        || self.at(Kind::Dedent)
                    {
                        // Normally the strings in two lines are not concatenated
                        // but if they are inside a [], {}, (), they are
                        // here we consume all the indent and newline in this case
                        if self.nested_expression_list > 0 {
                            self.bump_any();
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }

            return Ok(expr);
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let value = self.cur_token().value.to_string();
        self.expect(Kind::Identifier)?;
        Ok(Expression::Name(Box::new(Name {
            node: self.finish_node(node),
            id: value,
            parenthesized: false,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#yield-expressions
    fn parse_yield_expression(&mut self) -> Result<Expression, ParsingError> {
        let yield_node = self.start_node();
        self.expect(Kind::Yield)?;

        if self.eat(Kind::From) {
            let value = self.parse_expression()?;
            return Ok(Expression::YieldFrom(Box::new(YieldFrom {
                node: self.finish_node(yield_node),
                value,
            })));
        }
        if self.at(Kind::NewLine) || self.at(Kind::Eof) {
            return Ok(Expression::Yield(Box::new(Yield {
                node: self.finish_node(yield_node),
                value: None,
            })));
        }
        let value = match self.parse_expression_list() {
            Ok(expr) => Some(expr),
            _ => None,
        };
        Ok(Expression::Yield(Box::new(Yield {
            node: self.finish_node(yield_node),
            value,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#expression-lists
    fn parse_expression_list(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let mut expressions = vec![];
        expressions.push(self.parse_expression()?);
        while self.eat(Kind::Comma) && !self.at(Kind::Eof) {
            if !self.cur_kind().is_star_expression() {
                break;
            }
            let expr = self.parse_expression()?;
            expressions.push(expr);
        }
        if expressions.len() == 1 {
            return Ok(expressions.pop().unwrap());
        }
        Ok(Expression::Tuple(Box::new(Tuple {
            node: self.finish_node(node),
            elements: expressions,
        })))
    }

    fn parse_starred_expression_real(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        self.expect(Kind::Mul);
        let expr = self.parse_expression()?;
        Ok(Expression::Starred(Box::new(Starred {
            node: self.finish_node(node),
            value: expr,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#expression-lists
    fn parse_starred_expression(
        &mut self,
        node: Node,
        first_elm: Expression,
    ) -> Result<Expression, ParsingError> {
        let mut elements = vec![];
        // if tuple has one element but there's a comma after
        // it, it's a tuple
        let mut seen_comma = false;
        elements.push(first_elm);
        while !self.at(Kind::Eof) && !self.at(Kind::RightParen) {
            self.expect(Kind::Comma)?;
            if self.at(Kind::RightParen) {
                break;
            }
            let expr = self.parse_starred_item()?;
            elements.push(expr);
            seen_comma = true;
        }
        if elements.len() == 1 && !seen_comma {
            let expr = elements.pop().unwrap();
            if let Expression::Name(name) = expr {
                return Ok(Expression::Name(Box::new(Name {
                    node: name.node,
                    id: name.id,
                    parenthesized: true,
                })));
            } else {
                return Ok(expr);
            }
        }
        self.expect(Kind::RightParen)?;
        Ok(Expression::Tuple(Box::new(Tuple {
            node: self.finish_node(node),
            elements,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#slicings
    // Closing will be consumed by this function
    fn parse_slices(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();

        let mut elements: Vec<Expression> = vec![];
        while !self.at(Kind::RightBrace) {
            if self.at(Kind::Mul) {
                elements.push(self.parse_starred_expression_real()?);
                continue;
            }
            elements.push(self.parse_slice()?);
            if !self.eat(Kind::Comma) {
                break;
            }
        }
        self.expect(Kind::RightBrace)?;
        if elements.len() == 1 {
            return Ok(elements.pop().unwrap());
        }
        Ok(Expression::Tuple(Box::new(Tuple {
            node: self.finish_node(node),
            elements,
        })))
    }

    fn parse_slice(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        let named_expression_or_expression = if self.eat(Kind::Colon) {
            None
        } else {
            Some(self.parse_named_expression()?)
        };

        match named_expression_or_expression {
            // [expr
            Some(n) => {
                // [expr ,|]
                if self.at(Kind::RightBrace) || self.at(Kind::Comma) {
                    Ok(n)
                // [expr:
                } else {
                    self.expect(Kind::Colon);
                    // [expr::
                    if self.eat(Kind::Colon) {
                        // [expr:: end
                        if self.at(Kind::RightBrace) || self.at(Kind::Comma) {
                            Ok(Expression::Slice(Box::new(Slice {
                                node: self.finish_node(node),
                                lower: Some(n),
                                upper: None,
                                step: None,
                            })))
                            // [expr::expr end
                        } else {
                            let step = Some(self.parse_expression()?);
                            Ok(Expression::Slice(Box::new(Slice {
                                node: self.finish_node(node),
                                lower: Some(n),
                                upper: None,
                                step,
                            })))
                        }

                    // [expr: ,|] end
                    } else if self.at(Kind::RightBrace) || self.at(Kind::Comma) {
                        return Ok(Expression::Slice(Box::new(Slice {
                            node: self.finish_node(node),
                            lower: Some(n),
                            upper: None,
                            step: None,
                        })));
                    // [expr:expr
                    } else {
                        let upper_or_step = Some(self.parse_expression()?);
                        // [expr:expr:
                        if self.eat(Kind::Colon) {
                            // [expr:expr:] end
                            if self.at(Kind::RightBrace) || self.at(Kind::Comma) {
                                return Ok(Expression::Slice(Box::new(Slice {
                                    node: self.finish_node(node),
                                    lower: Some(n),
                                    upper: None,
                                    step: upper_or_step,
                                })));
                                // [expr:expr:expr] end
                            } else {
                                let step = Some(self.parse_expression()?);
                                return Ok(Expression::Slice(Box::new(Slice {
                                    node: self.finish_node(node),
                                    lower: Some(n),
                                    upper: upper_or_step,
                                    step,
                                })));
                            }
                        // [expr:expr
                        } else {
                            return Ok(Expression::Slice(Box::new(Slice {
                                node: self.finish_node(node),
                                lower: Some(n),
                                upper: upper_or_step,
                                step: None,
                            })));
                        }
                    }
                }
            }
            // [:
            None => {
                // [: ,|]
                if self.at(Kind::RightBrace) || self.at(Kind::Comma) {
                    Ok(Expression::Slice(Box::new(Slice {
                        node: self.finish_node(node),
                        lower: None,
                        upper: None,
                        step: None,
                    })))
                // [::
                } else if self.eat(Kind::Colon) {
                    // [::] || [::,
                    if self.at(Kind::RightBrace) || self.at(Kind::Comma) {
                        return Ok(Expression::Slice(Box::new(Slice {
                            node: self.finish_node(node),
                            lower: None,
                            upper: None,
                            step: None,
                        })));
                    // [::expr
                    } else {
                        let upper = Some(self.parse_expression()?);
                        return Ok(Expression::Slice(Box::new(Slice {
                            node: self.finish_node(node),
                            lower: None,
                            upper,
                            step: None,
                        })));
                    }
                // [:expr
                } else {
                    let first = Some(self.parse_expression()?);
                    // [:expr:
                    if self.eat(Kind::Colon) {
                        // [:expr:] end
                        if self.at(Kind::RightBrace) || self.at(Kind::Comma) {
                            return Ok(Expression::Slice(Box::new(Slice {
                                node: self.finish_node(node),
                                lower: None,
                                upper: first,
                                step: None,
                            })));
                        }
                        // [:expr:expr
                        let step = Some(self.parse_expression()?);
                        return Ok(Expression::Slice(Box::new(Slice {
                            node: self.finish_node(node),
                            lower: None,
                            upper: first,
                            step,
                        })));
                    }
                    return Ok(Expression::Slice(Box::new(Slice {
                        node: self.finish_node(node),
                        lower: None,
                        upper: None,
                        step: first,
                    })));
                }
            }
        }
    }

    // fn parse_constant(
    //     &mut self,
    //     start: Node,
    //     kind: Kind,
    //     value: &TokenValue,
    // ) -> Result<Expression, ParsingError> {
    //     };
    //     Ok(atom)
    // }

    fn parse_comp_operator(&mut self) -> Result<ComparisonOperator, ParsingError> {
        let node = self.start_node();
        let op = match self.cur_kind() {
            Kind::Less => ComparisonOperator::Lt,
            Kind::Greater => ComparisonOperator::Gt,
            Kind::LessEq => ComparisonOperator::LtE,
            Kind::GreaterEq => ComparisonOperator::GtE,
            Kind::Eq => ComparisonOperator::Eq,
            Kind::NotEq => ComparisonOperator::NotEq,
            Kind::In => ComparisonOperator::In,
            Kind::Is => match self.peek_kind() {
                Ok(Kind::Not) => {
                    self.bump_any();
                    ComparisonOperator::IsNot
                }
                _ => ComparisonOperator::Is,
            },
            Kind::Not => match self.peek_kind() {
                Ok(Kind::In) => {
                    self.bump_any();
                    ComparisonOperator::NotIn
                }
                _ => {
                    // return Err(self.unepxted_token(node, self.cur_kind()).err().unwrap());
                    panic!();
                }
            },
            _ => {
                // return Err(self.unepxted_token(node, self.cur_kind()).err().unwrap());
                panic!();
            }
        };
        self.bump_any();
        Ok(op)
    }

    fn parse_bin_arithmetic_op(&mut self) -> Result<BinaryOperator, ParsingError> {
        let op = match self.cur_kind() {
            Kind::Plus => Ok(BinaryOperator::Add),
            Kind::Minus => Ok(BinaryOperator::Sub),
            Kind::Mul => Ok(BinaryOperator::Mult),
            Kind::Div => Ok(BinaryOperator::Div),
            Kind::IntDiv => Ok(BinaryOperator::FloorDiv),
            Kind::Mod => Ok(BinaryOperator::Mod),
            Kind::Pow => Ok(BinaryOperator::Pow),
            Kind::MatrixMul => Ok(BinaryOperator::MatMult),
            _ => {
                // Err(self
                // .unepxted_token(self.start_node(), self.cur_kind())
                // .err()
                // .unwrap()),
                panic!();
            }
        };
        self.bump_any();
        op
    }

    fn parse_keyword_item(&mut self) -> Result<Keyword, ParsingError> {
        let node = self.start_node();
        let arg = self.cur_token().value.to_string();
        self.expect(Kind::Identifier);
        self.expect(Kind::Assign);
        let value = self.parse_expression()?;
        Ok(Keyword {
            node: self.finish_node(node),
            arg: Some(arg),
            value,
        })
    }

    fn parse_parameters(&mut self, is_lambda: bool) -> Result<Arguments, ParsingError> {
        let node = self.start_node();
        let mut seen_vararg = false;
        let mut seen_kwarg = false;
        let mut must_have_default = false;

        let mut posonlyargs = vec![];
        let mut args = vec![];
        let mut vararg = None;
        let mut kwarg = None;
        let mut kwonlyargs = vec![];
        let mut kw_defaults = vec![];
        let mut defaults = vec![];

        loop {
            if self.is_def_parameter() {
                let (param, default) = self.parse_parameter(is_lambda)?;
                if seen_vararg {
                    kwonlyargs.push(param);
                } else if seen_kwarg {
                    panic!("positional argument follows keyword argument");
                } else {
                    args.push(param);
                }

                // TODO: refactor this
                if seen_vararg {
                    kw_defaults.push(default);
                } else if let Some(default_value) = default {
                    must_have_default = true;
                    defaults.push(default_value);
                } else if must_have_default {
                    panic!("non-default argument follows default argument");
                }
            // If a parameter has a default value, all following parameters up
            // until the “*” must also have a default value — this
            // is a syntactic restriction that is not expressed by the grammar.
            } else if self.eat(Kind::Mul) {
                seen_vararg = true;
                // after seeing vararg the must_have_default is reset
                // until we see a default value again
                must_have_default = false;
                // In this case this is not a vararg but only marks the end of positional
                // arguments e.g. def foo(a, b, *, c, d)
                if self.eat(Kind::Comma) {
                    continue;
                }
                let (param, default) = self.parse_parameter(is_lambda)?;
                // default is not allowed for vararg
                if default.is_some() {
                    panic!("var-positional argument cannot have default value");
                }
                vararg = Some(param);
            } else if self.eat(Kind::Pow) {
                seen_kwarg = true;
                let (param, default) = self.parse_parameter(is_lambda)?;
                // default is not allowed for kwarg
                if default.is_some() {
                    panic!("var-keyword argument cannot have default value");
                }
                kwarg = Some(param);
            } else if self.eat(Kind::Comma) {
                continue;
            } else if self.eat(Kind::Div) {
                // copy the current args to posonlyargs
                posonlyargs = args;
                args = vec![];
            } else {
                break;
            }
        }
        // return Parameter

        Ok(Arguments {
            node: self.finish_node(node),
            posonlyargs,
            args,
            vararg,
            kwonlyargs,
            kw_defaults,
            kwarg,
            defaults,
        })
    }

    fn is_def_parameter(&mut self) -> bool {
        self.cur_kind() == Kind::Identifier
        // && matches!(self.peek_kind(), Ok(Kind::Assign))
        // || matches!(self.peek_kind(), Ok(Kind::Colon))
    }

    fn parse_parameter(
        &mut self,
        is_lambda: bool,
    ) -> Result<(Arg, Option<Expression>), ParsingError> {
        let node = self.start_node();
        let arg = self.cur_token().value.to_string();
        self.bump(Kind::Identifier);
        // Lambda parameters cannot have annotations
        let annotation = if self.at(Kind::Colon) && !is_lambda {
            self.bump(Kind::Colon);
            Some(self.parse_expression()?)
        } else {
            None
        };
        let arg_node = self.finish_node(node);
        let default = if self.eat(Kind::Assign) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok((
            Arg {
                node: arg_node,
                arg,
                annotation,
            },
            default,
        ))
    }

    // the FStringStart token is consumed by the caller
    fn parse_fstring(&mut self, node: Node) -> Result<Vec<Expression>, ParsingError> {
        let mut expressions = vec![];
        while self.cur_kind() != Kind::FStringEnd {
            expressions.push(self.parse_fstring_middle()?)
        }
        self.bump(Kind::FStringEnd);
        Ok(expressions)
    }

    // This function is just here to make it easier to refactor the spans.
    // I don't know how the spans and line number should be handled here
    pub(crate) fn get_span_on_line(&self, start: u32, end: u32) -> (usize, usize) {
        (start as usize, end as usize)
    }

    // https://docs.python.org/3/reference/compound_stmts.html#type-parameter-lists
    fn parse_type_parameters(&mut self) -> Result<Vec<TypeParam>, ParsingError> {
        let node = self.start_node();
        self.expect(Kind::LeftBrace)?;
        let mut type_params = vec![];
        while !self.eat(Kind::RightBrace) {
            match self.cur_kind() {
                Kind::Identifier => {
                    let node = self.start_node();
                    let name = self.cur_token().value.to_string();
                    self.bump(Kind::Identifier);
                    let bound = if self.eat(Kind::Colon) {
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };
                    type_params.push(TypeParam::TypeVar(TypeVar {
                        node: self.finish_node(node),
                        name,
                        bound,
                    }));
                }
                Kind::Pow => {
                    // param spec
                    let node = self.start_node();
                    self.bump(Kind::Pow);
                    let name = self.cur_token().value.to_string();
                    self.bump(Kind::Identifier);
                    type_params.push(TypeParam::ParamSpec(ParamSpec {
                        node: self.finish_node(node),
                        name,
                    }));
                }
                Kind::Mul => {
                    // type var tuple
                    let node = self.start_node();
                    self.bump(Kind::Mul);
                    let name = self.cur_token().value.to_string();
                    self.bump(Kind::Identifier);
                    type_params.push(TypeParam::TypeVarTuple(TypeVarTuple {
                        node: self.finish_node(node),
                        name,
                    }));
                }
                _ => {
                    return Err(self.unexpected_token_new(
                        node,
                        vec![Kind::Identifier, Kind::Pow, Kind::Mul, Kind::RightBracket],
                        "",
                    ));
                }
            }
            if !self.at(Kind::RightBrace) {
                self.expect(Kind::Comma)?;
            }
        }
        if type_params.is_empty() {
            return Err(self.unexpected_token_new(
                node,
                vec![Kind::Identifier, Kind::Pow, Kind::Mul],
                "Type parameter is empty",
            ));
        }
        Ok(type_params)
    }

    fn parse_type_alias_statement(&mut self) -> std::result::Result<Statement, ParsingError> {
        let node = self.start_node();
        self.expect(Kind::Identifier)?;
        let name = self.cur_token().value.to_string();
        self.expect(Kind::Identifier)?;
        let type_params = if self.eat(Kind::LeftBrace) {
            let type_params = self.parse_type_parameters()?;
            self.expect(Kind::RightBrace)?;
            type_params
        } else {
            vec![]
        };
        self.expect(Kind::Assign)?;
        let value = self.parse_expression()?;
        Ok(Statement::TypeAlias(Box::new(TypeAlias {
            node: self.finish_node(node),
            name,
            type_params,
            value,
        })))
    }

    fn parse_fstring_replacement_field(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        self.bump(Kind::LeftBracket);

        let expr = if self.at(Kind::Yield) {
            self.parse_yield_expression()?
        } else {
            self.parse_expressions()?
        };

        let mut conversion = -1;
        if self.eat(Kind::Assign) {
            conversion = 114;
        }
        if self.at(Kind::Identifier) {
            conversion = match self.cur_token.value.take_string().as_str() {
                "!s" => 115,
                "!r" => 114,
                "!a" => 97,
                _ => panic!("should not happen"),
            };
            self.bump_any();
        }
        let format_spec_node = self.start_node();
        let format_spec = if self.eat(Kind::Colon) {
            let mut specs = vec![];
            while matches!(self.cur_kind(), Kind::FStringMiddle | Kind::LeftBracket) {
                specs.push(self.parse_fstring_middle()?);
            }
            Some(Expression::JoinedStr(Box::new(JoinedStr {
                node: self.finish_node(format_spec_node),
                values: specs,
            })))
        } else {
            None
        };

        self.expect(Kind::RightBracket)?;
        Ok(Expression::FormattedValue(Box::new(FormattedValue {
            node: self.finish_node(node),
            value: expr,
            conversion,
            format_spec,
        })))
    }

    fn parse_fstring_middle(&mut self) -> Result<Expression, ParsingError> {
        let node = self.start_node();
        match self.cur_kind() {
            Kind::FStringMiddle => {
                let str_val = self.cur_token().value.to_string();
                self.bump(Kind::FStringMiddle);
                Ok(Expression::Constant(Box::new(Constant {
                    node: self.finish_node(node),
                    value: ConstantValue::Str(str_val),
                })))
            }
            Kind::LeftBracket => self.parse_fstring_replacement_field(),
            _ => {
                panic!("kind is {}", self.cur_token());
            }
        }
    }

    pub fn to_row_col(&self, source_offset: u32) -> (u32, u32) {
        self.lexer.to_row_col(source_offset)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use insta::assert_debug_snapshot;
    use insta::assert_snapshot;

    use super::*;

    #[test]
    fn test_parse_assignment() {
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
            "a = 1, 2",
            "a = 1, 2, ",
            "a = b = 1",
            "a,b = c,d = 1,2",
            // augmented assignment
            "a += 1",
            "a -= 1",
            "a *= 1",
            "a /= 1",
            "a //= 1",
            "a %= 1",
            "a **= 1",
            "a <<= 1",
            "a >>= 1",
            "a &= 1",
            "a ^= 1",
            "a |= 1",
            // annotated assignment
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_parse_assert_stmt() {
        for test_case in &["assert a", "assert a, b", "assert True, 'fancy message'"] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_pass_stmt() {
        for test_case in &["pass", "pass ", "pass\n"] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_parse_del_stmt() {
        for test_case in &["del a", "del a, b", "del a, b, "] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn parse_yield_statement() {
        for test_case in &["yield", "yield a", "yield a, b", "yield a, b, "] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_raise_statement() {
        for test_case in &["raise", "raise a", "raise a from c"] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_parse_break_continue() {
        for test_case in &["break", "continue"] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_parse_bool_op() {
        for test_case in &["a or b", "a and b", "a or b or c", "a and b or c"] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_parse_unary_op() {
        for test_case in &["not a", "+ a", "~ a", "-a"] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_named_expression() {
        {
            let test_case = &"(a := b)";
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_tuple() {
        for test_case in &[
            "(a, b, c)",
            "(a,
            b, c)",
            "(a
            , b, c)",
            "(a,
            b,
                c)",
            "(a,
)",
            "(a, b, c,)",
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_yield_expression() {
        for test_case in &["yield", "yield a", "yield from a"] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_starred() {
        {
            let test_case = &"(*a)";
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_await_expression() {
        {
            let test_case = &"await a";
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_attribute_ref() {
        for test_case in &["a.b", "a.b.c", "a.b_c", "a.b.c.d"] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn parse_call() {
        for test_case in &[
            "a()",
            "a(b)",
            "a(b, c)",
            "func(b=c)",
            "func(a, b=c, d=e)",
            "func(a, b=c, d=e, *f)",
            "func(a, b=c, d=e, *f, **g)",
            "func(a,)",
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_lambda() {
        for test_case in &[
            "lambda: a",
            "lambda a: a",
            "lambda a, b: a",
            "lambda a, b, c: a",
            "lambda a, *b: a",
            "lambda a, *b, c: a",
            "lambda a, *b, c, **d: a",
            "lambda a=1 : a",
            "lambda a=1 : a,",
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_conditional_expression() {
        {
            let test_case = &"a if b else c if d else e";
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_string_literal_concatnation() {
        for test_case in &[
            "'a' 'b'",
            "b'a' b'b'",
            "'a'   'b'",
            "r'a' 'b'",
            "('a'
            'b')",
            "('a'
            'b', 'c')",
            "('a'
                'b'
'c')",
            "f'a' 'c'",
            "f'a' 'b' 'c'",
            "'d' f'a' 'b'",
            "f'a_{1}' 'b' ",
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_fstring() {
        for test_case in &[
            "f'a'",
            "f'hello_{a}'",
            "f'hello_{a} {b}'",
            "f'hello_{a} {b} {c}'",
            // unsupported
            // "f'hello_{f'''{a}'''}'",
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_comparison() {
        for test_case in &[
            "a == b",
            "a != b",
            "a > b",
            "a < b",
            "a >= b",
            "a <= b",
            "a is b",
            "a is not b",
            "a in b",
            "a not in b",
            "a < b < c",
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                omit_expression => true // do not include the default expression
            }, {
                assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_while_statement() {
        for test_case in &[
            "while a: pass",
            "while a:
    pass",
            "while a:
        a = 1
else:
        b = 1
",
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_try_statement() {
        for test_case in &[
            "try:
    pass
except:
    pass",
            "try:
                pass
except Exception:
                pass",
            "try:
                pass
except Exception as e:
                pass",
            "try:
                pass
except Exception as e:
                pass
else:
                pass",
            "try:
                pass
except Exception as e:
                pass
else:
                pass
finally:
                pass",
            "try:
    pass
except *Exception as e:
    pass
",
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                        snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    #[test]
    fn test_ellipsis_statement() {
        for test_case in &[
            "def a(): ...",
            "def a():
    ...",
            "a = ...",
            "... + 1",
        ] {
            let mut parser = Parser::new(test_case, "");
            let program = parser.parse().expect("parsing failed");

            insta::with_settings!({
                    description => test_case.to_string(), // the template source code
                    snapshot_path => "../../test_data/output/",
                    omit_expression => true // do not include the default expression
                }, {
                    assert_debug_snapshot!(program);
            });
        }
    }

    macro_rules! parser_test {
        ($test_name:ident, $test_file:expr) => {
            #[test]
            fn $test_name() {
                let test_case = fs::read_to_string($test_file).unwrap();
                let mut parser = Parser::new(
                    &test_case,
                    $test_file,
                );
                let program = parser.parse().expect("parsing failed");
                let snapshot = format!("{program:#?}");

                insta::with_settings!({
                        description => format!("test file: {}\n{}", $test_file, test_case.clone()),
                        omit_expression => true,
                        snapshot_path => "../../test_data/output/"
                    }, {
                        assert_snapshot!(snapshot);
                });
            }
            }

    }

    parser_test!(test_functions, "test_data/inputs/functions.py");
    parser_test!(test_if, "test_data/inputs/if.py");
    parser_test!(test_indentation, "test_data/inputs/indentation.py");
    parser_test!(
        test_separate_statements,
        "test_data/inputs/separate_statements.py"
    );
    parser_test!(test_try, "test_data/inputs/try.py");
    parser_test!(
        annotated_assignment,
        "test_data/inputs/annotated_assignment.py"
    );
    parser_test!(binary_op, "test_data/inputs/binary_op.py");
    parser_test!(class, "test_data/inputs/class.py");
    parser_test!(dict, "test_data/inputs/dict.py");
    parser_test!(test_for, "test_data/inputs/for.py");
    parser_test!(from_import, "test_data/inputs/from_import.py");
    parser_test!(function_def, "test_data/inputs/function_def.py");
    parser_test!(
        generator_expressions,
        "test_data/inputs/generator_expressions.py"
    );
    parser_test!(lists, "test_data/inputs/lists.py");
    parser_test!(test_match, "test_data/inputs/match.py");
    parser_test!(sets, "test_data/inputs/sets.py");
    parser_test!(string, "test_data/inputs/string.py");
    parser_test!(subscript, "test_data/inputs/subscript.py");
    parser_test!(with, "test_data/inputs/with.py");
    parser_test!(newlines, "test_data/inputs/newlines.py");
    parser_test!(comments, "test_data/inputs/comments.py");
    parser_test!(types_alias, "test_data/inputs/type_alias.py");
}
