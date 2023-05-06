use std::panic;

use crate::lexer::lexer::Lexer;
use crate::parser::ast::*;
use crate::parser::string::{extract_string_inside, is_string};
use crate::token::{Kind, Token, TokenValue};
use miette::Result;

use super::diagnostics;
use super::expression::{is_atom, is_iterable, is_primary};
use super::operator::{
    is_bin_arithmetic_op, is_comparison_operator, is_unary_op, map_unary_operator,
};
use super::statement::is_at_compound_statement;
use super::string::concat_string_exprs;

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
            let stmt = if is_at_compound_statement(&self.cur_kind()) {
                self.parse_compount_statement()
            } else {
                self.parse_simple_statement()
            };
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

    fn parse_simple_statement(&mut self) -> Result<Statement> {
        let stmt = match self.cur_kind() {
            Kind::Assert => self.parse_assert_statement(),
            Kind::Pass => self.parse_pass_statement(),
            Kind::Del => self.parse_del_statement(),
            Kind::Return => self.parse_return_statement(),
            // https://docs.python.org/3/reference/simple_stmts.html#the-yield-statement
            Kind::Yield => Ok(Statement::ExpressionStatement(
                self.parse_yield_expression()?,
            )),
            Kind::Raise => self.parse_raise_statement(),
            Kind::Break => self.parse_break_statement(),
            Kind::Continue => self.parse_continue_statement(),
            Kind::Import => self.parse_import_statement(),
            Kind::From => self.parse_from_import_statement(),
            Kind::Global => self.parse_global_statement(),
            Kind::Nonlocal => self.parse_nonlocal_statement(),
            _ => self.parse_assignment_or_expression_statement(),
        };

        self.bump(Kind::NewLine);

        stmt
    }

    fn parse_compount_statement(&mut self) -> Result<Statement> {
        let stmt = match self.cur_kind() {
            Kind::If => self.parse_if_statement(),
            Kind::While => self.parse_while_statement(),
            Kind::For => self.parse_for_statement(),
            // Kind::Try => self.parse_try_statement(),
            Kind::With => self.parse_with_statement(),
            // Kind::Def => self.parse_function_definition(),
            // Kind::Class => self.parse_class_definition(),
            _ => {
                let range = self.finish_node(self.start_node());
                Err(
                    diagnostics::ExpectToken("compound statement", self.cur_kind().to_str(), range)
                        .into(),
                )
            }
        };

        self.bump(Kind::NewLine);

        stmt
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        self.bump(Kind::If);
        let node = self.start_node();
        let test = Box::new(self.parse_assignment_expression()?);
        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;
        let mut orelse: Option<If> = None;
        while self.at(Kind::Elif) {
            let elif_node = self.start_node();
            self.bump(Kind::Elif);
            let elif_test = Box::new(self.parse_assignment_expression()?);
            self.expect(Kind::Colon)?;
            let body = self.parse_suite()?;
            let if_value = If {
                node: self.finish_node(elif_node),
                test: elif_test,
                body,
                orelse: vec![],
            };
            if let Some(val) = &mut orelse {
                val.update_orelse(vec![Statement::IfStatement(if_value)]);
            } else {
                orelse = Some(if_value);
            }
        }

        if self.at(Kind::Else) {
            self.bump(Kind::Else);
            self.expect(Kind::Colon)?;
            let else_body = self.parse_suite()?;
            if let Some(val) = &mut orelse {
                val.update_orelse(else_body);
            }
        }

        // if we had any else or elif statements, we need to wrap them in a vec
        // otherwise we just return an empty vec as the else block of the if statement
        let or_else_vec = if let Some(val) = orelse {
            vec![Statement::IfStatement(val)]
        } else {
            vec![]
        };

        // There can be a dedent after the if block
        // The only other token here can be a eof
        self.bump(Kind::Dedent);

        Ok(Statement::IfStatement(If {
            node: self.finish_node(node),
            test,
            body,
            orelse: or_else_vec,
        }))
    }

    fn parse_while_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::While);
        let test = Box::new(self.parse_assignment_expression()?);
        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;
        let orelse = if self.at(Kind::Else) {
            self.bump(Kind::Else);
            self.expect(Kind::Colon)?;
            self.parse_suite()?
        } else {
            vec![]
        };

        self.bump(Kind::Dedent);

        Ok(Statement::WhileStatement(While {
            node: self.finish_node(node),
            test,
            body,
            orelse,
        }))
    }

    fn parse_for_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::For);
        let target = Box::new(self.parse_target_list()?);
        self.expect(Kind::In)?;
        let iter_list = self.parse_starred_list(Kind::Colon)?;
        let iter = if iter_list.len() > 1 {
            Box::new(Expression::Tuple(Box::new(Tuple {
                node: self.finish_node(node),
                elements: iter_list,
            })))
        } else if iter_list.len() == 1 {
            Box::new(iter_list.into_iter().next().unwrap())
        } else {
            return Err(diagnostics::ExpectToken(
                "exptected iterator in for loop",
                self.cur_kind().to_str(),
                self.finish_node(node),
            )
            .into());
        };
        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;
        let orelse = if self.eat(Kind::Else) {
            self.expect(Kind::Colon)?;
            self.parse_suite()?
        } else {
            vec![]
        };

        self.bump(Kind::Dedent);

        Ok(Statement::ForStatement(For {
            node: self.finish_node(node),
            target,
            iter,
            body,
            orelse,
        }))
    }

    fn parse_with_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::With);
        let items = self.parse_with_items()?;
        self.expect(Kind::Colon)?;
        let body = self.parse_suite()?;

        self.bump(Kind::Dedent);

        Ok(Statement::WithStatement(With {
            node: self.finish_node(node),
            items,
            body,
        }))
    }

    fn parse_with_items(&mut self) -> Result<Vec<WithItem>> {
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

    fn parse_with_item(&mut self) -> Result<WithItem> {
        let node = self.start_node();
        let context_expr = Box::new(self.parse_expression_2()?);
        let optional_vars = if self.eat(Kind::As) {
            Some(Box::new(self.parse_target()?))
        } else {
            None
        };

        Ok(WithItem {
            node: self.finish_node(node),
            context_expr,
            optional_vars,
        })
    }

    fn parse_assignment_or_expression_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        let lhs = self.parse_expression()?;
        let stmt = if self.cur_kind() == Kind::Assign {
            self.parse_assignment_statement(node, lhs)
        } else if matches!(
            self.cur_kind(),
            Kind::AddAssign
                | Kind::SubAssign
                | Kind::MulAssign
                | Kind::DivAssign
                | Kind::IntDivAssign
                | Kind::ModAssign
                | Kind::PowAssign
                | Kind::BitAndAssign
                | Kind::BitOrAssign
                | Kind::BitXorAssign
                | Kind::ShiftLeftAssign
                | Kind::ShiftRightAssign
        ) {
            self.parse_aug_assignment_statement(node, lhs)
        } else if self.at(Kind::Colon) {
            self.parse_ann_assign_statement(node, lhs)
        } else {
            Ok(Statement::ExpressionStatement(lhs))
        };
        stmt
    }

    // https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-suite
    fn parse_suite(&mut self) -> Result<Vec<Statement>> {
        if self.eat(Kind::NewLine) {
            self.expect(Kind::Indent)?;
            let mut stmts = vec![];
            while !self.eat(Kind::Dedent) && !self.at(Kind::Eof) {
                let stmt = self.parse_statement()?;
                stmts.extend(stmt);
            }
            Ok(stmts)
        } else {
            let stmt = self.parse_statement_list()?;
            self.bump(Kind::NewLine);
            Ok(stmt)
        }
    }

    // https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-statement
    fn parse_statement(&mut self) -> Result<Vec<Statement>> {
        let stmt = if is_at_compound_statement(&self.cur_kind()) {
            let comp_stmt = self.parse_compount_statement()?;
            Ok(vec![comp_stmt])
        } else {
            let stmt_list = self.parse_statement_list();
            self.bump(Kind::NewLine);
            stmt_list
        };
        stmt
    }

    // https://docs.python.org/3/reference/simple_stmts.html#grammar-token-python-grammar-stmt-list
    fn parse_statement_list(&mut self) -> Result<Vec<Statement>> {
        let mut stmts = vec![];
        let stmt = self.parse_simple_statement()?;
        stmts.push(stmt);
        while self.eat(Kind::SemiColon) {
            let stmt = self.parse_simple_statement()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn parse_del_statement(&mut self) -> Result<Statement> {
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
        Ok(Statement::Delete(Delete {
            node: self.finish_node(node),
            targets: expr,
        }))
    }

    fn parse_assignment_statement(&mut self, start: Node, lhs: Expression) -> Result<Statement> {
        let mut targets = vec![lhs];
        self.bump(Kind::Assign);
        let value = loop {
            let rhs = self.parse_expression()?;
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
        return Ok(Statement::AssignStatement(Assign {
            node: self.finish_node(start),
            targets,
            value,
        }));
    }

    fn parse_aug_assignment_statement(
        &mut self,
        start: Node,
        lhs: Expression,
    ) -> Result<Statement> {
        let op = self.parse_aug_assign_op()?;
        let value = self.parse_assignment_value()?;

        return Ok(Statement::AugAssignStatement(AugAssign {
            node: self.finish_node(start),
            target: lhs,
            op,
            value,
        }));
    }

    fn parse_ann_assign_statement(&mut self, start: Node, lhs: Expression) -> Result<Statement> {
        self.bump(Kind::Colon);
        let annotation = self.parse_expression_2()?;
        let value = if self.eat(Kind::Assign) {
            Some(self.parse_assignment_value()?)
        } else {
            None
        };
        return Ok(Statement::AnnAssignStatement(AnnAssign {
            node: self.finish_node(start),
            target: lhs,
            annotation,
            value,
            // TODO: implement simple
            simple: true,
        }));
    }

    // The value is either expression list or yield expression
    // https://docs.python.org/3/reference/simple_stmts.html#assignment-statements
    fn parse_assignment_value(&mut self) -> Result<Expression> {
        if self.cur_kind() == Kind::Yield {
            return Ok(self.parse_yield_expression()?);
        }
        self.parse_expression_list()
    }

    fn parse_aug_assign_op(&mut self) -> Result<AugAssignOp> {
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
            _ => {
                return Err(diagnostics::ExpectToken(
                    "augmented assignment",
                    self.cur_kind().to_str(),
                    self.finish_node(self.start_node()),
                )
                .into());
            }
        };
        self.bump_any();
        Ok(op)
    }

    fn parse_assert_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::Assert);
        let test = self.parse_expression_2()?;
        let msg = if self.eat(Kind::Comma) {
            Some(self.parse_expression_2()?)
        } else {
            None
        };

        return Ok(Statement::Assert(Assert {
            node: self.finish_node(node),
            test,
            msg,
        }));
    }

    fn parse_pass_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::Pass);
        return Ok(Statement::Pass(Pass {
            node: self.finish_node(node),
        }));
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::Return);
        let value = if self.at(Kind::NewLine) {
            None
        } else {
            Some(self.parse_expression_list()?)
        };
        return Ok(Statement::Return(Return {
            node: self.finish_node(node),
            value,
        }));
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement
    fn parse_raise_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::Raise);
        let exc = if matches!(self.cur_kind(), Kind::NewLine | Kind::Eof) {
            None
        } else {
            Some(self.parse_expression_2()?)
        };
        let cause = if self.eat(Kind::From) {
            Some(self.parse_expression_2()?)
        } else {
            None
        };
        return Ok(Statement::Raise(Raise {
            node: self.finish_node(node),
            exc,
            cause,
        }));
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-break-statement
    fn parse_break_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::Break);
        return Ok(Statement::Break(Break {
            node: self.finish_node(node),
        }));
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-continue-statement
    fn parse_continue_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::Continue);
        return Ok(Statement::Continue(Continue {
            node: self.finish_node(node),
        }));
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-global-statement
    fn parse_global_statement(&mut self) -> Result<Statement> {
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
        return Ok(Statement::Global(Global {
            node: self.finish_node(node),
            names,
        }));
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-nonlocal-statement
    fn parse_nonlocal_statement(&mut self) -> Result<Statement> {
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
        return Ok(Statement::Nonlocal(Nonlocal {
            node: self.finish_node(node),
            names,
        }));
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-import-statement
    fn parse_import_statement(&mut self) -> Result<Statement> {
        let node = self.start_node();
        self.bump(Kind::Import);
        let mut aliases = vec![];
        while self.at(Kind::Identifier) {
            let node = self.start_node();
            let module = self.parse_module_name();
            let alias = self.parse_alias(module, node);
            aliases.push(alias);

            if !self.eat(Kind::Comma) {
                break;
            }
        }
        return Ok(Statement::Import(Import {
            node: self.finish_node(node),
            names: aliases,
        }));
    }

    // https://docs.python.org/3/reference/simple_stmts.html#the-from-import-statement
    fn parse_from_import_statement(&mut self) -> Result<Statement> {
        let import_node = self.start_node();
        self.bump(Kind::From);
        let module = self.parse_module_name();
        self.bump(Kind::Import);
        let mut aliases = vec![];
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
        return Ok(Statement::ImportFrom(ImportFrom {
            node: self.finish_node(import_node),
            module,
            names: aliases,
            level: 0,
        }));
    }

    fn parse_alias(&mut self, name: String, node: Node) -> Alias {
        let asname = if self.eat(Kind::As) {
            let alias_name = self.cur_token().value.to_string();
            self.bump(Kind::Identifier);
            Some(alias_name)
        } else {
            None
        };
        return Alias {
            node: self.finish_node(node),
            name,
            asname,
        };
    }

    fn parse_module_name(&mut self) -> String {
        let mut module = String::from(self.cur_token().value.to_string());
        self.bump(Kind::Identifier);
        while self.eat(Kind::Dot) {
            module.push('.');
            module.push_str(self.cur_token().value.to_string().as_str());
            self.bump(Kind::Identifier);
        }
        return module;
    }

    // https://docs.python.org/3/library/ast.html#ast.Expr
    fn parse_expression(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let expr = self.parse_expression_2()?;

        let mut exprs = vec![];
        if self.at(Kind::Comma) {
            exprs.push(expr);
            while self.eat(Kind::Comma) {
                if self.at(Kind::Eof) {
                    break;
                }
                exprs.push(self.parse_expression_2()?);
            }
        } else {
            return Ok(expr);
        }

        return Ok(Expression::Tuple(Box::new(Tuple {
            node: self.finish_node(node),
            elements: exprs,
        })));
    }

    // https://docs.python.org/3/reference/expressions.html#conditional-expressions
    fn parse_conditional_expression(&mut self) -> Result<Expression> {
        let or_test = self.parse_or_test();
        if self.eat(Kind::If) {
            let test = self.parse_or_test()?;
            self.expect(Kind::Else)?;
            let or_else = self.parse_expression_2()?;
            return Ok(Expression::IfExp(Box::new(IfExp {
                node: self.start_node(),
                test: Box::new(test),
                body: Box::new(or_test?),
                orelse: Box::new(or_else),
            })));
        }

        or_test
    }

    // https://docs.python.org/3/reference/expressions.html#assignment-expressions
    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        let node = self.start_node();
        if self.at(Kind::Identifier) && matches!(self.peek_kind()?, Kind::Walrus) {
            let identifier = self.cur_token().value.to_string();
            let mut identifier_node = self.start_node();
            identifier_node = self.finish_node(identifier_node);
            self.expect(Kind::Identifier)?;
            identifier_node = self.finish_node(identifier_node);
            if self.eat(Kind::Walrus) {
                let value = self.parse_expression_2()?;
                return Ok(Expression::NamedExpr(Box::new(NamedExpression {
                    node: self.finish_node(node),
                    target: Box::new(Expression::Name(Box::new(Name {
                        node: identifier_node,
                        id: identifier,
                    }))),
                    value: Box::new(value),
                })));
            }
            return Ok(Expression::Name(Box::new(Name {
                node: identifier_node,
                id: identifier,
            })));
        }

        self.parse_expression_2()
    }

    // https://docs.python.org/3/reference/expressions.html#list-displays
    fn parse_list(&mut self) -> Result<Expression> {
        let node = self.start_node();
        self.bump(Kind::LeftBrace);
        let elements = self.parse_starred_list(Kind::RightBrace)?;
        self.expect(Kind::RightBrace)?;
        Ok(Expression::List(Box::new(List {
            node: self.finish_node(node),
            elements,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#parenthesized-forms
    fn parse_paren_form_or_generator(&mut self) -> Result<Expression> {
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
                self.parse_assignment_expression()?
            } else if self.eat(Kind::Mul) {
                let expr = self.parse_or_expr()?;
                Expression::Starred(Box::new(Starred {
                    node: self.finish_node(node),
                    value: Box::new(expr),
                }))
            } else {
                self.parse_expression_2()?
            };

        if matches!(self.cur_kind(), Kind::For) || matches!(self.peek_kind(), Ok(Kind::For)) {
            let generators = self.parse_comp_for()?;
            self.expect(Kind::RightParen)?;
            return Ok(Expression::Generator(Box::new(Generator {
                node: self.finish_node(node),
                element: Box::new(first_expr),
                generators,
            })));
        }

        let expr = self.parse_starred_expression(node, first_expr)?;
        self.expect(Kind::RightParen)?;
        Ok(expr)
    }

    // https://docs.python.org/3/reference/expressions.html#displays-for-lists-sets-and-dictionaries
    fn parse_comp_for(&mut self) -> Result<Vec<Comprehension>> {
        // if current token is async
        let is_async = if self.eat(Kind::Async) { true } else { false };

        let mut generators = vec![];
        loop {
            let node = self.start_node();
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
                target: Box::new(target),
                iter: Box::new(iter),
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
    fn parse_target_list(&mut self) -> Result<Expression> {
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
    fn parse_target(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let mut targets = vec![];
        let target = match self.cur_kind() {
            Kind::Identifier => match self.peek_kind() {
                // TODO: atom cannot be all the atoms like string, number
                Ok(Kind::LeftBrace) => {
                    let atom = self.parse_atom()?;
                    self.parse_subscript(node, atom)?
                }
                Ok(Kind::Dot) => {
                    let atom = self.parse_atom()?;
                    self.parse_atribute_ref(node, atom)?
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
                value: Box::new(self.parse_target()?),
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

    fn parse_dict_or_set(&mut self) -> Result<Expression> {
        let node = self.start_node();
        self.bump(Kind::LeftBracket);
        if matches!(self.peek_kind(), Ok(Kind::Comma)) {
            self.parse_set(node)
        } else {
            self.parse_dict(node)
        }
    }

    // https://docs.python.org/3/reference/expressions.html#set-displays
    fn parse_set(&mut self, node: Node) -> Result<Expression> {
        let elements = self.parse_starred_list(Kind::RightBracket)?;
        self.expect(Kind::RightBracket)?;
        Ok(Expression::Set(Box::new(Set {
            node: self.finish_node(node),
            elements,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#dictionary-displays
    fn parse_dict(&mut self, node: Node) -> Result<Expression> {
        let mut keys = vec![];
        let mut values = vec![];
        while !self.eat(Kind::RightBracket) {
            let key = self.parse_expression_2()?;
            self.expect(Kind::Colon)?;
            let value = self.parse_expression_2()?;
            keys.push(key);
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

    fn consume_whitespace_and_newline(&mut self) {
        while matches!(
            self.cur_kind(),
            Kind::WhiteSpace | Kind::NewLine | Kind::Indent | Kind::Dedent
        ) {
            self.bump(self.cur_kind());
        }
    }

    // https://docs.python.org/3/reference/expressions.html#expression-lists
    // termination_kind is used to know when to stop parsing the list
    // for example to parse a tuple the termination_kind is Kind::RightParen
    // caller is responsible to consume the first & last occurrence of the termination_kind
    fn parse_starred_list(&mut self, termination_kind: Kind) -> Result<Vec<Expression>> {
        let mut expressions = vec![];
        while !self.at(Kind::Eof) && !self.at(termination_kind) {
            let expr = self.parse_starred_item()?;
            if !self.at(Kind::Eof) && !self.at(termination_kind) {
                self.expect(Kind::Comma)?;
            }
            expressions.push(expr);
        }
        Ok(expressions)
    }

    // https://docs.python.org/3/reference/expressions.html#expression-lists
    fn parse_starred_item(&mut self) -> Result<Expression> {
        let mut node = self.start_node();
        if self.eat(Kind::Mul) {
            let starred_value_kind = self.cur_kind().clone();
            let expr = self.parse_or_expr()?;
            node = self.finish_node(node);
            if !is_iterable(&expr) {
                return Err(diagnostics::UnexpectedToken(starred_value_kind.to_str(), node).into());
            }
            return Ok(Expression::Starred(Box::new(Starred {
                node: self.finish_node(node),
                value: Box::new(expr),
            })));
        }
        self.parse_assignment_expression()
    }

    // https://docs.python.org/3/reference/expressions.html#await-expression
    fn parse_await(&mut self) -> Result<Expression> {
        let node = self.start_node();
        self.bump(Kind::Await);
        // we clone the token here to inform user in case
        // the expression is not a primary expression
        // it's more clear to say which token is unexpected
        // instead of saying some expression is unexpected
        let await_value_token = self.cur_token().clone();
        let await_value = self.parse_expression_2()?;
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

    // https://docs.python.org/3/reference/expressions.html#conditional-expressions
    fn parse_expression_2(&mut self) -> Result<Expression> {
        let node = self.start_node();
        if self.eat(Kind::Lambda) {
            let params_list = self.parse_parameters(true).expect("lambda params");
            self.expect(Kind::Colon)?;
            let expr = self.parse_expression_2()?;

            return Ok(Expression::Lambda(Box::new(Lambda {
                node: self.finish_node(node),
                args: params_list,
                body: Box::new(expr),
            })));
        }
        self.parse_conditional_expression()
    }

    // https://docs.python.org/3/reference/expressions.html#boolean-operations
    fn parse_or_test(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let lhs = self.parse_and_test()?;
        if self.eat(Kind::Or) {
            let rhs = self.parse_or_test()?;
            return Ok(Expression::BoolOp(Box::new(BoolOperation {
                node: self.finish_node(node),
                op: BooleanOperator::Or,
                values: vec![lhs, rhs],
            })));
        }
        Ok(lhs)
    }

    // https://docs.python.org/3/reference/expressions.html#boolean-operations
    fn parse_and_test(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let lhs = self.parse_not_test()?;
        if self.at(Kind::And) {
            self.bump(Kind::And);
            let rhs = self.parse_not_test()?;
            return Ok(Expression::BoolOp(Box::new(BoolOperation {
                node: self.finish_node(node),
                op: BooleanOperator::And,
                values: vec![lhs, rhs],
            })));
        }
        Ok(lhs)
    }

    // https://docs.python.org/3/reference/expressions.html#boolean-operations
    fn parse_not_test(&mut self) -> Result<Expression> {
        let node = self.start_node();
        if self.at(Kind::Not) {
            self.bump(Kind::Not);
            let operand = self.parse_not_test()?;
            return Ok(Expression::UnaryOp(Box::new(UnaryOperation {
                node: self.finish_node(node),
                op: UnaryOperator::Not,
                operand: Box::new(operand),
            })));
        }
        self.parse_comparison()
    }

    // https://docs.python.org/3/reference/expressions.html#comparisons
    fn parse_comparison(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let or_expr = self.parse_or_expr()?;
        let mut ops = vec![];
        let mut comparators = vec![];
        while is_comparison_operator(&self.cur_kind()) {
            ops.push(self.parse_comp_operator()?);
            comparators.push(self.parse_or_expr()?);
        }

        if !ops.is_empty() {
            return Ok(Expression::Compare(Box::new(Compare {
                node: self.finish_node(node),
                left: Box::new(or_expr),
                ops,
                comparators,
            })));
        }

        Ok(or_expr)
    }

    // Binary bitwise operations
    // https://docs.python.org/3/reference/expressions.html#binary-bitwise-operations
    fn parse_or_expr(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let xor_expr = self.parse_xor_expr()?;
        if self.eat(Kind::BitOr) {
            let lhs = self.parse_xor_expr()?;
            return Ok(Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op: BinaryOperator::BitOr,
                left: Box::new(xor_expr),
                right: Box::new(lhs),
            })));
        }
        return Ok(xor_expr);
    }

    // https://docs.python.org/3/reference/expressions.html#binary-bitwise-operations
    fn parse_xor_expr(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let and_expr = self.parse_and_expr()?;
        if self.eat(Kind::BitXor) {
            let lhs = self.parse_and_expr()?;
            return Ok(Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op: BinaryOperator::BitXor,
                left: Box::new(and_expr),
                right: Box::new(lhs),
            })));
        }
        return Ok(and_expr);
    }

    // https://docs.python.org/3/reference/expressions.html#binary-bitwise-operations
    fn parse_and_expr(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let shift_expr = self.parse_shift_expr()?;

        if self.eat(Kind::BitAnd) {
            let lhs = self.parse_shift_expr()?;
            return Ok(Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op: BinaryOperator::BitAnd,
                left: Box::new(shift_expr),
                right: Box::new(lhs),
            })));
        }
        return Ok(shift_expr);
    }

    // https://docs.python.org/3/reference/expressions.html#shifting-operations
    fn parse_shift_expr(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let arith_expr = self.parse_binary_arithmetic_operation()?;
        if self.at(Kind::LeftShift) || self.at(Kind::RightShift) {
            let op = if self.eat(Kind::LeftShift) {
                BinaryOperator::LShift
            } else {
                self.bump(Kind::RightShift);
                BinaryOperator::RShift
            };
            let lhs = self.parse_binary_arithmetic_operation()?;
            return Ok(Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op,
                left: Box::new(arith_expr),
                right: Box::new(lhs),
            })));
        }
        return Ok(arith_expr);
    }

    // https://docs.python.org/3/reference/expressions.html#binary-arithmetic-operations
    fn parse_binary_arithmetic_operation(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let lhs = self.parse_unary_arithmetric_operation()?;
        if is_bin_arithmetic_op(&self.cur_kind()) {
            let op = self.parse_bin_arithmetic_op()?;
            let rhs = self.parse_unary_arithmetric_operation()?;
            return Ok(Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
            })));
        }
        return Ok(lhs);
    }

    // https://docs.python.org/3/reference/expressions.html#unary-arithmetic-and-bitwise-operations
    fn parse_unary_arithmetric_operation(&mut self) -> Result<Expression> {
        let node = self.start_node();
        if is_unary_op(&self.cur_kind()) {
            let op = map_unary_operator(&self.cur_kind());
            self.bump_any();
            let operand = self.parse_unary_arithmetric_operation()?;
            return Ok(Expression::UnaryOp(Box::new(UnaryOperation {
                node: self.finish_node(node),
                op,
                operand: Box::new(operand),
            })));
        }
        self.parse_power_expression()
    }

    // https://docs.python.org/3/reference/expressions.html#the-power-operator
    fn parse_power_expression(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let base = if self.at(Kind::Await) {
            self.bump(Kind::Await);
            let value = self.parse_primary()?;
            Ok(Expression::Await(Box::new(Await {
                node: self.finish_node(node),
                value: Box::new(value),
            })))
        } else {
            self.parse_primary()
        };
        if self.eat(Kind::Pow) {
            let exponent = self.parse_unary_arithmetric_operation()?;
            return Ok(Expression::BinOp(Box::new(BinOp {
                node: self.finish_node(node),
                op: BinaryOperator::Pow,
                left: Box::new(base?),
                right: Box::new(exponent),
            })));
        }

        return base;
    }

    // https://docs.python.org/3/reference/expressions.html#primaries
    fn parse_primary(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let atom_or_primary = if is_atom(&self.cur_kind()) {
            self.parse_atom()?
        } else {
            unimplemented!("parse_primary: {:?}", self.cur_kind())
        };
        let primary = if self.at(Kind::Dot) {
            self.parse_atribute_ref(node, atom_or_primary)
        } else if self.at(Kind::LeftBrace) {
            // https://docs.python.org/3/reference/expressions.html#slicings
            self.parse_subscript(node, atom_or_primary)
        } else if self.eat(Kind::LeftParen) {
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
                    let keyword_arg = match self.parse_keyword_item() {
                        Ok(keyword_arg) => keyword_arg,
                        Err(_) => {
                            return Err(diagnostics::ExpectToken(
                                "Keyword argument",
                                self.cur_kind().to_str(),
                                self.finish_node(self.start_node()),
                            )
                            .into());
                        }
                    };
                    keyword_args.push(keyword_arg);
                } else if self.at(Kind::Mul) {
                    let star_arg_node = self.start_node();
                    self.bump(Kind::Mul);
                    let star_arg = Expression::Starred(Box::new(Starred {
                        node: self.finish_node(star_arg_node),
                        value: Box::new(self.parse_expression_2()?),
                    }));
                    positional_args.push(star_arg);
                } else if self.at(Kind::Pow) {
                    let kwarg_node = self.start_node();
                    self.bump(Kind::Pow);
                    seen_keyword = true;
                    let kwarg = Keyword {
                        node: self.finish_node(kwarg_node),
                        arg: None,
                        value: Box::new(self.parse_expression_2()?),
                    };
                    keyword_args.push(kwarg);
                } else {
                    if seen_keyword {
                        // TODO change to synatx error
                        return Err(diagnostics::ExpectToken(
                            "Positional argument after keyword argument",
                            self.cur_kind().to_str(),
                            self.finish_node(self.start_node()),
                        )
                        .into());
                    }
                    let arg = self.parse_assignment_expression()?;
                    positional_args.push(arg);
                }
                if !self.eat(Kind::Comma) {
                    break;
                }
            }

            self.bump(Kind::Comma);
            self.expect(Kind::RightParen)?;

            Ok(Expression::Call(Box::new(Call {
                node: self.finish_node(node),
                func: Box::new(atom_or_primary),
                args: positional_args,
                keywords: keyword_args,
                starargs: None,
                kwargs: None,
            })))
        } else {
            Ok(atom_or_primary)
        };

        return primary;
    }

    fn parse_atribute_ref(&mut self, node: Node, value: Expression) -> Result<Expression> {
        let mut expr = Ok(value);
        while self.eat(Kind::Dot) {
            let attr_val = self.cur_token().value.to_string();
            self.expect(Kind::Identifier)?;
            expr = Ok(Expression::Attribute(Box::new(Attribute {
                node: self.finish_node(node),
                value: Box::new(expr?),
                attr: attr_val,
            })));
        }
        return expr;
    }

    fn parse_subscript(&mut self, node: Node, value: Expression) -> Result<Expression> {
        let mut expr = Ok(value);
        while self.eat(Kind::LeftBrace) {
            let slice = self.parse_slice_list()?;
            expr = Ok(Expression::Subscript(Box::new(Subscript {
                node: self.finish_node(node),
                value: Box::new(expr?),
                slice: Box::new(slice),
            })));
        }
        return expr;
    }

    // https://docs.python.org/3/reference/expressions.html#atoms
    fn parse_atom(&mut self) -> Result<Expression> {
        let node = self.start_node();
        if self.at(Kind::Yield) {
            return self.parse_yield_expression();
        } else if self.at(Kind::LeftBrace) {
            self.nested_expression_list += 1;
            let list_expr = self.parse_list();
            self.nested_expression_list -= 1;
            return list_expr;
        } else if self.at(Kind::LeftBracket) {
            self.nested_expression_list += 1;
            let dict_or_set_expr = self.parse_dict_or_set();
            self.nested_expression_list -= 1;
            return dict_or_set_expr;
        } else if self.at(Kind::LeftParen) {
            self.nested_expression_list += 1;
            let tuple_or_named_expr = self.parse_paren_form_or_generator();
            self.nested_expression_list -= 1;
            return tuple_or_named_expr;
        } else if self.at(Kind::Identifier) {
            let value = self.cur_token().value.to_string();
            self.bump(Kind::Identifier);
            return Ok(Expression::Name(Box::new(Name {
                node: self.finish_node(node),
                id: value,
            })));
        } else if is_atom(&self.cur_kind()) {
            // value must be cloned to be assigned to the node
            let token_value = self.cur_token().value.clone();
            let token_kind = self.cur_kind();
            // bump needs to happen before creating the atom node
            // otherwise the end would be the same as the start
            self.bump_any();
            let mut expr = self.map_to_atom(node, &token_kind, token_value)?;

            // If the current token is a string, we need to check if there are more strings
            // and concat them
            // https://docs.python.org/3/reference/lexical_analysis.html#string-literal-concatenation
            if is_string(&token_kind) {
                loop {
                    if is_string(&self.cur_kind()) {
                        let token_value = self.cur_token().value.clone();
                        let token_kind = self.cur_kind();
                        self.bump_any();
                        let next_str = self.map_to_atom(node, &token_kind, token_value)?;
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
        } else {
            return Err(diagnostics::UnexpectedToken(
                self.cur_kind().to_str(),
                self.finish_node(node),
            )
            .into());
        }
    }

    // https://docs.python.org/3/reference/expressions.html#yield-expressions
    fn parse_yield_expression(&mut self) -> Result<Expression> {
        let yield_node = self.start_node();
        self.expect(Kind::Yield)?;

        if self.eat(Kind::From) {
            let value = self.parse_expression_2()?;
            return Ok(Expression::YieldFrom(Box::new(YieldFrom {
                node: self.finish_node(yield_node),
                value: Box::new(value),
            })));
        }
        if self.eat(Kind::NewLine) || self.at(Kind::Eof) {
            return Ok(Expression::Yield(Box::new(Yield {
                node: self.finish_node(yield_node),
                value: None,
            })));
        }
        let value = match self.parse_expression_list() {
            Ok(expr) => Some(Box::new(expr)),
            _ => None,
        };
        return Ok(Expression::Yield(Box::new(Yield {
            node: self.finish_node(yield_node),
            value,
        })));
    }

    // https://docs.python.org/3/reference/expressions.html#expression-lists
    fn parse_expression_list(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let mut expressions = vec![];
        expressions.push(self.parse_expression_2()?);
        while self.eat(Kind::Comma) && !self.at(Kind::Eof) {
            let expr = self.parse_expression_2()?;
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

    // https://docs.python.org/3/reference/expressions.html#expression-lists
    fn parse_starred_expression(
        &mut self,
        node: Node,
        first_elm: Expression,
    ) -> Result<Expression> {
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
            return Ok(elements.pop().unwrap());
        }
        Ok(Expression::Tuple(Box::new(Tuple {
            node: self.finish_node(node),
            elements,
        })))
    }

    // https://docs.python.org/3/reference/expressions.html#slicings
    // Clsoing will be consumed by this function
    fn parse_slice_list(&mut self) -> Result<Expression> {
        let node = self.start_node();
        let mut elements = vec![];
        while !self.at(Kind::Eof) && !self.at(Kind::RightBrace) {
            if self.at(Kind::Colon) {
                elements.push(self.parse_proper_slice(None)?);
            } else {
                let expr = self.parse_expression_2()?;
                if self.at(Kind::Colon) {
                    elements.push(self.parse_proper_slice(Some(expr))?);
                } else {
                    elements.push(expr);
                }
            }
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

    // https://docs.python.org/3/reference/expressions.html#slicings
    fn parse_proper_slice(&mut self, lower: Option<Expression>) -> Result<Expression> {
        let node = self.start_node();

        let slice_lower = if lower.is_some() {
            Some(Box::new(lower.unwrap()))
        } else {
            if self.eat(Kind::Colon) {
                None
            } else {
                Some(Box::new(self.parse_expression_2()?))
            }
        };
        let upper = if self.eat(Kind::Colon) {
            if self.at(Kind::RightBrace) {
                None
            } else {
                Some(Box::new(self.parse_expression_2()?))
            }
        } else {
            None
        };
        let step = if self.eat(Kind::Colon) {
            if self.at(Kind::RightBrace) {
                None
            } else {
                Some(Box::new(self.parse_expression_2()?))
            }
        } else {
            None
        };
        Ok(Expression::Slice(Box::new(Slice {
            node: self.finish_node(node),
            lower: slice_lower,
            upper,
            step,
        })))
    }

    fn map_to_atom(&mut self, start: Node, kind: &Kind, value: TokenValue) -> Result<Expression> {
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
            Kind::FStringStart => {
                let fstring = self.parse_fstring()?;
                Expression::JoinedStr(Box::new(JoinedStr {
                    node: self.finish_node(start),
                    values: fstring,
                }))
            }
            _ => {
                return Err(
                    diagnostics::InvalidSyntax("Unexpected token", self.finish_node(start)).into(),
                )
            }
        };
        Ok(atom)
    }

    fn parse_comp_operator(&mut self) -> Result<ComparisonOperator> {
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
                    return Err(diagnostics::UnexpectedToken(
                        self.cur_kind().to_str(),
                        self.finish_node(node),
                    )
                    .into())
                }
            },
            _ => {
                return Err(diagnostics::UnexpectedToken(
                    self.cur_kind().to_str(),
                    self.finish_node(node),
                )
                .into())
            }
        };
        self.bump_any();
        Ok(op)
    }

    fn parse_bin_arithmetic_op(&mut self) -> Result<BinaryOperator> {
        let op = match self.cur_kind() {
            Kind::Plus => Ok(BinaryOperator::Add),
            Kind::Minus => Ok(BinaryOperator::Sub),
            Kind::Mul => Ok(BinaryOperator::Mult),
            Kind::Div => Ok(BinaryOperator::Div),
            Kind::IntDiv => Ok(BinaryOperator::FloorDiv),
            Kind::Mod => Ok(BinaryOperator::Mod),
            Kind::Pow => Ok(BinaryOperator::Pow),
            Kind::MatrixMul => Ok(BinaryOperator::MatMult),
            _ => Err(diagnostics::UnexpectedToken(
                self.cur_kind().to_str(),
                self.finish_node(self.start_node()),
            )
            .into()),
        };
        self.bump_any();
        op
    }

    fn parse_keyword_item(&mut self) -> Result<Keyword> {
        let node = self.start_node();
        let arg = self.cur_token().value.to_string();
        self.bump(Kind::Identifier);
        self.bump(Kind::Assign);
        let value = Box::new(self.parse_expression_2()?);
        Ok(Keyword {
            node: self.finish_node(node),
            arg: Some(arg),
            value,
        })
    }

    fn parse_parameters(&mut self, is_lambda: bool) -> Result<Arguments> {
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
                    return Err(diagnostics::InvalidSyntax(
                        "parameter after kwarg",
                        self.finish_node(node),
                    )
                    .into());
                } else {
                    args.push(param);
                }
                if let Some(default_value) = default {
                    if seen_vararg {
                        kw_defaults.push(default_value);
                    } else {
                        must_have_default = true;
                        defaults.push(default_value);
                    }
                } else if must_have_default {
                    return Err(diagnostics::InvalidSyntax(
                        "non-default argument follows default argument",
                        self.finish_node(node),
                    )
                    .into());
                }
            // If a parameter has a default value, all following parameters up until the “*”
            // must also have a default value — this is a syntactic restriction that is not expressed by the grammar.
            } else if self.eat(Kind::Mul) {
                seen_vararg = true;
                let (param, default) = self.parse_parameter(is_lambda)?;
                // default is not allowed for vararg
                if default.is_some() {
                    return Err(diagnostics::InvalidSyntax(
                        "var-positional argument cannot have default value",
                        self.finish_node(node),
                    )
                    .into());
                }
                vararg = Some(param);
            } else if self.eat(Kind::Pow) {
                seen_kwarg = true;
                let (param, default) = self.parse_parameter(is_lambda)?;
                // default is not allowed for kwarg
                if default.is_some() {
                    return Err(diagnostics::InvalidSyntax(
                        "var-keyword argument cannot have default value",
                        self.finish_node(node),
                    )
                    .into());
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
        return self.cur_kind() == Kind::Identifier;
        // && matches!(self.peek_kind(), Ok(Kind::Assign))
        // || matches!(self.peek_kind(), Ok(Kind::Colon))
    }

    fn parse_parameter(&mut self, is_lambda: bool) -> Result<(Arg, Option<Expression>)> {
        let node = self.start_node();
        let arg = self.cur_token().value.to_string();
        self.bump(Kind::Identifier);
        // Lambda parameters cannot have annotations
        let annotation = if self.at(Kind::Colon) && !is_lambda {
            self.bump(Kind::Colon);
            Some(self.parse_expression_2()?)
        } else {
            None
        };
        let default = if self.eat(Kind::Assign) {
            Some(self.parse_expression_2()?)
        } else {
            None
        };
        Ok((
            Arg {
                node: self.finish_node(node),
                arg,
                annotation,
            },
            default,
        ))
    }

    // the FStringStart token is consumed by the caller
    fn parse_fstring(&mut self) -> Result<Vec<Expression>> {
        let mut expressions = vec![];
        while self.cur_kind() != Kind::FStringEnd {
            match self.cur_kind() {
                Kind::FStringMiddle => {
                    let str_val = self.cur_token().value.to_string().clone();
                    self.bump(Kind::FStringMiddle);
                    expressions.push(Expression::Constant(Box::new(Constant {
                        node: self.start_node(),
                        value: ConstantValue::Str(str_val),
                    })));
                }
                Kind::LeftBracket => {
                    self.bump(Kind::LeftBracket);
                    expressions.push(self.parse_expression()?);
                    self.expect(Kind::RightBracket)?;
                }
                _ => {
                    return Err(diagnostics::UnexpectedToken(
                        "unknown token in fstring",
                        self.finish_node(self.start_node()),
                    )
                    .into());
                }
            }
        }
        self.bump(Kind::FStringEnd);
        Ok(expressions)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

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
            "a: int = 1",
            "a: int = 1, 2",
            "a: int = 1, 2, ",
            "a: int = b",
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
    fn test_parse_assert_stmt() {
        for test_case in &["assert a", "assert a, b", "assert True, 'fancy message'"] {
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
    fn test_pass_stmt() {
        for test_case in &["pass", "pass ", "pass\n"] {
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
    fn test_parse_del_stmt() {
        for test_case in &["del a", "del a, b", "del a, b, "] {
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
    fn parse_yield_statement() {
        for test_case in &["yield", "yield a", "yield a, b", "yield a, b, "] {
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
    fn test_raise_statement() {
        for test_case in &["raise", "raise a", "raise a from c"] {
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
    fn test_parse_break_continue() {
        for test_case in &["break", "continue"] {
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
    fn test_parse_import_statement() {
        for test_case in &[
            "import a",
            "import a as b",
            "import a.b",
            "import a.b as c",
            "import a.b.c",
            "from a import b",
            "from a import b as c",
            "from a.b import c",
            "from a.b import c as d",
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
        for test_case in &[
            "[a, b, c]",
            "[a,
            b, c]",
            "[a
            , b, c]",
            "[a,
            b,
                c]",
            "[a,
            ]",
            "[a, b, c,]",
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
        // do the same here
        for test_case in &[
            "{a: b, c: d}",
            "{a: b,
            c: d}",
            "{a: b
            , c: d}",
            "{a: b,
            c: d,
                e: f}",
            "{a: b,
            }",
            "{a: b, c: d,}",
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
    fn test_set() {
        for test_case in &[
            "{a, b, c}",
            "{a,
            b, c}",
            "{a
            , b, c}",
            "{a,
            b,
                c}",
            "{a,
            }",
            "{a, b, c,}",
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
        for test_case in &["(*a)"] {
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
    fn test_subscript() {
        for test_case in &[
            "a[b]",
            "a[b:c]",
            "a[b:c:d]",
            "a[b, c, d]",
            "a[b, c: d, e]",
            "a[::]",
            "a[b, c:d:e, f]",
            "a[::d,]",
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
    fn test_attribute_ref() {
        for test_case in &["a.b", "a.b.c", "a.b_c", "a.b.c.d"] {
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
    fn test_generator_expression() {
        for test_case in &[
            "(a for a in b)",
            "(a for a in b if c)",
            "(a for a in b if c if d)",
            "(a for a in b for c in d)",
            "(ord(c) for line in file for c in line)",
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
    fn test_conditional_expression() {
        for test_case in &[
            // "a if b else c",
            "a if b else c if d else e",
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
    fn test_string_literal_concatnation() {
        for test_case in &[
            "'a' 'b'",
            "b'a' b'b'",
            "'a'   'b'",
            "r'a' 'b'",
            "b'a' 'b'",
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
    fn test_fstring() {
        for test_case in &[
            "f'a'",
            "f'hello_{a}'",
            "f'hello_{a} {b}'",
            "f'hello_{a} {b} {c}'",
            // unsupported
            // "f'hello_{f'''{a}'''}'",
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
    fn test_if() {
        for test_case in &[
            "if a: pass",
            "if a:
    pass",
            "if a:
        a = 1
if a:
        b = 1

",
            "if a:
    pass;pass",
            "if a is b:
            pass",
            "if a is b:
                pass
elif a is c:
                pass",
            "if a is b:
                pass
elif a is c:
                pass
else:
                pass
",
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
    fn test_for_statement() {
        for test_case in &[
            "for a in b: pass",
            "for a in b:
    pass",
            "for a in range(10):
    a = 1
else:
    b = 1",
            "for a in range(10), range(10):
    a = 1
",
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
    fn test_with_statement() {
        for test_case in &[
            "with a: pass",
            "with a as b: pass",
            "with a as b, c as d: pass",
            "with (a as b, c as d): pass",
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
