pub mod error;
use std::num::NonZeroU32;

use error::{ParseError, Result};

pub mod ast;

use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::{Span, SpanValue};
use thin_vec::ThinVec;

use crate::{
    ast::{
        AssignmentStatement, BinOp, ConstItem, Expression, ExpressionKind, ExpressionStatement,
        Item, ItemKind, LetStatement, Statement, StatementKind, UnOp,
    },
    error::ErrorExt,
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct NodeId {
    id: NonZeroU32,
    file: NonZeroU32,
}

#[derive(Debug, Clone, Copy)]
struct ParserSnapshot {
    ptr: usize,
    open: NonZeroU32,
    failed: bool,
}

pub struct Parser<'src, F> {
    file: &'src F,
    _global: &'src Global,
    tokens: &'src [Token],
    ptr: usize,
    errors: Vec<ParseError>,
    failed: bool,

    open: NonZeroU32,
    spans: Vec<Span>,
}

impl<'src, F: File> Parser<'src, F> {
    #[allow(clippy::missing_panics_doc)]
    pub const fn new(tokens: &'src [Token], file: &'src F, global: &'src Global) -> Self {
        Parser {
            failed: false,
            file,
            _global: global,
            tokens,
            ptr: 0,
            errors: Vec::new(),

            open: NonZeroU32::new(1).unwrap(),
            spans: Vec::new(),
        }
    }

    const fn check_failed(&self) -> Result<()> {
        if self.failed {
            Err(ParseError::failed())
        } else {
            Ok(())
        }
    }

    const fn snapshot(&self) -> ParserSnapshot {
        ParserSnapshot {
            ptr: self.ptr,
            open: self.open,
            failed: self.failed,
        }
    }

    fn restore(&mut self, snapshot: ParserSnapshot) {
        let ParserSnapshot { ptr, open, failed } = snapshot;
        if failed {
            return;
        }

        self.ptr = ptr;
        self.open = open;
        #[allow(clippy::cast_possible_truncation)]
        self.spans.truncate(open.get() as usize - 1);
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.ptr).copied()
    }

    fn eat(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        self.peek()
            .filter(|tok| kinds.contains(&tok.kind))
            .inspect(|_| {
                self.advance();
            })
    }

    fn expect(&mut self, kinds: &[TokenKind]) -> Result<Token> {
        self.expect_peek(kinds).inspect(|_| {
            self.advance();
        })
    }

    fn expect_peek(&self, kinds: &[TokenKind]) -> Result<Token> {
        self.expect_peek_all().and_then(|token| {
            if kinds.contains(&token.kind) {
                Ok(token)
            } else {
                Err(ParseError::expected(None, token))
            }
        })
    }

    fn expect_peek_all(&self) -> Result<Token> {
        self.peek().ok_or_else(|| ParseError::eof(None))
    }

    fn advance(&mut self) -> Option<Token> {
        let tok = self.peek();
        self.ptr += 1;
        tok
    }

    fn prepare_node(&mut self, span: Span) -> NodeId {
        self.spans.push(span);
        let open = NodeId {
            id: self.open,
            file: self.file.id(),
        };
        self.open = self.open.checked_add(1).expect("NodeId limit reached!");
        open
    }

    fn push_err(&mut self, e: ParseError) {
        if self.errors.len() > 100 {
            if !self.failed {
                self.failed = true;
                self.errors.push(ParseError::error_limit());
            }
            return;
        }
        self.errors.push(e);
    }

    fn report_error<T>(&mut self, err: Result<T>, expected: &'static str) -> Option<T> {
        err.set_expected(expected)
            .map_err(|e| self.push_err(e))
            .ok()
    }

    fn handle_error_sync<T>(
        &mut self,
        err: Result<T>,
        snapshot: ParserSnapshot,
        cancel: &[TokenKind],
        expected: &'static str,
    ) -> Option<T> {
        if err.is_err() {
            self.restore(snapshot);
            self.synchronize(cancel);
        }
        self.report_error(err, expected)
    }

    fn synchronize(&mut self, cancel: &[TokenKind]) {
        let mut depth: u32 = 0;

        loop {
            let Some(tok) = self.peek() else {
                return;
            };
            match tok.kind {
                TokenKind::LBrace | TokenKind::LBracket | TokenKind::LParen => {
                    depth += 1;
                }
                TokenKind::RBrace | TokenKind::RBracket | TokenKind::RParen => {
                    depth = depth.saturating_sub(1);
                }
                kind if cancel.contains(&kind) && depth == 0 => {
                    return;
                }
                _ => {}
            }
            self.advance();
        }
    }

    fn loc(&self) -> SpanValue {
        self.peek()
            .map_or_else(|| Span::eof(self.file).end, |tok| tok.span.start)
    }

    fn span(&self, id: NodeId) -> Span {
        #[allow(clippy::cast_possible_truncation)]
        self.spans[id.id.get() as usize - 1]
    }

    fn expect_report(&mut self, kinds: &[TokenKind], expected: &'static str) -> Option<Token> {
        let tok_res = self.expect(kinds);
        self.report_error(tok_res, expected)
    }

    fn parse_sync<T, P: FnOnce(&mut Self) -> Result<T>>(
        &mut self,
        func: P,
        cancel: &[TokenKind],
        expected: &'static str,
    ) -> Option<T> {
        let snapshot = self.snapshot();
        let res = func(self);
        self.handle_error_sync(res, snapshot, cancel, expected)
    }

    /// Parses an expression from `lexer`.
    ///
    /// # Errors
    /// Returns an error if it failed to parse an expression.
    pub fn parse(
        tokens: &'src [Token],
        file: &'src F,
        global: &'src Global,
    ) -> (Option<Statement>, Vec<ParseError>) {
        let mut p = Parser::new(tokens, file, global);
        let res = p.statement();
        (p.report_error(res, "statement"), p.errors)
    }
}

impl<F: File> Parser<'_, F> {
    fn delimited<T>(
        &mut self,
        seperator: TokenKind,
        closing: TokenKind,
        parse: fn(&mut Self) -> Result<T>,
        expected: &'static str,
    ) -> ThinVec<Option<T>> {
        let mut args = ThinVec::new();
        loop {
            if let Some(tok) = self.peek()
                && tok.kind == closing
            {
                break;
            }

            let snapshot = self.snapshot();
            let expr_res = parse(self);
            let expr = self.handle_error_sync(expr_res, snapshot, &[closing, seperator], expected);
            args.push(expr);
            if self.eat(&[seperator]).is_none() {
                break;
            }
        }

        args
    }

    fn item(&mut self) -> Result<Item> {
        self.check_failed()?;
        let tok = self.expect_peek(&[TokenKind::Const])?;
        match tok.kind {
            TokenKind::Const => self.const_item(),
            _ => Err(ParseError::expected(None, tok)),
        }
    }

    fn const_item(&mut self) -> Result<Item> {
        self.check_failed()?;
        let const_key = self.expect(&[TokenKind::Const])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let ty = if self.eat(&[TokenKind::Colon]).is_some() {
            Some(self.parse_sync(Self::expression, &[TokenKind::Eq], "type"))
        } else {
            None
        };

        let _eq = self.expect_report(&[TokenKind::Eq], "'='");
        let expr = self.parse_sync(Self::expression, &[TokenKind::Semicolon], "expression");

        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            const_key.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
        );

        Ok(Item::new(
            ItemKind::ConstItem(Box::new(ConstItem {
                expr,
                ty,
                ident: ident.map(Into::into),
            })),
            self.prepare_node(span),
        ))
    }

    fn statement(&mut self) -> Result<Statement> {
        self.check_failed()?;
        let tok = self.expect_peek_all()?;
        match tok.kind {
            TokenKind::Let => self.let_statement(),
            TokenKind::Const => self.item_statement(),
            _ => self.statement_starting_with_expression(),
        }
    }

    fn let_statement(&mut self) -> Result<Statement> {
        self.check_failed()?;
        let let_key = self.expect(&[TokenKind::Let])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let ty = if self.eat(&[TokenKind::Colon]).is_some() {
            Some(self.parse_sync(Self::expression, &[TokenKind::Eq], "type"))
        } else {
            None
        };

        let expr = if self.eat(&[TokenKind::Eq]).is_some() {
            Some(self.parse_sync(Self::expression, &[TokenKind::Semicolon], "expression"))
        } else {
            None
        };

        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            let_key.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
        );

        Ok(Statement::new(
            StatementKind::LetStatement(Box::new(LetStatement {
                expr,
                ty,
                ident: ident.map(Into::into),
            })),
            self.prepare_node(span),
        ))
    }

    fn item_statement(&mut self) -> Result<Statement> {
        self.check_failed()?;
        let item = self.item()?;
        let span = self.span(item.id);
        Ok(Statement::new(
            StatementKind::Item(item),
            self.prepare_node(span),
        ))
    }

    fn statement_starting_with_expression(&mut self) -> Result<Statement> {
        self.check_failed()?;
        let expr = self.expression()?;
        match self.peek().map(|tok| tok.kind) {
            Some(TokenKind::Eq) => self.assignment_statement(expr),
            _ => self.expression_statement(expr),
        }
    }

    fn expression_statement(&mut self, expr: Expression) -> Result<Statement> {
        self.check_failed()?;
        let semi = if expr.kind.is_block() {
            self.eat(&[TokenKind::Semicolon])
        } else {
            self.expect_report(&[TokenKind::Semicolon], "';'")
        };
        let span = semi.map_or_else(
            || self.span(expr.id),
            |tok| Span::new_from(self.span(expr.id).start, tok.span.end),
        );
        Ok(Statement::new(
            StatementKind::ExpressionStatement(Box::new(ExpressionStatement { expr, semi })),
            self.prepare_node(span),
        ))
    }

    fn assignment_statement(&mut self, lhs: Expression) -> Result<Statement> {
        self.check_failed()?;
        let _eq = self.expect(&[TokenKind::Eq])?;
        let rhs = self.parse_sync(Self::expression, &[TokenKind::Semicolon], "expression");
        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            self.span(lhs.id).start,
            semi.map_or_else(|| self.loc(), |tok| tok.span.end),
        );
        Ok(Statement::new(
            StatementKind::AssignmentStatement(Box::new(AssignmentStatement { lhs, rhs })),
            self.prepare_node(span),
        ))
    }

    fn expression(&mut self) -> Result<Expression> {
        self.comparison()
    }

    fn comparison(&mut self) -> Result<Expression> {
        self.binary_expr(
            &[
                (TokenKind::Gt, BinOp::Gt),
                (TokenKind::Lt, BinOp::Lt),
                (TokenKind::GtEq, BinOp::GtEq),
                (TokenKind::LtEq, BinOp::LtEq),
                (TokenKind::EqEq, BinOp::Eq),
                (TokenKind::BangEq, BinOp::NotEq),
            ],
            Self::term,
            true,
        )
    }

    fn term(&mut self) -> Result<Expression> {
        self.binary_expr(
            &[
                (TokenKind::Plus, BinOp::Add),
                (TokenKind::Minus, BinOp::Sub),
            ],
            Self::factor,
            false,
        )
    }

    fn factor(&mut self) -> Result<Expression> {
        self.binary_expr(
            &[
                (TokenKind::Star, BinOp::Mul),
                (TokenKind::Slash, BinOp::Div),
            ],
            Self::unary,
            false,
        )
    }

    #[inline]
    fn binary_expr(
        &mut self,
        tokens: &[(TokenKind, BinOp)],
        next: fn(&mut Self) -> Result<Expression>,
        comparison: bool,
    ) -> Result<Expression> {
        self.check_failed()?;
        let mut lhs = next(self)?;
        let mut ops = Vec::new();
        while let Some(peeked) = self.peek() {
            let Some((_, op)) = tokens.iter().find(|(tok, _)| peeked.kind == *tok) else {
                break;
            };

            self.advance(); // Skip operator
            let rhs = next(self)?;
            let id = self.prepare_node(Span::new_from(
                self.span(lhs.id).start,
                self.span(rhs.id).end,
            ));
            lhs = Expression::new(
                ExpressionKind::Binary(*op, Box::new(lhs), Box::new(rhs)),
                id,
            );

            if comparison
                && let Some(peeked2) = self.peek()
                && let Some((_, _)) = tokens.iter().find(|(tok, _)| peeked2.kind == *tok)
            {
                ops.push(peeked);
                ops.push(peeked2);
            }
        }
        if !ops.is_empty() {
            self.push_err(ParseError::other(
                "comparison operators cannot be chained",
                ops,
            ));
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> Result<Expression> {
        self.check_failed()?;
        let Some(op) = self.eat(&[TokenKind::Minus, TokenKind::Bang]) else {
            return self.postfix();
        };
        let rhs = self.unary()?;
        let span = Span::new_from(op.span.start, self.span(rhs.id).end);
        let kind = match op.kind {
            TokenKind::Minus => ExpressionKind::Unary(UnOp::Neg, Box::new(rhs)),
            TokenKind::Bang => ExpressionKind::Unary(UnOp::Not, Box::new(rhs)),
            _ => unreachable!(),
        };
        Ok(Expression::new(kind, self.prepare_node(span)))
    }

    fn postfix(&mut self) -> Result<Expression> {
        self.check_failed()?;
        let mut lhs = self.grouping()?;
        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Dot => lhs = self.parse_access(lhs),
                TokenKind::LParen => lhs = self.parse_call(lhs),
                TokenKind::LBracket => lhs = self.parse_index(lhs),
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_index(&mut self, lhs: Expression) -> Expression {
        self.advance();
        let snapshot = self.snapshot();
        let expr_res = self.expression();
        let expr = self.handle_error_sync(expr_res, snapshot, &[TokenKind::RBracket], "expression");
        let closing_res = self.expect(&[TokenKind::RBracket]);
        let closing = self.report_error(closing_res, "']'");
        let span = Span::new_from(
            self.span(lhs.id).start,
            closing.map_or_else(|| self.loc(), |tok| tok.span.end),
        );

        Expression::new(
            ExpressionKind::Index(Box::new(lhs), expr.map(Box::new)),
            self.prepare_node(span),
        )
    }

    fn parse_call(&mut self, lhs: Expression) -> Expression {
        self.advance();
        let args = self.delimited(
            TokenKind::Comma,
            TokenKind::RParen,
            Self::expression,
            "expression",
        );
        let closing_res = self.expect(&[TokenKind::RParen]);
        let closing = self.report_error(closing_res, "')'");
        let span = Span::new_from(
            self.span(lhs.id).start,
            closing.map_or_else(|| self.loc(), |tok| tok.span.end),
        );
        Expression::new(
            ExpressionKind::Call(Box::new(lhs), args),
            self.prepare_node(span),
        )
    }

    fn parse_access(&mut self, lhs: Expression) -> Expression {
        self.advance();
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let span = Span::new_from(
            self.span(lhs.id).start,
            ident.map_or_else(|| self.loc(), |tok| tok.span.end),
        );
        Expression::new(
            ExpressionKind::Member(Box::new(lhs), ident.map(Into::into).map(Box::new)),
            self.prepare_node(span),
        )
    }

    fn grouping(&mut self) -> Result<Expression> {
        self.check_failed()?;
        let Some(opening) = self.eat(&[TokenKind::LParen]) else {
            return self.primary();
        };

        let inner = self.parse_sync(Self::expression, &[TokenKind::RParen], "expression");
        let closing = self.expect_report(&[TokenKind::RParen], "')'");

        let expr = Expression::new(
            ExpressionKind::Grouping(inner.map(Box::new)),
            self.prepare_node(Span::new_from(
                opening.span.start,
                closing.map_or_else(|| self.loc(), |tok| tok.span.end),
            )),
        );
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expression> {
        self.check_failed()?;
        let tok = self.expect(&[TokenKind::IntLit, TokenKind::FloatLit, TokenKind::Ident])?;
        let primary = match tok.kind {
            TokenKind::IntLit => ExpressionKind::IntLit(tok.lexeme),
            TokenKind::FloatLit => ExpressionKind::FloatLit(tok.lexeme),
            TokenKind::Ident => ExpressionKind::Ident(tok.lexeme),
            _ => unreachable!(),
        };
        let expr = Expression::new(primary, self.prepare_node(tok.span));

        Ok(expr)
    }
}
