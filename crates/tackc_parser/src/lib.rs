pub mod error;
use std::num::NonZeroU64;

use error::{ParseError, Result};

pub mod ast;

use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::{Span, SpanValue};

use crate::ast::{BinOp, Expression, ExpressionKind, UnOp};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct NodeId {
    id: NonZeroU64,
    file: NonZeroU64,
}

#[derive(Debug, Clone, Copy)]
struct ParserSnapshot {
    ptr: usize,
    errors: usize,
    open: NonZeroU64,
}

pub struct Parser<'src, F> {
    file: &'src F,
    _global: &'src Global,
    tokens: &'src [Token],
    ptr: usize,
    errors: Vec<ParseError>,

    open: NonZeroU64,
    spans: Vec<Span>,
}

impl<'src, F: File> Parser<'src, F> {
    #[allow(clippy::missing_panics_doc)]
    pub const fn new(tokens: &'src [Token], file: &'src F, global: &'src Global) -> Self {
        Parser {
            file,
            _global: global,
            tokens,
            ptr: 0,
            errors: Vec::new(),

            open: NonZeroU64::new(1).unwrap(),
            spans: Vec::new(),
        }
    }

    const fn snapshot(&self) -> ParserSnapshot {
        ParserSnapshot {
            ptr: self.ptr,
            errors: self.errors.len(),
            open: self.open,
        }
    }

    fn restore(&mut self, snapshot: ParserSnapshot) {
        let ParserSnapshot { ptr, errors, open } = snapshot;

        self.ptr = ptr;
        self.errors.truncate(errors);
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

    fn expect(&mut self, kinds: &[TokenKind], expected: Option<&'static str>) -> Result<Token> {
        self.peek()
            .ok_or_else(|| ParseError::eof(expected))
            .and_then(|token| {
                if kinds.contains(&token.kind) {
                    self.advance();
                    Ok(token)
                } else {
                    Err(ParseError::expected(expected, token))
                }
            })
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
        self.errors.push(e);
    }

    fn report_error<T>(&mut self, err: Result<T>) -> Option<T> {
        err.map_err(|e| self.push_err(e)).ok()
    }

    fn handle_error_sync<T>(
        &mut self,
        err: Result<T>,
        snapshot: ParserSnapshot,
        cancel: &[TokenKind],
    ) -> Option<T> {
        if err.is_err() {
            self.restore(snapshot);
            self.synchronize(cancel);
        }
        self.report_error(err)
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

    /// Parses an expression from `lexer`.
    ///
    /// # Errors
    /// Returns an error if it failed to parse an expression.
    pub fn parse(
        tokens: &'src [Token],
        file: &'src F,
        global: &'src Global,
    ) -> (Option<Expression>, Vec<ParseError>) {
        let mut p = Parser::new(tokens, file, global);
        let res = p.expression();
        (p.report_error(res), p.errors)
    }
}

impl<F: File> Parser<'_, F> {
    fn delimited<T>(
        &mut self,
        seperator: TokenKind,
        closing: TokenKind,
        parse: fn(&mut Self) -> Result<T>,
    ) -> Vec<Option<T>> {
        let mut args = Vec::new();
        loop {
            if let Some(tok) = self.peek()
                && tok.kind == closing
            {
                break;
            }

            let snapshot = self.snapshot();
            let expr_res = parse(self);
            let expr = self.handle_error_sync(expr_res, snapshot, &[closing, seperator]);
            args.push(expr);
            if self.eat(&[seperator]).is_none() {
                break;
            }
        }

        args
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
        let expr = self.handle_error_sync(expr_res, snapshot, &[TokenKind::RBracket]);
        let closing_res = self.expect(&[TokenKind::RBracket], Some("']'"));
        let closing = self.report_error(closing_res);
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
        let args = self.delimited(TokenKind::Comma, TokenKind::RParen, Self::expression);
        let closing_res = self.expect(&[TokenKind::RParen], Some("')'"));
        let closing = self.report_error(closing_res);
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
        let ident_res = self.expect(&[TokenKind::Ident], Some("identifier"));
        let ident = self.report_error(ident_res);
        let span = Span::new_from(
            self.span(lhs.id).start,
            ident.map_or_else(|| self.loc(), |tok| tok.span.end),
        );
        Expression::new(
            ExpressionKind::Member(Box::new(lhs), ident.map(Into::into)),
            self.prepare_node(span),
        )
    }

    fn grouping(&mut self) -> Result<Expression> {
        let Some(opening) = self.eat(&[TokenKind::LParen]) else {
            return self.primary();
        };

        let snapshot = self.snapshot();
        let inner_result = self.expression();
        let inner = self.handle_error_sync(inner_result, snapshot, &[TokenKind::RParen]);
        let closing_res = self.expect(&[TokenKind::RParen], Some("')'"));
        let closing = self.report_error(closing_res);

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
        let tok = self.expect(
            &[TokenKind::IntLit, TokenKind::FloatLit, TokenKind::Ident],
            None,
        )?;
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
