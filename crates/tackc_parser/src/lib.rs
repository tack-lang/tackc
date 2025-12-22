pub mod error;
use std::num::NonZeroU64;

use error::{ParseError, Result};

pub mod ast;

use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::{Span, SpanValue};

use crate::ast::{Expression, ExpressionKind};

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
    global: &'src Global,
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
            global,
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

    fn report_error<T>(&mut self, err: Result<T>) -> Option<T> {
        err.map_err(|e| self.errors.push(e)).ok()
    }

    fn handle_error_sync<T>(
        &mut self,
        err: Result<T>,
        snapshot: ParserSnapshot,
        cancel: TokenKind,
    ) -> Option<T> {
        if err.is_err() {
            self.restore(snapshot);
            self.synchronize(cancel);
        }
        self.report_error(err)
    }

    fn synchronize(&mut self, cancel: TokenKind) {
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
                kind if kind == cancel && depth == 0 => {
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

    /// Parses an expression from `lexer`.
    ///
    /// # Errors
    /// Returns an error if it failed to parse an expression.
    pub fn parse(tokens: &'src [Token], file: &'src F, global: &'src Global) -> (Result<Expression>, Vec<ParseError>) {
        let mut p = Parser::new(tokens, file, global);
        (p.expression(), p.errors)
    }
}

impl<F: File> Parser<'_, F> {
    fn expression(&mut self) -> Result<Expression> {
        self.grouping()
    }

    fn grouping(&mut self) -> Result<Expression> {
        let Some(opening) = self.eat(&[TokenKind::LParen]) else {
            return self.primary();
        };

        let snapshot = self.snapshot();
        let inner_result = self.expression();
        let inner = self.handle_error_sync(inner_result, snapshot, TokenKind::RParen);
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
