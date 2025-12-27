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
        AssignmentStatement, BinOp, Block, ConstItem, Expression, ExpressionKind,
        ExpressionStatement, FuncItem, ImpItem, Item, ItemKind, LetStatement, ModStatement, Path,
        Program, Statement, StatementKind, Symbol, UnOp,
    },
    error::ErrorExt,
};

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub enum BlockMode {
    NoBlocks,
    Normal,
}

impl BlockMode {
    pub fn normal(self) -> bool {
        self == Self::Normal
    }
}

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

    fn peek2(&self) -> Option<Token> {
        self.tokens.get(self.ptr + 1).copied()
    }

    fn at_eof(&self) -> bool {
        self.peek().is_none()
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

    fn expect_peek2(&self, kinds: &[TokenKind]) -> Result<Token> {
        self.expect_peek2_all().and_then(|token| {
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

    fn expect_peek2_all(&self) -> Result<Token> {
        self.peek2().ok_or_else(|| ParseError::eof(None))
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
        skip: bool,
    ) -> Option<T> {
        if err.is_err() {
            self.restore(snapshot);
            if skip {
                self.eat(cancel);
            }
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

    fn synchronize_skip_next_block(&mut self) {
        self.synchronize(&[TokenKind::LBrace]);
        self.advance();
        self.synchronize(&[TokenKind::RBrace]);
        self.advance();
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
        self.handle_error_sync(res, snapshot, cancel, expected, false)
    }

    fn parse_sync_skip<T, P: FnOnce(&mut Self) -> Result<T>>(
        &mut self,
        func: P,
        cancel: &[TokenKind],
        expected: &'static str,
    ) -> Option<T> {
        let snapshot = self.snapshot();
        let res = func(self);
        self.handle_error_sync(res, snapshot, cancel, expected, true)
    }

    fn parse_report<T, P: FnOnce(&mut Self) -> Result<T>>(
        &mut self,
        func: P,
        expected: &'static str,
    ) -> Option<T> {
        let res = func(self);
        self.report_error(res, expected)
    }

    /// Parses an program from `tokens` and `file`, and returns all errors.
    pub fn parse(
        tokens: &'src [Token],
        file: &'src F,
        global: &'src Global,
    ) -> (Program, Vec<ParseError>) {
        let mut p = Parser::new(tokens, file, global);
        let prog = p.program();
        (prog, p.errors)
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

            let expr = self.parse_sync(parse, &[closing, seperator], expected);
            args.push(expr);
            if self.eat(&[seperator]).is_none() {
                break;
            }
        }

        args
    }

    fn visibility(&mut self) -> bool {
        self.eat(&[TokenKind::Exp]).is_some()
    }

    fn program(&mut self) -> Program {
        let mod_stmt_res = self.mod_statement();
        let mod_stmt = self.report_error(mod_stmt_res, "`mod` statement");

        let mut items = ThinVec::new();
        while !self.at_eof() {
            let item =
                self.parse_sync_skip(Self::item, &[TokenKind::Const, TokenKind::Func], "item");
            items.push(item);
        }

        Program { mod_stmt, items }
    }

    fn mod_statement(&mut self) -> Result<ModStatement> {
        let mod_key = self.expect(&[TokenKind::Mod])?;
        let path = self.parse_sync(Self::path, &[TokenKind::Semicolon], "path");
        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            mod_key.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
        );
        Ok(ModStatement {
            path,
            id: self.prepare_node(span),
        })
    }

    fn path(&mut self) -> Result<Path> {
        let mut components = ThinVec::new();
        let ident = self.expect(&[TokenKind::Ident])?;
        components.push(Some(ident.into()));
        while self.eat(&[TokenKind::Dot]).is_some() {
            let ident = self.expect_report(&[TokenKind::Ident], "identifier");
            components.push(ident.map(Into::into));
        }
        let span = Span::new_from(
            ident.span.start,
            components
                .last()
                .unwrap()
                .map_or_else(|| self.loc(), |sym: Symbol| sym.1.end),
        );
        Ok(Path {
            components,
            id: self.prepare_node(span),
        })
    }

    fn item(&mut self) -> Result<Item> {
        self.check_failed()?;
        let starts = &[TokenKind::Const, TokenKind::Func, TokenKind::Imp];
        let tok = if self.expect_peek(&[TokenKind::Exp]).is_ok() {
            self.expect_peek2(starts)
        } else {
            self.expect_peek(starts)
        }?;
        match tok.kind {
            TokenKind::Const => self.const_item(),
            TokenKind::Func => self.func_item(),
            TokenKind::Imp => self.imp_item(),
            _ => Err(ParseError::expected(None, tok)),
        }
    }

    fn const_item(&mut self) -> Result<Item> {
        self.check_failed()?;

        let exported = self.visibility();
        let const_key = self.expect(&[TokenKind::Const])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let ty = if self.eat(&[TokenKind::Colon]).is_some() {
            Some(self.parse_sync(Self::expression_normal, &[TokenKind::Eq], "type"))
        } else {
            None
        };

        let _eq = self.expect_report(&[TokenKind::Eq], "'='");
        let expr = self.parse_sync(
            Self::expression_normal,
            &[TokenKind::Semicolon],
            "expression",
        );

        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            const_key.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
        );

        Ok(Item::new(
            ItemKind::ConstItem(Box::new(ConstItem {
                exported,
                expr,
                ty,
                ident: ident.map(Into::into),
            })),
            self.prepare_node(span),
        ))
    }

    fn func_item(&mut self) -> Result<Item> {
        self.check_failed()?;

        let exported = self.visibility();
        let func = self.expect(&[TokenKind::Func])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let _opening = self.expect_report(&[TokenKind::LParen], "'('");

        let mut params = Vec::new();
        loop {
            if let Some(tok) = self.peek()
                && tok.kind == TokenKind::RParen
            {
                break;
            }

            let ident = self.expect_report(&[TokenKind::Ident], "identifier");
            let _colon = self.expect_report(&[TokenKind::Colon], "':'");
            let expr = self.parse_sync(
                Self::expression_normal,
                &[TokenKind::Comma, TokenKind::RParen],
                "expression",
            );
            params.push((ident.map(Into::into), expr));
            if self.eat(&[TokenKind::Comma]).is_none() {
                break;
            }
        }

        let _closing = self.expect_report(&[TokenKind::RParen], "')'");
        let ret_type = if self
            .peek()
            .filter(|tok| tok.kind != TokenKind::LBrace)
            .is_some()
        {
            Some(self.parse_sync(Self::expression_no_blocks, &[TokenKind::LBrace], "type"))
        } else {
            None
        };
        let block = self.parse_report(Self::block, "block");

        let span = Span::new_from(
            func.span.start,
            block
                .as_ref()
                .map_or_else(|| self.loc(), |block| self.span(block.id).end),
        );

        Ok(Item {
            kind: ItemKind::FuncItem(Box::new(FuncItem {
                exported,
                ident: ident.map(Into::into),
                params,
                ret_type,
                block,
            })),
            id: self.prepare_node(span),
        })
    }

    fn imp_item(&mut self) -> Result<Item> {
        let exported = self.visibility();
        let imp = self.expect(&[TokenKind::Imp])?;
        let path = self.parse_sync(Self::path, &[TokenKind::Semicolon], "path");
        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            imp.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
        );
        Ok(Item {
            kind: ItemKind::ImpItem(Box::new(ImpItem { exported, path })),
            id: self.prepare_node(span),
        })
    }

    fn block(&mut self) -> Result<Block> {
        self.check_failed()?;
        let opening = self.expect(&[TokenKind::LBrace])?;
        let mut stmts = Vec::new();
        let expr = loop {
            if let Some(tok) = self.peek()
                && tok.kind == TokenKind::RBrace
            {
                break None;
            }

            match self.peek().map(|tok| tok.kind) {
                // Statements that end in semicolons
                Some(TokenKind::Let | TokenKind::Const) => {
                    let stmt = self.parse_sync(
                        Self::statement,
                        &[TokenKind::Semicolon],
                        "statement, item, or expression",
                    );
                    stmts.push(stmt);
                }
                // Statements that don't end in semicolons
                Some(TokenKind::Func) => {
                    let snapshot = self.snapshot();
                    let stmt_res = self.statement();
                    let stmt = self.report_error(stmt_res, "statement, item, or expression");
                    if stmt.is_none() {
                        self.restore(snapshot);
                        self.synchronize_skip_next_block();
                    }
                    stmts.push(stmt);
                }
                // Expression that end in semicolons when used as statements
                Some(_) => {
                    let loc = self.loc();
                    let expr = self.parse_sync(
                        Self::expression_normal,
                        &[TokenKind::Semicolon, TokenKind::RBrace],
                        "statement, item, or expression",
                    );
                    if let Some(tok) = self.peek()
                        && tok.kind == TokenKind::RBrace
                    {
                        break Some(expr);
                    }

                    let semi = self.expect_report(&[TokenKind::Semicolon], "';'");
                    let span = Span::new_from(
                        expr.as_ref().map_or(loc, |expr| self.span(expr.id).start),
                        semi.map_or_else(|| self.loc(), |semi| semi.span.end),
                    );
                    stmts.push(expr.map(|expr| {
                        Statement::new(
                            StatementKind::ExpressionStatement(Box::new(ExpressionStatement {
                                expr,
                                semi: Some(semi),
                            })),
                            self.prepare_node(span),
                        )
                    }));
                }
                None => {
                    self.push_err(ParseError::eof(Some("statement, item, or expression")));
                    break None;
                }
            }
        };

        let closing = self.expect_report(&[TokenKind::RBrace], "'}'");
        let span = Span::new_from(
            opening.span.start,
            closing.map_or_else(|| self.loc(), |tok| tok.span.end),
        );

        Ok(Block {
            stmts,
            expr,
            id: self.prepare_node(span),
        })
    }

    fn statement(&mut self) -> Result<Statement> {
        self.check_failed()?;
        let tok = self.expect_peek_all()?;
        match tok.kind {
            TokenKind::Let => self.let_statement(),
            TokenKind::Const | TokenKind::Func => self.item_statement(),
            _ => self.statement_starting_with_expression(),
        }
    }

    fn let_statement(&mut self) -> Result<Statement> {
        self.check_failed()?;
        let let_key = self.expect(&[TokenKind::Let])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let ty = if self.eat(&[TokenKind::Colon]).is_some() {
            Some(self.parse_sync(Self::expression_normal, &[TokenKind::Eq], "type"))
        } else {
            None
        };

        let expr = if self.eat(&[TokenKind::Eq]).is_some() {
            Some(self.parse_sync(
                Self::expression_normal,
                &[TokenKind::Semicolon],
                "expression",
            ))
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
        let expr = self.expression(BlockMode::Normal)?;
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
            StatementKind::ExpressionStatement(Box::new(ExpressionStatement {
                expr,
                semi: semi.is_some().then_some(semi),
            })),
            self.prepare_node(span),
        ))
    }

    fn assignment_statement(&mut self, lhs: Expression) -> Result<Statement> {
        self.check_failed()?;
        let _eq = self.expect(&[TokenKind::Eq])?;
        let rhs = self.parse_sync(
            Self::expression_normal,
            &[TokenKind::Semicolon],
            "expression",
        );
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

    #[inline]
    fn expression_normal(&mut self) -> Result<Expression> {
        self.expression(BlockMode::Normal)
    }

    #[inline]
    fn expression_no_blocks(&mut self) -> Result<Expression> {
        self.expression(BlockMode::NoBlocks)
    }

    fn expression(&mut self, mode: BlockMode) -> Result<Expression> {
        self.comparison(mode)
    }

    fn comparison(&mut self, mode: BlockMode) -> Result<Expression> {
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
            mode,
        )
    }

    fn term(&mut self, mode: BlockMode) -> Result<Expression> {
        self.binary_expr(
            &[
                (TokenKind::Plus, BinOp::Add),
                (TokenKind::Minus, BinOp::Sub),
            ],
            Self::factor,
            false,
            mode,
        )
    }

    fn factor(&mut self, mode: BlockMode) -> Result<Expression> {
        self.binary_expr(
            &[
                (TokenKind::Star, BinOp::Mul),
                (TokenKind::Slash, BinOp::Div),
            ],
            Self::unary,
            false,
            mode,
        )
    }

    #[inline]
    fn binary_expr(
        &mut self,
        tokens: &[(TokenKind, BinOp)],
        next: fn(&mut Self, BlockMode) -> Result<Expression>,
        comparison: bool,
        mode: BlockMode,
    ) -> Result<Expression> {
        self.check_failed()?;
        let mut lhs = next(self, mode)?;
        let mut ops = Vec::new();
        while let Some(peeked) = self.peek() {
            let Some((_, op)) = tokens.iter().find(|(tok, _)| peeked.kind == *tok) else {
                break;
            };

            self.advance(); // Skip operator
            let rhs = next(self, mode)?;
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

    fn unary(&mut self, mode: BlockMode) -> Result<Expression> {
        self.check_failed()?;
        let Some(op) = self.eat(&[TokenKind::Minus, TokenKind::Bang]) else {
            return self.postfix(mode);
        };
        let rhs = self.unary(mode)?;
        let span = Span::new_from(op.span.start, self.span(rhs.id).end);
        let kind = match op.kind {
            TokenKind::Minus => ExpressionKind::Unary(UnOp::Neg, Box::new(rhs)),
            TokenKind::Bang => ExpressionKind::Unary(UnOp::Not, Box::new(rhs)),
            _ => unreachable!(),
        };
        Ok(Expression::new(kind, self.prepare_node(span)))
    }

    fn postfix(&mut self, mode: BlockMode) -> Result<Expression> {
        self.check_failed()?;
        let mut lhs = self.grouping(mode)?;
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
        let expr_res = self.expression(BlockMode::Normal);
        let expr = self.handle_error_sync(
            expr_res,
            snapshot,
            &[TokenKind::RBracket],
            "expression",
            false,
        );
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
            Self::expression_normal,
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

    fn grouping(&mut self, mode: BlockMode) -> Result<Expression> {
        self.check_failed()?;
        let Some(opening) = self.eat(&[TokenKind::LParen]) else {
            return self.block_expr(mode);
        };

        let inner = self.parse_sync(Self::expression_normal, &[TokenKind::RParen], "expression");
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

    fn block_expr(&mut self, mode: BlockMode) -> Result<Expression> {
        self.check_failed()?;

        if !mode.normal()
            || self
                .peek()
                .filter(|t| t.kind == TokenKind::LBrace)
                .is_none()
        {
            return self.primary();
        }

        let block = self.block()?;
        let span = self.span(block.id);

        Ok(Expression::new(
            ExpressionKind::Block(Box::new(block)),
            self.prepare_node(span),
        ))
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
