//! Parsing in tackc.

pub mod error;
use std::{collections::HashMap, num::NonZeroU32};

const RECURSION_LIMIT: u32 = 300;
const PATH_COMPONENTS_LIMIT: usize = 32;

use error::{ParseError, Result};
use nonzero::nonzero;

use crate::ast::NodeId;
use crate::file::File;
use crate::global::Global;
use crate::lexer::{Token, TokenKind};
use crate::span::{Span, SpanValue};
use crate::utils::UnwrapExt;
use thin_vec::ThinVec;

use crate::{
    ast::{
        AssignmentStatement, AstModule, AstPath, BinOp, Block, ConstItem, Expression,
        ExpressionKind, ExpressionStatement, FuncItem, ImpItem, Item, ItemKind, LetStatement,
        ModStatement, Statement, StatementKind, Symbol, UnOp,
    },
    parser::error::ErrorExt,
};

/// The ways to parse, in respect to blocks.
#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub enum BlockMode {
    /// Parsing without blocks.
    NoBlocks,
    /// Parsing with blocks.
    Normal,
}

impl BlockMode {
    /// Returns `true` if this block mode is [`Normal`](Self::Normal).
    pub fn normal(self) -> bool {
        self == Self::Normal
    }
}

#[derive(Debug, Clone, Copy)]
struct ParserSnapshot {
    ptr: usize,
    open: NonZeroU32,
    failed: bool,
}

/// The state of a parser.
pub struct Parser<'src, 'a> {
    file: &'src File<'src>,
    tokens: &'a [Token],
    ptr: usize,
    errors: Vec<ParseError>,

    failed: bool,
    failed_recursion: bool,
    failed_error: bool,

    next_open: NonZeroU32,
    spans: HashMap<NodeId, Span>,

    global: &'src Global,
}

impl<'src, 'a> Parser<'src, 'a> {
    /// Creates a new parser.
    pub fn new(tokens: &'a [Token], file: &'src File, global: &'src Global) -> Self {
        Parser {
            file,
            tokens,
            ptr: 0,
            errors: Vec::new(),

            failed: false,
            failed_recursion: false,
            failed_error: false,

            next_open: nonzero!(1u32),
            spans: HashMap::new(),

            global,
        }
    }

    fn check_failed(&mut self, recursion: u32) -> Result<()> {
        if recursion > RECURSION_LIMIT {
            self.errors.push(ParseError::recursion_limit());
            self.failed = true;
            self.failed_recursion = true;
        }
        if self.failed {
            Err(ParseError::failed())
        } else {
            Ok(())
        }
    }

    const fn snapshot(&self) -> ParserSnapshot {
        ParserSnapshot {
            ptr: self.ptr,
            open: self.next_open,
            failed: self.failed,
        }
    }

    const fn restore(&mut self, snapshot: ParserSnapshot) {
        let ParserSnapshot { ptr, open, failed } = snapshot;
        if failed {
            return;
        }

        self.ptr = ptr;
        self.next_open = open;
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

    fn expect_kinds(&mut self, kinds: &[TokenKind]) -> Result<Token> {
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

    fn prepare_node(&mut self, span: Span) -> Result<NodeId> {
        let open = NodeId {
            id: self.next_open,
            file: self.file.id(),
        };
        self.spans.insert(open, span);
        let Some(next_open) = self.next_open.checked_add(1) else {
            return Err(ParseError::node_id_limit());
        };

        self.next_open = next_open;
        Ok(open)
    }

    fn push_err(&mut self, e: ParseError) {
        if self.errors.len() > 100 && !self.failed_error {
            self.failed = true;
            self.failed_error = true;
            self.errors.push(ParseError::error_limit());
        }
        if self.failed {
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
                kind if cancel.contains(&kind) && depth == 0 => {
                    return;
                }
                TokenKind::LBrace | TokenKind::LBracket | TokenKind::LParen => {
                    depth += 1;
                }
                TokenKind::RBrace | TokenKind::RBracket | TokenKind::RParen => {
                    depth = depth.saturating_sub(1);
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

    /// Gets the span for a specific node in the AST being parsed by this parser.
    ///
    /// # Panics
    /// This function panics if the file is not the file being parsed, or if the span map is missing the id.
    fn span(&self, id: NodeId) -> Span {
        assert!(id.file == self.file.id(), "wrong file!");
        assert!(self.spans.contains_key(&id), "missing id!");

        // We already asserted that self.spans contains the key.
        *self.spans.get(&id).expect_unreachable() // CHECKED(Chloe)
    }

    fn expect_report(&mut self, kinds: &[TokenKind], expected: &'static str) -> Option<Token> {
        let tok_res = self.expect_kinds(kinds);
        self.report_error(tok_res, expected)
    }

    fn parse_sync<T: 'src, P: FnOnce(&mut Self, u32) -> Result<T>>(
        &mut self,
        func: P,
        cancel: &[TokenKind],
        expected: &'static str,
        recursion: u32,
    ) -> Option<T> {
        let snapshot = self.snapshot();
        let res = func(self, recursion + 1);
        self.handle_error_sync(res, snapshot, cancel, expected, false)
    }

    fn parse_sync_skip<T: 'src, P: FnOnce(&mut Self, u32) -> Result<T>>(
        &mut self,
        func: P,
        cancel: &[TokenKind],
        expected: &'static str,
        recursion: u32,
    ) -> Option<T> {
        let snapshot = self.snapshot();
        let res = func(self, recursion + 1);
        self.handle_error_sync(res, snapshot, cancel, expected, true)
    }

    fn parse_report<T, P: FnOnce(&mut Self, u32) -> Result<T>>(
        &mut self,
        func: P,
        expected: &'static str,
        recursion: u32,
    ) -> Option<T> {
        let res = func(self, recursion + 1);
        self.report_error(res, expected)
    }

    /// Parses an program from `tokens` and `file`, and returns all errors.
    pub fn parse(
        tokens: &'a [Token],
        file: &'src File,
        global: &'src Global,
    ) -> (AstModule<'src>, Vec<ParseError>, bool) {
        let mut p = Parser::new(tokens, file, global);
        let mut module = p.module(0);
        // Override default spans
        module.spans = p.spans;
        (module, p.errors, p.failed)
    }

    #[inline]
    fn alloc<T>(&self, val: T) -> &'src T {
        self.global.alloc(val)
    }

    #[inline]
    fn alloc_option<T>(&self, val: Option<T>) -> Option<&'src T> {
        val.map(|val| self.alloc(val))
    }

    // We use Option<Option<T>> for error handling.
    #[expect(clippy::option_option)] // CHECKED(Chloe)
    #[inline]
    fn alloc_option_option<T>(&self, val: Option<Option<T>>) -> Option<Option<&'src T>> {
        val.map(|val| self.alloc_option(val))
    }

    fn delimited<T: 'src>(
        &mut self,
        seperator: TokenKind,
        closing: TokenKind,
        parse: fn(&mut Self, u32) -> Result<T>,
        expected: &'static str,
        recursion: u32,
    ) -> ThinVec<Option<&'src T>> {
        let mut args = ThinVec::new();
        loop {
            if let Some(tok) = self.peek()
                && tok.kind == closing
            {
                break;
            }

            let expr = self.parse_sync(parse, &[closing, seperator], expected, recursion);
            args.push(self.alloc_option(expr));
            if self.eat(&[seperator]).is_none() {
                break;
            }
        }

        args
    }

    fn visibility(&mut self) -> bool {
        self.eat(&[TokenKind::Exp]).is_some()
    }

    fn module(&mut self, recursion: u32) -> AstModule<'src> {
        let mod_stmt_res = self.mod_statement(recursion + 1);
        let mod_stmt = self.report_error(mod_stmt_res, "`mod` statement");

        let mut items = ThinVec::new();
        while !self.at_eof() {
            let item = self.parse_sync_skip(
                Self::item,
                &[TokenKind::Const, TokenKind::Func],
                "item",
                recursion + 1,
            );
            items.push(item.map(|i| self.alloc(i)));
        }

        // Default map for spans
        AstModule {
            mod_stmt: self.alloc_option(mod_stmt),
            items,
            spans: HashMap::new(),
        }
    }

    fn mod_statement(&mut self, recursion: u32) -> Result<ModStatement> {
        let exported = self.visibility();
        let mod_key = self.expect_kinds(&[TokenKind::Mod])?;
        let path = self.parse_sync(Self::path, &[TokenKind::Semicolon], "path", recursion + 1);
        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            mod_key.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
        );
        Ok(ModStatement {
            exported,
            path,
            id: self.prepare_node(span)?,
        })
    }

    fn path(&mut self, recursion: u32) -> Result<AstPath> {
        self.check_failed(recursion)?;

        let mut components = ThinVec::new();
        let ident = self.expect_kinds(&[TokenKind::Ident])?;
        components.push(Some(Symbol::new(ident, self.file.id())));
        while self.eat(&[TokenKind::Dot]).is_some() {
            let ident = self.expect_report(&[TokenKind::Ident], "identifier");
            components.push(ident.map(|ident| Symbol::new(ident, self.file.id())));

            if components.len() > PATH_COMPONENTS_LIMIT {
                self.push_err(ParseError::path_components_limit());
                self.failed = true;
                self.check_failed(recursion)?;
            }
        }
        let span = Span::new_from(
            ident.span.start,
            components
                .last()
                // One component was already pushed, so there has to be at least one.
                .expect_unreachable() // CHECKED(Chloe)
                .map_or_else(|| self.loc(), |sym: Symbol| sym.1.end),
        );
        Ok(AstPath {
            components,
            id: self.prepare_node(span)?,
        })
    }

    fn item(&mut self, recursion: u32) -> Result<Item<'src>> {
        self.check_failed(recursion)?;

        let starts = &[TokenKind::Const, TokenKind::Func, TokenKind::Imp];
        let tok = if self.expect_peek(&[TokenKind::Exp]).is_ok() {
            self.expect_peek2(starts)
        } else {
            self.expect_peek(starts)
        }?;
        match tok.kind {
            TokenKind::Const => self.const_item(recursion + 1),
            TokenKind::Func => self.func_item(recursion + 1),
            TokenKind::Imp => self.imp_item(recursion + 1),
            _ => Err(ParseError::expected(None, tok)),
        }
    }

    fn const_item(&mut self, recursion: u32) -> Result<Item<'src>> {
        self.check_failed(recursion)?;

        let exported = self.visibility();
        let const_key = self.expect_kinds(&[TokenKind::Const])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let ty = if self.eat(&[TokenKind::Colon]).is_some() {
            Some(self.parse_sync(
                Self::expression_normal,
                &[TokenKind::Eq],
                "type",
                recursion + 1,
            ))
        } else {
            None
        };

        let _eq = self.expect_report(&[TokenKind::Eq], "'='");
        let expr = self.parse_sync(
            Self::expression_normal,
            &[TokenKind::Semicolon],
            "expression",
            recursion + 1,
        );

        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            const_key.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
        );

        Ok(Item::new(
            self.alloc(ItemKind::ConstItem(self.alloc(ConstItem {
                exported,
                expr: self.alloc_option(expr),
                ty: self.alloc_option_option(ty),
                ident: ident.map(|ident| Symbol::new(ident, self.file.id())),
            }))),
            self.prepare_node(span)?,
        ))
    }

    fn func_item(&mut self, recursion: u32) -> Result<Item<'src>> {
        self.check_failed(recursion)?;

        let exported = self.visibility();
        let func = self.expect_kinds(&[TokenKind::Func])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let _opening = self.expect_report(&[TokenKind::LParen], "'('");

        let mut params = ThinVec::new();
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
                recursion + 1,
            );
            let expr = self.alloc_option(expr);
            params.push((ident.map(|ident| Symbol::new(ident, self.file.id())), expr));
            if self.eat(&[TokenKind::Comma]).is_none() {
                break;
            }
        }

        let _closing = self.expect_report(&[TokenKind::RParen], "')'");
        let ret_type = self
            .peek()
            .filter(|tok| tok.kind != TokenKind::LBrace)
            .map(|_| {
                self.parse_sync(
                    Self::expression_no_blocks,
                    &[TokenKind::LBrace],
                    "type",
                    recursion + 1,
                )
            });
        let block = self.parse_report(Self::block, "block", recursion + 1);

        let span = Span::new_from(
            func.span.start,
            block
                .as_ref()
                .map_or_else(|| self.loc(), |block| self.span(block.id).end),
        );

        Ok(Item {
            kind: self.alloc(ItemKind::FuncItem(self.alloc(FuncItem {
                exported,
                ident: ident.map(|ident| Symbol::new(ident, self.file.id())),
                params,
                ret_type: self.alloc_option_option(ret_type),
                block: self.alloc_option(block),
            }))),
            id: self.prepare_node(span)?,
        })
    }

    fn imp_item(&mut self, recursion: u32) -> Result<Item<'src>> {
        self.check_failed(recursion)?;

        let exported = self.visibility();
        let imp = self.expect_kinds(&[TokenKind::Imp])?;
        let path = self.parse_sync(Self::path, &[TokenKind::Semicolon], "path", recursion + 1);
        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            imp.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
        );
        Ok(Item {
            kind: self.alloc(ItemKind::ImpItem(self.alloc(ImpItem {
                exported,
                path: self.alloc_option(path),
            }))),
            id: self.prepare_node(span)?,
        })
    }

    fn block(&mut self, recursion: u32) -> Result<Block<'src>> {
        self.check_failed(recursion)?;

        let opening = self.expect_kinds(&[TokenKind::LBrace])?;
        let mut stmts = ThinVec::new();
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
                        recursion + 1,
                    );
                    stmts.push(self.alloc_option(stmt));
                }
                // Statements that don't end in semicolons
                Some(TokenKind::Func) => {
                    let snapshot = self.snapshot();
                    let stmt_res = self.statement(recursion + 1);
                    let stmt = self.report_error(stmt_res, "statement, item, or expression");
                    if stmt.is_none() {
                        self.restore(snapshot);
                        self.synchronize_skip_next_block();
                    }
                    stmts.push(self.alloc_option(stmt));
                }
                // Expressions that optionally end in semicolons when used as statements
                Some(TokenKind::LBrace) => {
                    let expr = self.expression(BlockMode::Normal, recursion + 1)?;
                    if let Some(tok) = self.peek()
                        && tok.kind == TokenKind::RBrace
                    {
                        break Some(Some(expr));
                    }
                    let semi = self.eat(&[TokenKind::Semicolon]);

                    let span = Span::new_from(
                        self.span(expr.id).start,
                        semi.map_or_else(|| self.loc(), |semi| semi.span.end),
                    );
                    let stmt = Statement::new(
                        self.alloc(StatementKind::ExpressionStatement(self.alloc(
                            ExpressionStatement {
                                expr: self.alloc(expr),
                                semi: semi.map(Some),
                            },
                        ))),
                        self.prepare_node(span)?,
                    );
                    stmts.push(Some(self.alloc(stmt)));
                }
                // Expressions that end in semicolons when used as statements
                Some(_) => {
                    let loc = self.loc();
                    let expr = self.parse_sync(
                        Self::expression_normal,
                        &[TokenKind::Semicolon, TokenKind::RBrace],
                        "statement, item, or expression",
                        recursion + 1,
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
                    let statement = match expr {
                        Some(expr) => Some(Statement::new(
                            self.alloc(StatementKind::ExpressionStatement(self.alloc(
                                ExpressionStatement {
                                    expr: self.alloc(expr),
                                    semi: Some(semi),
                                },
                            ))),
                            self.prepare_node(span)?,
                        )),
                        None => None,
                    };

                    stmts.push(self.alloc_option(statement));
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
            expr: self.alloc_option_option(expr),
            id: self.prepare_node(span)?,
        })
    }

    fn statement(&mut self, recursion: u32) -> Result<Statement<'src>> {
        self.check_failed(recursion)?;

        let tok = self.expect_peek_all()?;
        match tok.kind {
            TokenKind::Let => self.let_statement(recursion + 1),
            TokenKind::Const | TokenKind::Func => self.item_statement(recursion + 1),
            _ => self.statement_starting_with_expression(recursion + 1),
        }
    }

    fn let_statement(&mut self, recursion: u32) -> Result<Statement<'src>> {
        self.check_failed(recursion)?;

        let let_key = self.expect_kinds(&[TokenKind::Let])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let ty = if self.eat(&[TokenKind::Colon]).is_some() {
            Some(self.parse_sync(
                Self::expression_normal,
                &[TokenKind::Eq],
                "type",
                recursion + 1,
            ))
        } else {
            None
        };

        let expr = if self.eat(&[TokenKind::Eq]).is_some() {
            Some(self.parse_sync(
                Self::expression_normal,
                &[TokenKind::Semicolon],
                "expression",
                recursion + 1,
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
            self.alloc(StatementKind::LetStatement(self.alloc(LetStatement {
                expr: self.alloc_option_option(expr),
                ty: self.alloc_option_option(ty),
                ident: ident.map(|ident| Symbol::new(ident, self.file.id())),
            }))),
            self.prepare_node(span)?,
        ))
    }

    fn item_statement(&mut self, recursion: u32) -> Result<Statement<'src>> {
        self.check_failed(recursion)?;

        let item = self.item(recursion + 1)?;
        let span = self.span(item.id);
        Ok(Statement::new(
            self.alloc(StatementKind::Item(self.alloc(item))),
            self.prepare_node(span)?,
        ))
    }

    fn statement_starting_with_expression(&mut self, recursion: u32) -> Result<Statement<'src>> {
        self.check_failed(recursion)?;

        let expr = self.expression(BlockMode::Normal, recursion + 1)?;
        match self.peek().map(|tok| tok.kind) {
            Some(TokenKind::Eq) => self.assignment_statement(expr, recursion + 1),
            _ => self.expression_statement(expr, recursion + 1),
        }
    }

    fn expression_statement(
        &mut self,
        expr: Expression<'src>,
        recursion: u32,
    ) -> Result<Statement<'src>> {
        self.check_failed(recursion)?;

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
            self.alloc(StatementKind::ExpressionStatement(self.alloc(
                ExpressionStatement {
                    expr: self.alloc(expr),
                    semi: semi.is_some().then_some(semi),
                },
            ))),
            self.prepare_node(span)?,
        ))
    }

    fn assignment_statement(
        &mut self,
        lhs: Expression<'src>,
        recursion: u32,
    ) -> Result<Statement<'src>> {
        self.check_failed(recursion)?;

        let _eq = self.expect_kinds(&[TokenKind::Eq])?;
        let rhs = self.parse_sync(
            Self::expression_normal,
            &[TokenKind::Semicolon],
            "expression",
            recursion + 1,
        );
        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            self.span(lhs.id).start,
            semi.map_or_else(|| self.loc(), |tok| tok.span.end),
        );
        Ok(Statement::new(
            self.alloc(StatementKind::AssignmentStatement(self.alloc(
                AssignmentStatement {
                    lhs: self.alloc(lhs),
                    rhs: self.alloc_option(rhs),
                },
            ))),
            self.prepare_node(span)?,
        ))
    }

    #[inline]
    fn expression_normal(&mut self, recursion: u32) -> Result<Expression<'src>> {
        self.expression(BlockMode::Normal, recursion)
    }

    #[inline]
    fn expression_no_blocks(&mut self, recursion: u32) -> Result<Expression<'src>> {
        self.expression(BlockMode::NoBlocks, recursion)
    }

    fn expression(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression<'src>> {
        self.comparison(mode, recursion)
    }

    fn comparison(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression<'src>> {
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
            recursion,
        )
    }

    fn term(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression<'src>> {
        self.binary_expr(
            &[
                (TokenKind::Plus, BinOp::Add),
                (TokenKind::Minus, BinOp::Sub),
            ],
            Self::factor,
            false,
            mode,
            recursion,
        )
    }

    fn factor(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression<'src>> {
        self.binary_expr(
            &[
                (TokenKind::Star, BinOp::Mul),
                (TokenKind::Slash, BinOp::Div),
            ],
            Self::unary,
            false,
            mode,
            recursion,
        )
    }

    #[inline]
    fn binary_expr(
        &mut self,
        tokens: &[(TokenKind, BinOp)],
        next: fn(&mut Self, BlockMode, u32) -> Result<Expression<'src>>,
        comparison: bool,
        mode: BlockMode,
        recursion: u32,
    ) -> Result<Expression<'src>> {
        self.check_failed(recursion)?;

        let mut lhs = next(self, mode, recursion + 1)?;
        let mut ops = Vec::new();
        while let Some(peeked) = self.peek() {
            let Some((_, op)) = tokens.iter().find(|(tok, _)| peeked.kind == *tok) else {
                break;
            };

            self.advance(); // Skip operator
            let rhs = next(self, mode, recursion + 1)?;
            let id = self.prepare_node(Span::new_from(
                self.span(lhs.id).start,
                self.span(rhs.id).end,
            ))?;
            lhs = Expression::new(
                self.alloc(ExpressionKind::Binary(
                    *op,
                    self.alloc(lhs),
                    self.alloc(rhs),
                )),
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

    fn unary(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression<'src>> {
        self.check_failed(recursion)?;

        let Some(op) = self.eat(&[TokenKind::Minus, TokenKind::Bang]) else {
            return self.postfix(mode, recursion + 1);
        };
        let rhs = self.unary(mode, recursion + 1)?;
        let span = Span::new_from(op.span.start, self.span(rhs.id).end);
        let kind = match op.kind {
            TokenKind::Minus => ExpressionKind::Unary(UnOp::Neg, self.alloc(rhs)),
            TokenKind::Bang => ExpressionKind::Unary(UnOp::Not, self.alloc(rhs)),
            // `eat` will only ever return tokens with the input types, which are all arms.
            _ => unreachable!(), // CHECKED(Chloe)
        };
        Ok(Expression::new(self.alloc(kind), self.prepare_node(span)?))
    }

    fn postfix(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression<'src>> {
        self.check_failed(recursion)?;

        let mut lhs = self.grouping(mode, recursion + 1)?;
        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Dot => lhs = self.parse_access(lhs)?,
                TokenKind::LParen => lhs = self.parse_call(lhs, recursion + 1)?,
                TokenKind::LBracket => lhs = self.parse_index(lhs, recursion + 1)?,
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_index(&mut self, lhs: Expression<'src>, recursion: u32) -> Result<Expression<'src>> {
        self.advance();

        let snapshot = self.snapshot();
        let expr_res = self.expression(BlockMode::Normal, recursion + 1);
        let expr = self.handle_error_sync(
            expr_res,
            snapshot,
            &[TokenKind::RBracket],
            "expression",
            false,
        );
        let closing_res = self.expect_kinds(&[TokenKind::RBracket]);
        let closing = self.report_error(closing_res, "']'");
        let span = Span::new_from(
            self.span(lhs.id).start,
            closing.map_or_else(|| self.loc(), |tok| tok.span.end),
        );

        Ok(Expression::new(
            self.alloc(ExpressionKind::Index(
                self.alloc(lhs),
                self.alloc_option(expr),
            )),
            self.prepare_node(span)?,
        ))
    }

    fn parse_call(&mut self, lhs: Expression<'src>, recursion: u32) -> Result<Expression<'src>> {
        self.advance();
        let args = self.delimited(
            TokenKind::Comma,
            TokenKind::RParen,
            Self::expression_normal,
            "expression",
            recursion + 1,
        );
        let closing_res = self.expect_kinds(&[TokenKind::RParen]);
        let closing = self.report_error(closing_res, "')'");
        let span = Span::new_from(
            self.span(lhs.id).start,
            closing.map_or_else(|| self.loc(), |tok| tok.span.end),
        );
        Ok(Expression::new(
            self.alloc(ExpressionKind::Call(self.alloc(lhs), args)),
            self.prepare_node(span)?,
        ))
    }

    fn parse_access(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        self.advance();
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let span = Span::new_from(
            self.span(lhs.id).start,
            ident.map_or_else(|| self.loc(), |tok| tok.span.end),
        );
        Ok(Expression::new(
            self.alloc(ExpressionKind::Member(
                self.alloc(lhs),
                self.alloc_option(ident.map(|ident| Symbol::new(ident, self.file.id()))),
            )),
            self.prepare_node(span)?,
        ))
    }

    fn grouping(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression<'src>> {
        self.check_failed(recursion)?;

        let Some(opening) = self.eat(&[TokenKind::LParen]) else {
            return self.block_expr(mode, recursion + 1);
        };

        let inner = self.parse_sync(
            Self::expression_normal,
            &[TokenKind::RParen],
            "expression",
            recursion + 1,
        );
        let closing = self.expect_report(&[TokenKind::RParen], "')'");

        let expr = Expression::new(
            self.alloc(ExpressionKind::Grouping(self.alloc_option(inner))),
            self.prepare_node(Span::new_from(
                opening.span.start,
                closing.map_or_else(|| self.loc(), |tok| tok.span.end),
            ))?,
        );
        Ok(expr)
    }

    fn block_expr(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression<'src>> {
        self.check_failed(recursion)?;

        if !mode.normal()
            || self
                .peek()
                .filter(|t| t.kind == TokenKind::LBrace)
                .is_none()
        {
            return self.global_ident(recursion);
        }

        let block = self.block(recursion)?;
        let span = self.span(block.id);

        Ok(Expression::new(
            self.alloc(ExpressionKind::Block(self.alloc(block))),
            self.prepare_node(span)?,
        ))
    }

    fn global_ident(&mut self, recursion: u32) -> Result<Expression<'src>> {
        self.check_failed(recursion)?;

        let Some(dot) = self.eat(&[TokenKind::Dot]) else {
            return self.primary(recursion + 1);
        };

        if let Some(tok) = self.eat(&[TokenKind::IntLit]) {
            self.push_err(ParseError::other(
                "float literals cannot start with a '.'",
                [tok.span],
            ));
            return Ok(Expression::new(
                self.alloc(ExpressionKind::GlobalIdent(None)),
                self.prepare_node(Span::new_from(dot.span.start, tok.span.end))?,
            ));
        }

        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let span = Span::new_from(
            dot.span.start,
            ident
                .as_ref()
                .map_or_else(|| self.loc(), |ident| ident.span.end),
        );
        Ok(Expression::new(
            self.alloc(ExpressionKind::GlobalIdent(
                ident.map(|ident| Symbol::new(ident, self.file.id())),
            )),
            self.prepare_node(span)?,
        ))
    }

    fn primary(&mut self, recursion: u32) -> Result<Expression<'src>> {
        self.check_failed(recursion)?;

        let tok = self.expect_kinds(&[
            TokenKind::IntLit,
            TokenKind::FloatLit,
            TokenKind::Ident,
            TokenKind::StringLit,
        ])?;
        let primary = match tok.kind {
            TokenKind::IntLit => ExpressionKind::IntLit(tok.lexeme),
            TokenKind::FloatLit => ExpressionKind::FloatLit(tok.lexeme),
            TokenKind::Ident => ExpressionKind::Ident(Symbol::new(tok, self.file.id())),
            TokenKind::StringLit => ExpressionKind::StringLit(tok.lexeme),
            // `expect` will only return token kinds of the inputs, and all the inputs are arms.
            _ => unreachable!(), // CHECKED(Chloe)
        };
        let expr = Expression::new(self.alloc(primary), self.prepare_node(tok.span)?);

        Ok(expr)
    }
}

#[test]
fn parser_test_glob() {
    setup_insta_test!();

    insta::glob!("parser/tests/*.tck", run_prog_parser_test);
}

#[cfg(test)]
use std::path::Path;

#[cfg(test)]
// No `unwrap`s in this function are documented, because all of them sidestep errors.
fn run_prog_parser_test(path: &Path) {
    use std::fs;

    use crate::file::File;
    use crate::lexer::Lexer;

    let src = fs::read_to_string(path).unwrap(); // CHECKED(Chloe)
    let file = File::new(src, Path::new("testing.tck"));
    let global = Global::create_heap();
    let lexer = Lexer::new(&file, &global);
    let tokens = lexer
        .map(
            |res| res.unwrap(), // CHECKED(Chloe)
        )
        .collect::<Vec<_>>();
    let (prog, err, _) = Parser::parse(&tokens, &file, &global);
    insta::assert_ron_snapshot!((prog, err));
}
