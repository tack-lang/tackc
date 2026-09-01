//! Parsing in tackc.

pub mod error;
use std::num::NonZeroU32;

/// The limit on recursion in the parser. Once this limit is reached, the compiler will throw a fatal error.
const RECURSION_LIMIT: u32 = 300;
/// The limit on path length in the parser. Once this limit is reached, the compiler will throw a fatal error.
const PATH_COMPONENTS_LIMIT: usize = 32;

use error::{ParseError, Result};
use nonzero::nonzero;
use thin_vec::ThinVec;

use crate::{
    file::File,
    frontend::{
        ast::{
            AssignmentStatement, AstModule, AstPath, BinOp, Block, ConstItem, Expression,
            ExpressionKind, ExpressionStatement, FuncItem, Function, FunctionType, ImpItem, Item,
            ItemKind, LetStatement, ModStatement, NodeId, Statement, StatementKind, Symbol,
            TriState, UnOp,
        },
        lexer::{Token, TokenKind},
        parser::error::ErrorExt,
    },
    global::Global,
    span::{Span, SpanValue},
    utils::{UnwrapExt, intern::Interned},
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

/// A snapshot of the parser's state.
#[derive(Debug, Clone, Copy)]
struct ParserSnapshot {
    /// A pointer to where the parser was in the `tokens` list.
    ptr: usize,
    /// The next open node ID.
    next_open: NonZeroU32,
    /// Whether or not the parser was in a failure mode.
    failed: bool,
}

/// The state of a parser.
pub struct Parser<'src, 'token> {
    /// The file this parser is parsing.
    file: &'src File,
    /// The tokens created from the file.
    tokens: &'token [Token],
    /// A pointer to the parser's location in the `tokens` list.
    ptr: usize,
    /// A list of errors accumulated while parsing.
    errors: Vec<ParseError>,

    /// Whether or not the parser has hit a fatal error.
    failed: bool,
    /// Whether or not the parser has hit the recursion limit.
    failed_recursion: bool,
    /// Whether or not the parser has hit the error limit.
    failed_error: bool,

    /// The next open node ID.
    next_open: NonZeroU32,

    /// The global context given to the parser.
    global: &'src Global,
}

impl<'src, 'token> Parser<'src, 'token> {
    /// Creates a new parser.
    const fn new(tokens: &'token [Token], file: &'src File, global: &'src Global) -> Self {
        Parser {
            file,
            tokens,
            ptr: 0,
            errors: Vec::new(),

            failed: false,
            failed_recursion: false,
            failed_error: false,

            next_open: nonzero!(1_u32),

            global,
        }
    }

    /// Checks whether the parser has failed or not. Should be called at the beginning of most functions in the parser.
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

    /// Creates a snapshot of the parser's state.
    const fn snapshot(&self) -> ParserSnapshot {
        ParserSnapshot {
            ptr: self.ptr,
            next_open: self.next_open,
            failed: self.failed,
        }
    }

    /// Restores a snapshot of the parser's state.
    const fn restore(&mut self, snapshot: ParserSnapshot) {
        let ParserSnapshot {
            ptr,
            next_open,
            failed,
        } = snapshot;
        if failed {
            return;
        }

        self.ptr = ptr;
        self.next_open = next_open;
    }

    /// Gets the token being pointed to, if any.
    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.ptr).copied()
    }

    /// Gets the token after the token being pointed to, if any.
    fn peek2(&self) -> Option<Token> {
        self.tokens.get(self.ptr + 1).copied()
    }

    /// Returns whether or not the parser is at the end of the token list.
    fn at_eof(&self) -> bool {
        self.peek().is_none()
    }

    /// If the next token is in `kinds`, this function will consume it and return it. Otherwise, it will return None.
    fn eat(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        self.peek()
            .filter(|tok| kinds.contains(&tok.kind))
            .inspect(|_| {
                self.advance();
            })
    }

    /// This function expects the next token to be in `kinds`, consumes it if it's there, and throws an error if it's not.
    fn expect_kinds(&mut self, kinds: &[TokenKind]) -> Result<Token> {
        self.expect_peek(kinds).inspect(|_| {
            self.advance();
        })
    }

    /// This function expects the next token to be in `kinds`, and throws an error if it's not.
    fn expect_peek(&self, kinds: &[TokenKind]) -> Result<Token> {
        let token = self.expect_peek_all()?;
        if kinds.contains(&token.kind) {
            Ok(token)
        } else {
            Err(ParseError::expected(None, token))
        }
    }

    /// This function expects the token after the next token to be in `kinds`, and throws an error if it's not.
    fn expect_peek2(&self, kinds: &[TokenKind]) -> Result<Token> {
        let token = self.expect_peek2_all()?;
        if kinds.contains(&token.kind) {
            Ok(token)
        } else {
            Err(ParseError::expected(None, token))
        }
    }

    /// This function expects the next token to exist, and throws an error if it doesn't.
    fn expect_peek_all(&self) -> Result<Token> {
        self.peek().ok_or_else(|| ParseError::eof(None, self.file))
    }

    /// This function expects the token after the next token to exist, and throws an error if it doesn't.
    fn expect_peek2_all(&self) -> Result<Token> {
        self.peek2().ok_or_else(|| ParseError::eof(None, self.file))
    }

    /// This function returns the current token, and advances the parser to the next token.
    fn advance(&mut self) -> Option<Token> {
        let tok = self.peek();
        self.ptr += 1;
        tok
    }

    /// This function returns the next availible [`NodeId`], throwing an error if there is none.
    const fn get_id(&mut self) -> Result<NodeId> {
        let open = NodeId {
            id: self.next_open,
            file: self.file.id(),
        };
        let Some(next_open) = self.next_open.checked_add(1) else {
            return Err(ParseError::node_id_limit());
        };

        self.next_open = next_open;
        Ok(open)
    }

    /// This function pushes an error to the error list.
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

    /// This function reports `err` with `expected` if `err` is [`Err`].
    fn report_error<T>(&mut self, err: Result<T>, expected: &'static str) -> Option<T> {
        err.set_expected(expected)
            .map_err(|e| self.push_err(e))
            .ok()
    }

    /// This function handles an error and synchronizes the parser.
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

    /// This function synchronizes the parser, using `cancel` as the token to stop at.
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

    /// This function synchronizes by skipping the next block.
    fn synchronize_skip_next_block(&mut self) {
        self.synchronize(&[TokenKind::LBrace]);
        self.advance();
        self.synchronize(&[TokenKind::RBrace]);
        self.advance();
    }

    /// Returns the location at which the parser is currently located.
    fn loc(&self) -> SpanValue {
        self.peek()
            .map_or_else(|| Span::eof(self.file).start, |tok| tok.span.start)
    }

    /// This function runs [`Self::expect_kinds`], and then [`Self::report_error`] on the result.
    fn expect_report(&mut self, kinds: &[TokenKind], expected: &'static str) -> Option<Token> {
        let tok_res = self.expect_kinds(kinds);
        self.report_error(tok_res, expected)
    }

    /// Parses a value using `func`, then synchronizes on failure using `cancel` and `expected`.
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

    /// Parses a value using `func`, then synchronizes on failure using `cancel` and `expected`, while skipping a `cancel` token if it's the next one.
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

    /// Parses using `func`, and reports the result.
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
    ///
    /// # Returns
    /// This function returns three things. An [`AstModule`], a [`Vec`] of [`ParseErrors`](ParseError), and a bool to represent whether the parser "failed" or not.
    /// If the parser failed, that means it reached an error that it couldn't recover from. This is rare, and should probably halt execution.
    pub fn parse(
        tokens: &'token [Token],
        file: &'src File,
        global: &'src Global,
    ) -> (AstModule, Vec<ParseError>, bool) {
        let mut p = Parser::new(tokens, file, global);
        let module = p.module(0);
        // Override default spans
        (module, p.errors, p.failed)
    }

    /// Parses a delimited list.
    fn delimited<T: 'src>(
        &mut self,
        seperator: TokenKind,
        closing: TokenKind,
        parse: fn(&mut Self, u32) -> Result<T>,
        expected: &'static str,
        recursion: u32,
    ) -> ThinVec<Option<T>> {
        let mut args = ThinVec::new();
        loop {
            if let Some(tok) = self.peek()
                && tok.kind == closing
            {
                break;
            }

            let expr = self.parse_sync(parse, &[closing, seperator], expected, recursion);
            args.push(expr);
            if self.eat(&[seperator]).is_none() {
                break;
            }
        }

        args
    }

    /// Parses a visibility. Returns true if `exp`, and false if not.
    fn visibility(&mut self) -> bool {
        self.eat(&[TokenKind::Exp]).is_some()
    }

    /// Parses a module.
    fn module(&mut self, recursion: u32) -> AstModule {
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
            items.push(item);
        }

        // Default map for spans
        AstModule {
            mod_stmt,
            items,
            file: self.file.id(),
        }
    }

    /// Parses a mod statement.
    fn mod_statement(&mut self, recursion: u32) -> Result<ModStatement> {
        let exported = self.visibility();
        let mod_key = self.expect_kinds(&[TokenKind::Mod])?;
        let path = self.parse_sync(Self::path, &[TokenKind::Semicolon], "path", recursion + 1);
        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            mod_key.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
            self.file,
        );
        Ok(ModStatement {
            exported,
            path,
            id: self.get_id()?,
            span,
        })
    }

    /// Parses an [`AstPath`].
    fn path(&mut self, recursion: u32) -> Result<AstPath> {
        self.check_failed(recursion)?;

        let mut components = ThinVec::new();
        let ident = self.expect_kinds(&[TokenKind::Ident])?;
        components.push(Some(self.global.interner.intern(Symbol::new(ident))));

        while self.eat(&[TokenKind::Dot]).is_some() {
            let tok = self.expect_report(&[TokenKind::Ident], "identifier");
            let ident = tok.map(|ident| self.global.interner.intern(Symbol::new(ident)));
            components.push(ident);

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
                .map_or_else(|| self.loc(), |sym| sym.get(&self.global.interner).1.end),
            self.file,
        );
        Ok(AstPath::new(components, self.get_id()?, span))
    }

    /// Parses an item.
    fn item(&mut self, recursion: u32) -> Result<Item> {
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

    /// Parses a `const` item.
    fn const_item(&mut self, recursion: u32) -> Result<Item> {
        self.check_failed(recursion)?;

        let exported = self.visibility();
        let const_key = self.expect_kinds(&[TokenKind::Const])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let ty = if self.eat(&[TokenKind::Colon]).is_some() {
            TriState::from_error(self.parse_sync(
                Self::expression_normal,
                &[TokenKind::Eq],
                "type",
                recursion + 1,
            ))
        } else {
            TriState::None
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
            self.file,
        );

        Ok(Item::new(
            ItemKind::ConstItem(ConstItem {
                exported,
                expr,
                ty,
                ident: ident.map(|ident| self.global.interner.intern(Symbol::new(ident))),
            }),
            self.get_id()?,
            span,
        ))
    }

    /// Parses a `func` item.
    fn func_item(&mut self, recursion: u32) -> Result<Item> {
        self.check_failed(recursion)?;

        let exported = self.visibility();
        let func = self.expect_kinds(&[TokenKind::Func])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let _opening = self.expect_report(&[TokenKind::LParen], "'('");

        let params = self.param_list_required_ident(recursion + 1)?;

        let _closing = self.expect_report(&[TokenKind::RParen], "')'");
        let ret_type = if self.peek().is_some_and(|tok| tok.kind != TokenKind::LBrace) {
            TriState::from_error(self.parse_sync(
                Self::expression_no_blocks,
                &[TokenKind::LBrace],
                "type",
                recursion + 1,
            ))
        } else {
            TriState::None
        };
        let block = self.parse_report(Self::block, "block", recursion + 1);

        let span = Span::new_from(
            func.span.start,
            block
                .as_ref()
                .map_or_else(|| self.loc(), |block| block.span.end),
            self.file,
        );

        Ok(Item {
            kind: ItemKind::FuncItem(FuncItem {
                exported,
                ident: ident.map(|ident| self.global.interner.intern(Symbol::new(ident))),
                params,
                ret_type,
                block,
            }),
            id: self.get_id()?,
            span,
        })
    }

    /*/// Parses a parameter list with identifiers being optional.
    fn param_list_optional_ident(
        &mut self,
        recursion: u32,
    ) -> Result<ThinVec<(Option<Interned<Symbol>>, Option<Expression>)>> {
        self.param_list(recursion + 1, false)
    }*/

    /// Parses a parameter list with identifiers being required.
    fn param_list_required_ident(
        &mut self,
        recursion: u32,
    ) -> Result<ThinVec<(Option<Interned<Symbol>>, Option<Expression>)>> {
        self.param_list(recursion + 1, true)
    }

    /// Parses a parameter list.
    fn param_list(
        &mut self,
        recursion: u32,
        required_ident: bool,
    ) -> Result<ThinVec<(Option<Interned<Symbol>>, Option<Expression>)>> {
        self.check_failed(recursion)?;

        let mut params = ThinVec::new();
        loop {
            if let Some(tok) = self.peek()
                && tok.kind == TokenKind::RParen
            {
                break;
            }

            let ident = if required_ident {
                let ident = self.expect_report(&[TokenKind::Ident], "identifier");
                let _colon = self.expect_report(&[TokenKind::Colon], "':'");
                ident
            } else {
                let ident = self.eat(&[TokenKind::Ident]);
                if ident.is_some() {
                    let _colon = self.expect_report(&[TokenKind::Colon], "':'");
                }
                ident
            };
            let expr = self.parse_sync(
                Self::expression_normal,
                &[TokenKind::Comma, TokenKind::RParen],
                "expression",
                recursion + 1,
            );
            params.push((
                ident.map(|ident| self.global.interner.intern(Symbol::new(ident))),
                expr,
            ));
            if self.eat(&[TokenKind::Comma]).is_none() {
                break;
            }
        }

        Ok(params)
    }

    /// Parses a `imp` item.
    fn imp_item(&mut self, recursion: u32) -> Result<Item> {
        self.check_failed(recursion)?;

        let exported = self.visibility();
        let imp = self.expect_kinds(&[TokenKind::Imp])?;
        let path = self.parse_sync(Self::path, &[TokenKind::Semicolon], "path", recursion + 1);
        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            imp.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
            self.file,
        );
        Ok(Item {
            kind: ItemKind::ImpItem(ImpItem { exported, path }),
            id: self.get_id()?,
            span,
        })
    }

    /// Parses a block.
    fn block(&mut self, recursion: u32) -> Result<Block> {
        self.check_failed(recursion)?;

        let opening = self.expect_kinds(&[TokenKind::LBrace])?;
        let mut stmts = ThinVec::new();
        let expr = loop {
            if let Some(tok) = self.peek()
                && tok.kind == TokenKind::RBrace
            {
                break TriState::None;
            }

            match self.peek().map(|tok| tok.kind) {
                // Statements that end in semicolons
                Some(TokenKind::Let | TokenKind::Const) => {
                    stmts.push(self.semicolon_statement(recursion + 1));
                }
                // Statements that don't end in semicolons
                Some(TokenKind::Func) => {
                    stmts.push(self.no_semicolon_statement(recursion + 1));
                }
                // Expressions that optionally end in semicolons when used as statements
                Some(TokenKind::LBrace) => {
                    let expr = self.expression(BlockMode::Normal, recursion + 1)?;
                    if let Some(tok) = self.peek()
                        && tok.kind == TokenKind::RBrace
                    {
                        break TriState::Some(expr);
                    }

                    stmts.push(self.optional_semicolon_expression_statement(expr)?);
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
                        break TriState::from_error(expr);
                    }
                    stmts.push(self.semicolon_expression_statement(loc, expr)?);
                }
                None => {
                    self.push_err(ParseError::eof(
                        Some("statement, item, or expression"),
                        self.file,
                    ));
                    break TriState::None;
                }
            }
        };

        let closing = self.expect_report(&[TokenKind::RBrace], "'}'");
        let span = Span::new_from(
            opening.span.start,
            closing.map_or_else(|| self.loc(), |tok| tok.span.end),
            self.file,
        );

        Ok(Block {
            stmts,
            expr,
            id: self.get_id()?,
            span,
        })
    }

    /// Parses a statement ending in a semicolon.
    fn semicolon_statement(&mut self, recursion: u32) -> Option<Statement> {
        self.parse_sync(
            Self::statement,
            &[TokenKind::Semicolon],
            "statement, item, or expression",
            recursion + 1,
        )
    }

    /// Parses a statement not ending a semicolon.
    fn no_semicolon_statement(&mut self, recursion: u32) -> Option<Statement> {
        let snapshot = self.snapshot();
        let stmt_res = self.statement(recursion + 1);
        let stmt = self.report_error(stmt_res, "statement, item, or expression");
        if stmt.is_none() {
            self.restore(snapshot);
            self.synchronize_skip_next_block();
        }
        stmt
    }

    /// Parses a statement which may or may not end in a semicolon.
    fn optional_semicolon_expression_statement(
        &mut self,
        expr: Expression,
    ) -> Result<Option<Statement>> {
        let semi = self.eat(&[TokenKind::Semicolon]);

        let span = Span::new_from(
            expr.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
            self.file,
        );
        let stmt = Statement::new(
            StatementKind::ExpressionStatement(ExpressionStatement {
                expr,
                semi: TriState::from_optional(semi),
            }),
            self.get_id()?,
            span,
        );
        Ok(Some(stmt))
    }

    /// Parses an expression statement ending with a semicolon, given the expression.
    fn semicolon_expression_statement(
        &mut self,
        loc: u32,
        expr: Option<Expression>,
    ) -> Result<Option<Statement>> {
        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            expr.as_ref().map_or(loc, |expr| expr.span.start),
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
            self.file,
        );
        let statement = match expr {
            Some(expr) => Some(Statement::new(
                StatementKind::ExpressionStatement(ExpressionStatement {
                    expr,
                    semi: TriState::from_error(semi),
                }),
                self.get_id()?,
                span,
            )),
            None => None,
        };

        Ok(statement)
    }

    /// Parses a statement.
    fn statement(&mut self, recursion: u32) -> Result<Statement> {
        self.check_failed(recursion)?;

        let tok = self.expect_peek_all()?;
        match tok.kind {
            TokenKind::Let => self.let_statement(recursion + 1),
            TokenKind::Const | TokenKind::Func => self.item_statement(recursion + 1),
            _ => self.statement_starting_with_expression(recursion + 1),
        }
    }

    /// Parses a let statement.
    fn let_statement(&mut self, recursion: u32) -> Result<Statement> {
        self.check_failed(recursion)?;

        let let_key = self.expect_kinds(&[TokenKind::Let])?;
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let ty = if self.eat(&[TokenKind::Colon]).is_some() {
            TriState::from_error(self.parse_sync(
                Self::expression_normal,
                &[TokenKind::Eq],
                "type",
                recursion + 1,
            ))
        } else {
            TriState::None
        };

        let expr = if self.eat(&[TokenKind::Eq]).is_some() {
            TriState::from_error(self.parse_sync(
                Self::expression_normal,
                &[TokenKind::Semicolon],
                "expression",
                recursion + 1,
            ))
        } else {
            TriState::None
        };

        let semi = self.expect_report(&[TokenKind::Semicolon], "';'");

        let span = Span::new_from(
            let_key.span.start,
            semi.map_or_else(|| self.loc(), |semi| semi.span.end),
            self.file,
        );

        Ok(Statement::new(
            StatementKind::LetStatement(LetStatement {
                expr,
                ty,
                ident: ident.map(|ident| self.global.interner.intern(Symbol::new(ident))),
            }),
            self.get_id()?,
            span,
        ))
    }

    /// Parses an item statement.
    fn item_statement(&mut self, recursion: u32) -> Result<Statement> {
        self.check_failed(recursion)?;

        let item = self.item(recursion + 1)?;
        let span = item.span;
        Ok(Statement::new(
            StatementKind::Item(item),
            self.get_id()?,
            span,
        ))
    }

    /// Parses a statement starting with an expression.
    fn statement_starting_with_expression(&mut self, recursion: u32) -> Result<Statement> {
        self.check_failed(recursion)?;

        let expr = self.expression(BlockMode::Normal, recursion + 1)?;
        match self.peek().map(|tok| tok.kind) {
            Some(TokenKind::Eq) => self.assignment_statement(expr, recursion + 1),
            _ => self.expression_statement(expr, recursion + 1),
        }
    }

    /// Parses an expression statement.
    fn expression_statement(&mut self, expr: Expression, recursion: u32) -> Result<Statement> {
        self.check_failed(recursion)?;

        let semi = if expr.kind.is_block() {
            TriState::from_optional(self.eat(&[TokenKind::Semicolon]))
        } else {
            TriState::from_error(self.expect_report(&[TokenKind::Semicolon], "';'"))
        };
        let span = semi.some().map_or(expr.span, |tok| {
            Span::new_from(expr.span.start, tok.span.end, self.file)
        });
        Ok(Statement::new(
            StatementKind::ExpressionStatement(ExpressionStatement { expr, semi }),
            self.get_id()?,
            span,
        ))
    }

    /// Parses an assignment statement.
    fn assignment_statement(&mut self, lhs: Expression, recursion: u32) -> Result<Statement> {
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
            lhs.span.start,
            semi.map_or_else(|| self.loc(), |tok| tok.span.end),
            self.file,
        );
        Ok(Statement::new(
            StatementKind::AssignmentStatement(AssignmentStatement { lhs, rhs }),
            self.get_id()?,
            span,
        ))
    }

    /// Parses an expression in normal mode.
    #[inline]
    fn expression_normal(&mut self, recursion: u32) -> Result<Expression> {
        self.expression(BlockMode::Normal, recursion)
    }

    /// Parses an expression in no block mode.
    #[inline]
    fn expression_no_blocks(&mut self, recursion: u32) -> Result<Expression> {
        self.expression(BlockMode::NoBlocks, recursion)
    }

    /// Parses an expression.
    fn expression(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression> {
        self.comparison(mode, recursion)
    }

    /// Parses a comparison expression.
    fn comparison(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression> {
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

    /// Parses a term expression.
    fn term(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression> {
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

    /// Parses a factor expression.
    fn factor(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression> {
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

    /// Parses a binary expression.
    #[inline]
    fn binary_expr(
        &mut self,
        tokens: &[(TokenKind, BinOp)],
        next: fn(&mut Self, BlockMode, u32) -> Result<Expression>,
        comparison: bool,
        mode: BlockMode,
        recursion: u32,
    ) -> Result<Expression> {
        self.check_failed(recursion)?;

        let mut lhs = next(self, mode, recursion + 1)?;
        let mut ops = Vec::new();
        while let Some(peeked) = self.peek() {
            let Some(&(_, op)) = tokens.iter().find(|&&(tok, _)| peeked.kind == tok) else {
                break;
            };

            self.advance(); // Skip operator
            let rhs = next(self, mode, recursion + 1)?;
            let span = Span::new_from(lhs.span.start, rhs.span.end, self.file);
            lhs = Expression::new(
                ExpressionKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                self.get_id()?,
                span,
            );

            if comparison
                && let Some(peeked2) = self.peek()
                && tokens.iter().any(|&(tok, _)| peeked2.kind == tok)
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

    /// Parses a unary expression.
    fn unary(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression> {
        self.check_failed(recursion)?;

        let Some(op) = self.eat(&[TokenKind::Minus, TokenKind::Bang]) else {
            return self.postfix(mode, recursion + 1);
        };
        let rhs = self.unary(mode, recursion + 1)?;
        let span = Span::new_from(op.span.start, rhs.span.end, self.file);
        let kind = match op.kind {
            TokenKind::Minus => ExpressionKind::Unary(UnOp::Neg, Box::new(rhs)),
            TokenKind::Bang => ExpressionKind::Unary(UnOp::Not, Box::new(rhs)),
            // `eat` will only ever return tokens with the input types, which are all arms.
            _ => unreachable!(), // CHECKED(Chloe)
        };
        Ok(Expression::new(kind, self.get_id()?, span))
    }

    /// Parses a postfix expression.
    fn postfix(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression> {
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

    /// Parses a indexing expression.
    fn parse_index(&mut self, lhs: Expression, recursion: u32) -> Result<Expression> {
        self.advance();

        let expr = self.parse_sync(
            Self::expression_normal,
            &[TokenKind::RBracket],
            "expression",
            recursion,
        );
        let closing_res = self.expect_kinds(&[TokenKind::RBracket]);
        let closing = self.report_error(closing_res, "']'");
        let span = Span::new_from(
            lhs.span.start,
            closing.map_or_else(|| self.loc(), |tok| tok.span.end),
            self.file,
        );

        Ok(Expression::new(
            ExpressionKind::Index(Box::new(lhs), expr.map(Box::new)),
            self.get_id()?,
            span,
        ))
    }

    /// Parses a call expression.
    fn parse_call(&mut self, lhs: Expression, recursion: u32) -> Result<Expression> {
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
            lhs.span.start,
            closing.map_or_else(|| self.loc(), |tok| tok.span.end),
            self.file,
        );
        Ok(Expression::new(
            ExpressionKind::Call(Box::new(lhs), args),
            self.get_id()?,
            span,
        ))
    }

    /// Parses a member access expression.
    fn parse_access(&mut self, lhs: Expression) -> Result<Expression> {
        self.advance();
        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let span = Span::new_from(
            lhs.span.start,
            ident.map_or_else(|| self.loc(), |tok| tok.span.end),
            self.file,
        );
        Ok(Expression::new(
            ExpressionKind::Member(
                Box::new(lhs),
                ident.map(|ident| self.global.interner.intern(Symbol::new(ident))),
            ),
            self.get_id()?,
            span,
        ))
    }

    /// Parses a grouping expression.
    fn grouping(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression> {
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
            ExpressionKind::Grouping(inner.map(Box::new)),
            self.get_id()?,
            Span::new_from(
                opening.span.start,
                closing.map_or_else(|| self.loc(), |tok| tok.span.end),
                self.file,
            ),
        );
        Ok(expr)
    }

    /// Parses a block expression.
    fn block_expr(&mut self, mode: BlockMode, recursion: u32) -> Result<Expression> {
        self.check_failed(recursion)?;

        if !mode.normal()
            || self
                .peek()
                .as_ref()
                .is_none_or(|t| t.kind != TokenKind::LBrace)
        {
            return self.global_ident(recursion);
        }

        let block = self.block(recursion)?;
        let span = block.span;

        Ok(Expression::new(
            ExpressionKind::Block(Box::new(block)),
            self.get_id()?,
            span,
        ))
    }

    /// Parses a global identifier.
    fn global_ident(&mut self, recursion: u32) -> Result<Expression> {
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
                ExpressionKind::GlobalIdent(None),
                self.get_id()?,
                Span::new_from(dot.span.start, tok.span.end, self.file),
            ));
        }

        let ident = self.expect_report(&[TokenKind::Ident], "identifier");
        let span = Span::new_from(
            dot.span.start,
            ident
                .as_ref()
                .map_or_else(|| self.loc(), |ident| ident.span.end),
            self.file,
        );
        Ok(Expression::new(
            ExpressionKind::GlobalIdent(
                ident.map(|ident| self.global.interner.intern(Symbol::new(ident))),
            ),
            self.get_id()?,
            span,
        ))
    }

    /// Parses a primary expression.
    fn primary(&mut self, recursion: u32) -> Result<Expression> {
        self.check_failed(recursion)?;

        let tok = self.expect_peek(&[
            TokenKind::IntLit,
            TokenKind::FloatLit,
            TokenKind::Ident,
            TokenKind::StringLit,
            TokenKind::Func,
        ])?;
        let primary = match tok.kind {
            TokenKind::IntLit => ExpressionKind::IntLit(tok.lexeme),
            TokenKind::FloatLit => ExpressionKind::FloatLit(tok.lexeme),
            TokenKind::Ident => {
                ExpressionKind::Ident(self.global.interner.intern(Symbol::new(tok)))
            }
            TokenKind::StringLit => ExpressionKind::StringLit(tok.lexeme),
            TokenKind::Func => return self.function_type_and_expr(recursion + 1),
            // `expect_peek` will only return token kinds of the inputs, and all the inputs are arms.
            _ => unreachable!(), // CHECKED(Chloe)
        };
        self.advance();
        let expr = Expression::new(primary, self.get_id()?, tok.span);

        Ok(expr)
    }

    /// Parses a function type, or a function expression.
    fn function_type_and_expr(&mut self, recursion: u32) -> Result<Expression> {
        self.check_failed(recursion)?;

        let func = self.expect_kinds(&[TokenKind::Func])?;

        let _opening = self.expect_report(&[TokenKind::LParen], "'('");

        let params = self.param_list_required_ident(recursion + 1)?;

        let _closing = self.expect_report(&[TokenKind::RParen], "')'");
        let ret_type = if self
            .peek()
            .is_some_and(|tok| tok.kind != TokenKind::LBrace && tok.kind != TokenKind::Semicolon)
        {
            TriState::from_error(
                self.parse_sync(
                    Self::expression_no_blocks,
                    &[TokenKind::LBrace, TokenKind::Semicolon],
                    "type",
                    recursion + 1,
                )
                .map(Box::new),
            )
        } else {
            TriState::None
        };

        let next = self.expect_peek_all()?;

        if next.kind == TokenKind::LBrace {
            let block = self.block(recursion + 1)?;
            let span = Span::new_from(func.span.start, block.span.end, self.file);

            Ok(Expression {
                kind: ExpressionKind::Function(Function {
                    params,
                    ret_type,
                    block: Box::new(block),
                }),
                id: self.get_id()?,
                span,
            })
        } else {
            let semi = self.expect_report(&[TokenKind::Semicolon], ";");

            let param_types = params.into_iter().map(|(_, ty)| ty).collect::<ThinVec<_>>();

            Ok(Expression {
                kind: ExpressionKind::FunctionType(FunctionType {
                    params: param_types,
                    ret_type,
                }),
                id: self.get_id()?,
                span: Span::new_from(
                    func.span.start,
                    semi.map_or_else(|| self.loc(), |tok| tok.span.end),
                    self.file,
                ),
            })
        }
    }
}

insta_test!(parser_test, "parser-tests/*.tck", run_parser_test);

#[cfg(test)]
use std::path::Path;

#[cfg(test)]
fn run_parser_test(src: String) {
    use crate::{file::File, frontend::lexer::Lexer};

    let file = File::new(src, Path::new("testing.tck"));
    let global = Global::create_heap();
    let lexer = Lexer::new(&file, &global);
    let tokens = lexer
        .map(|res| {
            // Panic on error
            res.unwrap() // CHECKED(Chloe)
        })
        .collect::<Vec<_>>();
    let (prog, err, _) = Parser::parse(&tokens, &file, &global);
    insta::assert_ron_snapshot!((prog, err));
}
