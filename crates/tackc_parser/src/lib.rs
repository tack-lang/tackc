//! `tackc`'s parser.

/// AST nodes
pub mod ast;
/// Error types for the parser
pub mod error;

use error::{ParseError, ParseErrors, Result};

use tackc_ast::{NodeId, Symbol};
use tackc_file::File;
use tackc_global::{Global, Internable, Interned};
use tackc_lexer::{Token, TokenKind};

use crate::ast::AstNode;

/// The largest possible recursion depth. If a reasonable file exceeds this limit, please open an issue or a pull request.
pub const MAX_RECURSION_DEPTH: u32 = 256;

/// This struct contains a snapshotted version of [`Parser`].
/// This can be created by [`Parser::snapshot`], and restored by [`Parser::restore`].
pub struct ParserSnapshot<I>(I, u64);

/// The parser struct, containing a stream of tokens, a [`Global`] reference, and the first open `AstNode` ID.
pub struct Parser<'a, I, F> {
    iter: I,
    file: &'a F,
    global: &'a Global,
    open_id: u64,
}

impl<'a, I, F: File> Parser<'a, I, F>
where
    I: Iterator<Item = Token> + Clone,
{
    /// Create a snapshot of this parser
    #[inline]
    pub fn snapshot(&self) -> ParserSnapshot<I> {
        ParserSnapshot(self.iter.clone(), self.open_id)
    }

    /// Restore a snapshot of this parser
    #[inline]
    pub fn restore(&mut self, snapshot: ParserSnapshot<I>) {
        *self = Parser {
            iter: snapshot.0,
            file: self.file,
            global: self.global,
            open_id: snapshot.1,
        };
    }

    pub fn collect_error(&mut self, errors: &mut Option<ParseErrors>, e: ParseErrors) {
        if let Some(err) = errors {
            err.merge(e);
        } else {
            *errors = Some(e);
        }
    }

    /// Try to run `op` by passing `self`. On error, restore parser to before trying to run `op`.
    ///
    /// # Errors
    /// This function returns the same value as `op`, so it will only return an error if `op` returns an error.
    pub fn try_run<T, O: FnOnce(&mut Self) -> Result<T>>(&mut self, op: O) -> Result<T> {
        let snapshot = self.snapshot();
        let res = op(self);
        if res.is_err() {
            self.restore(snapshot);
        }
        res
    }

    /// Create a new parser
    #[inline]
    pub fn new(iter: I, global: &'a Global, file: &'a F) -> Self {
        Parser {
            iter,
            file,
            global,
            open_id: 0,
        }
    }

    pub fn intern<T: Internable>(&self, val: T) -> Interned<T> {
        self.global.intern(val)
    }

    /// Returns the next open [`NodeId`], and increments the open ID.
    pub fn node_id(&mut self) -> NodeId {
        let old = self.open_id;
        self.open_id += 1;
        NodeId(old)
    }

    /// Returns true if the inner token stream has reached an end of file.
    pub fn is_eof(&self) -> bool {
        self.peek_token().is_none()
    }

    /// Attempts to parse an `AstNode` from the given parser.
    ///
    /// # Errors
    /// This function will return an error when it fails to parse the `AstNode`.
    #[inline]
    pub fn parse<N: AstNode>(&mut self, recursion: u32) -> Result<N> {
        // Don't add one, inlined
        N::parse(self, recursion)
    }

    /// Attempts to parse an `AstNode` from the given parser. On failure, the parser will backtrack to before the failed node.
    ///
    /// # Errors
    /// This function will return an error when it fails to parse the `AstNode`.
    pub fn try_parse<N: AstNode>(&mut self, recursion: u32) -> Result<N> {
        self.try_run(|p| N::parse(p, recursion + 1))
    }

    /// Consumes the next token from the lexer.
    pub fn next_token(&mut self) -> Option<Token> {
        self.iter.next()
    }

    /// Gets the next token from the lexer, without consuming it.
    pub fn peek_token(&self) -> Option<Token> {
        self.iter.clone().next()
    }

    /// This function can be used to ensure `recursion` is below the maximum recursion depth.
    ///
    /// # Errors
    /// This function returns a recursion error if `recursion` is greater than [`MAX_RECURSION_DEPTH`].
    pub fn check_recursion(&self, recursion: u32) -> Result<()> {
        if recursion > MAX_RECURSION_DEPTH {
            Err(ParseErrors::new(ParseError::recursion()))
        } else {
            Ok(())
        }
    }

    /// Peeks a token, and if `callback(token.kind) == true`, consumes the token and returns true. Otherwise, returns false.
    pub fn consume<K>(&mut self, callback: K) -> Option<Token>
    where
        K: FnOnce(TokenKind) -> bool,
    {
        if self.peek_is(callback) {
            self.next_token()
        } else {
            None
        }
    }

    /// Peeks a token, and if `callback(token.kind) == true`, returns true. Otherwise, returns false.
    pub fn peek_is<K>(&self, callback: K) -> bool
    where
        K: FnOnce(TokenKind) -> bool,
    {
        let tok = self.peek_token();
        if let Some(tok) = tok
            && callback(tok.kind)
        {
            return true;
        }

        false
    }

    /// Consumes the next token, returning an 'unexpected EOF' error on failure. If `callback(token.kind) == false`, return an error where expected = `expected`.
    ///
    /// # Errors
    /// This function returns an error if the lexer is at the EOF, or if `callback(token.kind) == false`.
    pub fn expect_token_kind<K>(
        &mut self,
        expected: Option<&'static str>,
        callback: K,
    ) -> Result<Token>
    where
        K: FnOnce(TokenKind) -> bool,
    {
        let tok = self.expect_token(expected)?;

        if callback(tok.kind) {
            Ok(tok)
        } else {
            Err(ParseErrors::new(ParseError::new(expected, tok)))
        }
    }

    /// Consumes the next token, and if it's not an identifier, return an error.
    ///
    /// # Errors
    /// This function returns an error if the next token isn't an identifier.
    pub fn identifier(&mut self) -> Result<Symbol> {
        let Token {
            span,
            kind: TokenKind::Ident(ident),
        } = self.expect_token_kind(Some("identifier"), kind!(TokenKind::Ident(_)))?
        else {
            unreachable!()
        };
        Ok(Symbol::new(span, ident))
    }

    /// Consumes the next token, returning an 'unexpected EOF' error on failure.
    ///
    /// # Errors
    /// This function returns an error if the lexer is at the EOF.
    pub fn expect_token(&mut self, expected: Option<&'static str>) -> Result<Token> {
        self.iter
            .next()
            .ok_or_else(|| ParseErrors::new(ParseError::eof(expected)))
    }

    /// Gets the next token, without consuming it, returning an 'unexpected EOF' error on failure.
    ///
    /// # Errors
    /// This function returns an error if the lexer is at the EOF.
    pub fn expect_peek_token(&self, expected: Option<&'static str>) -> Result<Token> {
        self.iter
            .clone()
            .next()
            .ok_or_else(|| ParseErrors::new(ParseError::eof(expected)))
    }
}
