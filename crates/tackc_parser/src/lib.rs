pub mod ast;
pub mod error;

use std::sync::atomic::{AtomicU32, Ordering};

use error::{ParseError, ParseErrors, Result};

use tackc_lexer::Token;

use crate::ast::AstNode;

pub static MAX_RECURSION_DEPTH: AtomicU32 = AtomicU32::new(256);

pub fn set_max_recursion_depth(depth: u32) {
    MAX_RECURSION_DEPTH.store(depth, Ordering::Release);
}

pub struct ParserSnapshot<I>(I);

pub struct Parser<I> {
    iter: I,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token> + Clone,
{
    #[inline]
    pub fn snapshot(&self) -> ParserSnapshot<I> {
        ParserSnapshot(self.iter.clone())
    }

    #[inline]
    pub fn restore(&mut self, snapshot: ParserSnapshot<I>) {
        self.iter = snapshot.0;
    }

    #[inline]
    pub fn new(iter: I) -> Self {
        Parser { iter }
    }

    /// Attempts to parse an `AstNode` from the given parser. On failure, the parser will backtrack to before the failed node.
    ///
    /// # Errors
    /// This function will return an error when it fails to parse the `AstNode`.
    pub fn try_parse<N: AstNode>(&mut self, recursion: u32) -> Result<N> {
        let snapshot = self.snapshot();
        let res = N::parse(self, recursion + 1);
        if res.is_err() {
            self.restore(snapshot);
        }
        res
    }

    /// Consumes the next token from the lexer.
    pub fn next_token(&mut self) -> Option<Token> {
        self.iter.next()
    }

    /// Gets the next token from the lexer, without consuming it.
    pub fn peek_token(&self) -> Option<Token> {
        self.iter.clone().next()
    }

    /// Returns a recursion error if `recursion` is greater than [`MAX_RECURSION_DEPTH`].
    #[allow(clippy::missing_errors_doc)]
    pub fn check_recursion(&self, recursion: u32) -> Result<()> {
        if recursion > MAX_RECURSION_DEPTH.load(Ordering::Acquire) {
            Err(ParseErrors::new(ParseError::recursion()))
        } else {
            Ok(())
        }
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
