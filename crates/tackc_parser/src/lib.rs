pub mod ast;
pub mod error;

use error::{ParseError, ParseErrors, Result};

use tackc_lexer::Token;

use crate::ast::AstNode;

pub struct ParserSnapshot<I>(I)
where
    I: Iterator<Item = Token> + Clone;

pub struct Parser<I>
where
    I: Iterator<Item = Token> + Clone,
{
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
    pub fn try_parse<N: AstNode>(&mut self) -> Result<N> {
        let snapshot = self.snapshot();
        let res = N::parse(self);
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
    pub fn peek_token(&mut self) -> Option<Token> {
        self.iter.clone().next()
    }

    /// Consumes the next token, returning an 'unexpected EOF' error on failure.
    ///
    /// # Errors
    /// This function returns an error if the lexer is at the EOF.
    pub fn expect_token(&mut self, expected: &'static str) -> Result<Token> {
        self.iter
            .next()
            .ok_or_else(|| ParseErrors::new(ParseError::eof(expected)))
    }

    /// Gets the next token, without consuming it, returning an 'unexpected EOF' error on failure.
    ///
    /// # Errors
    /// This function returns an error if the lexer is at the EOF.
    pub fn expect_peek_token(&self, expected: &'static str) -> Result<Token> {
        self.iter
            .clone()
            .next()
            .ok_or_else(|| ParseErrors::new(ParseError::eof(expected)))
    }
}
