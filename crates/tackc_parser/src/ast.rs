use std::fmt::Debug;
use std::hash::Hash;

use crate::error::Result;
use tackc_global::{Global, Interned};
use tackc_lexer::Token;
use tackc_span::Span;

use crate::Parser;

#[macro_export]
macro_rules! token_kind {
    ($pat:pat) => {
        |kind| matches!(kind, $pat)
    };
}

#[cfg(feature = "serde")]
pub trait Serde: serde::Serialize + for<'a> serde::Deserialize<'a> {}
#[cfg(feature = "serde")]
impl<T: serde::Serialize + for<'a> serde::Deserialize<'a>> Serde for T {}
#[cfg(not(feature = "serde"))]
pub trait Serde {}
#[cfg(not(feature = "serde"))]
impl<T> Serde for T {}

pub trait AstNode: Debug + PartialEq + Eq + Hash + Sized + Serde {
    /// Parse the AST node using the given parser.
    ///
    /// # Errors
    /// If the node cannot be parsed from the given parser, this function will return an error.
    /// The parser may be in any state; do not trust it.
    /// If you need to, the parser can be cloned previous to calling this function.
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone;
    fn span(&self) -> Span;
    fn display(&self, global: &Global) -> String;
}

mod expr;
pub use expr::*;

mod stmt;
pub use stmt::*;

mod block;
pub use block::*;

mod item;
pub use item::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Symbol {
    pub span: Span,
    pub ident: Interned<str>,
}

impl Symbol {
    pub fn new(span: Span, ident: Interned<str>) -> Self {
        Symbol { span, ident }
    }

    pub fn display<'a>(&self, global: &'a Global) -> &'a str {
        self.ident.display(global)
    }
}
