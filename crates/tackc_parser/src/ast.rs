use std::fmt::Debug;
use std::hash::Hash;

use crate::error::Result;
use tackc_global::Global;
use tackc_lexer::Token;
use tackc_span::Span;

use crate::Parser;

#[cfg(feature = "serde")]
pub trait Serde: serde::Serialize + for<'a> serde::Deserialize<'a> {}
#[cfg(feature = "serde")]
impl<T: serde::Serialize + for<'a> serde::Deserialize<'a>> Serde for T {}
#[cfg(not(feature = "serde"))]
pub trait Serde {}
#[cfg(not(feature = "serde"))]
impl<T> Serde for T {}

pub trait AstNode: Debug + PartialEq + Eq + Hash + Clone + Sized + Serde {
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
