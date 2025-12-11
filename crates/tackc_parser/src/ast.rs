use std::fmt::Debug;
use std::hash::Hash;

use crate::error::Result;
use serde::{Deserialize, Serialize};
use tackc_global::{Global, Interned};
use tackc_lexer::Token;
use tackc_span::Span;

use crate::Parser;

/// Returns a closure that takes one value, and returns whether the value matches the given pattern.
#[macro_export]
macro_rules! kind {
    ($pat:pat) => {
        |kind| matches!(kind, $pat)
    };
}

/// This trait is implemented by all AST nodes.
pub trait AstNode:
    Debug + PartialEq + Eq + Hash + Sized + Serialize + for<'a> Deserialize<'a>
{
    /// Parse the AST node using the given parser.
    ///
    /// # Errors
    /// If the node cannot be parsed from the given parser, this function will return an error.
    /// The parser may be in any state; do not trust it.
    /// If you need to, the parser can be cloned previous to calling this function.
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone;
    /// Get the span of the AST node
    fn span(&self) -> Span;
    /// Display the AST node
    fn display(&self, global: &Global) -> String;
    /// Get the ID of the AST node
    fn id(&self) -> NodeId;
}

/// An index types for Node IDs.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct NodeId(pub(super) u64);

mod expr;
pub use expr::*;

mod stmt;
pub use stmt::*;

mod block;
pub use block::*;

mod item;
pub use item::*;

mod prog;
pub use prog::*;

mod prim;
pub use prim::*;

mod util;
pub use util::*;

/// Representation of a symbol in the file. This contains a [`Span`], and an [`Interned<str>`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Symbol {
    /// Span of the symbol
    pub span: Span,
    /// Value of the symbol, interned
    pub inner: Interned<str>,
}

impl Symbol {
    /// Create a new symbol
    pub fn new(span: Span, ident: Interned<str>) -> Self {
        Symbol { span, inner: ident }
    }

    /// Display the symbol
    pub fn display<'a>(&self, global: &'a Global) -> &'a str {
        self.inner.display(global)
    }
}
