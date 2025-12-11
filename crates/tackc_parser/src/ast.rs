use std::fmt::Debug;
use std::hash::Hash;

use crate::error::Result;
use serde::{Deserialize, Serialize};
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
    fn span(&self) -> Span;
    fn display(&self, global: &Global) -> String;
    fn id(&self) -> NodeId;
}

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
