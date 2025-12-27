use std::num::NonZeroU32;

use serde::{Deserialize, Serialize};
use tackc_global::{Global, Interned};
use tackc_lexer::Token;
use tackc_span::Span;

pub mod expr;
pub use expr::*;

pub mod item;
pub use item::*;

pub mod stmt;
pub use stmt::*;

pub mod block;
pub use block::*;

pub mod prog;
pub use prog::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Symbol(pub Interned<str>, pub Span);

impl Symbol {
    pub fn display<'a>(&self, global: &'a Global) -> &'a str {
        self.0.display(global)
    }
}

impl From<Token> for Symbol {
    fn from(value: Token) -> Self {
        Self(value.lexeme, value.span)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct NodeId {
    pub id: NonZeroU32,
    pub file: NonZeroU32,
}
