use tackc_global::{Global, Interned};
use tackc_lexer::Token;
use tackc_span::Span;

pub mod expr;
pub use expr::*;

pub mod item;
pub use item::*;

pub mod stmt;
pub use stmt::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
