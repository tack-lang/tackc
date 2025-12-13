mod expr;
pub use expr::*;

mod stmt;
use serde::{Deserialize, Serialize};
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
use tackc_global::{Global, Interned};
use tackc_span::Span;
pub use util::*;

/// An index types for Node IDs.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct NodeId(pub u64);

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

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Binding {
    pub span: Span,
    pub ty_annotation: Option<NodeId>,
}
