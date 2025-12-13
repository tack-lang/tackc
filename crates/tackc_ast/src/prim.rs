use tackc_global::Interned;
use tackc_lexer::IntegerBase;
use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{Binding, NodeId, Symbol};

/// A primary expression
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct Primary {
    #[allow(missing_docs)]
    pub span: Span,
    #[allow(missing_docs)]
    pub kind: PrimaryKind,
    #[allow(missing_docs)]
    pub id: NodeId,
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub enum PrimaryKind {
    Binding(Symbol, Option<Interned<Binding>>),
    IntLit(Symbol, IntegerBase),
    FloatLit(Symbol),

    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}
