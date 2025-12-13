use serde::{Deserialize, Serialize};
use tackc_span::Span;

use crate::{Expression, NodeId, Statement};

/// A block containing statements, and an optional expression.
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Block {
    #[allow(missing_docs)]
    pub span: Span,
    /// A list of statements in this block
    pub stmts: Vec<Statement>,
    /// The last expression of this block
    pub expr: Option<Expression>,
    #[allow(missing_docs)]
    pub id: NodeId,
}
