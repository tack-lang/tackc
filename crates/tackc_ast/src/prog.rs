use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{Item, NodeId, Path};

/// A full program
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Program {
    #[allow(missing_docs)]
    pub span: Span,
    /// The initial mod statement for this program.
    pub mod_stmt: ModStatement,
    /// The items of this program
    pub items: Vec<Item>,
    #[allow(missing_docs)]
    pub id: NodeId,
}

/// The mod statement of a program
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModStatement {
    #[allow(missing_docs)]
    pub span: Span,
    /// The path of this mod statement
    pub path: Box<Path>,
    #[allow(missing_docs)]
    pub id: NodeId,
}
