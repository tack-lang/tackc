use serde::{Deserialize, Serialize};
use tackc_global::Interned;
use tackc_span::Span;

use crate::{Binding, NodeId, Symbol};

/// A path to an item
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Path {
    #[allow(missing_docs)]
    pub span: Span,
    /// The components of this path, seperated by `.`
    pub components: Vec<(Symbol, Option<Interned<Binding>>)>,
    #[allow(missing_docs)]
    pub id: NodeId,
}
