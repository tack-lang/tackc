use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{Block, Expression, NodeId, Symbol};

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Item {
    ConstItem(ConstItem),
    FuncItem(FuncItem),
    // When adding to `Item`, update `prog::sync_item`.
}

/// A constant declaration
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ConstItem {
    #[allow(missing_docs)]
    pub span: Span,
    /// The identifier being bound to
    pub ident: Symbol,
    /// The type annotation given
    pub ty: Option<Expression>,
    /// The value given
    pub expr: Expression,
    #[allow(missing_docs)]
    pub id: NodeId,
}

/// A function declaration
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FuncItem {
    #[allow(missing_docs)]
    pub span: Span,
    /// The name of this function
    pub ident: Symbol,
    /// The parameters of this function
    pub params: Vec<(Symbol, Expression)>,
    /// The return type of this function
    pub ret_ty: Option<Expression>,
    /// The code of this function
    pub block: Block,
    #[allow(missing_docs)]
    pub id: NodeId,
}
