use tackc_global::Interned;
use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{Binding, Block, Expression, Maybe, NodeId, Symbol};

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
    /// The binding for this const declaration
    pub binding: Option<Interned<Binding>>,
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
    pub params: Vec<(Symbol, Option<Expression>, Option<Interned<Binding>>)>,
    /// The return type of this function
    pub ret_ty: Maybe<Expression>,
    /// The code of this function
    pub block: Block,
    /// The binding of this function declaration
    pub binding: Option<Interned<Binding>>,
    #[allow(missing_docs)]
    pub id: NodeId,
}
