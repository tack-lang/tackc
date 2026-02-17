//! Items in tackc.

use std::borrow::Cow;

use crate::{global::Global, utils::tree::TreeItem};
use serde::{Deserialize, Serialize};

use crate::ast::{AstPath, Block, Expression, NodeId, Symbol};

/// An item.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Item {
    /// The kind of item this is.
    pub kind: ItemKind,
    /// The ID of this item.
    pub id: NodeId,
}

impl Item {
    /// Create a new item using an item kind, and an ID.
    pub const fn new(kind: ItemKind, id: NodeId) -> Self {
        Self { kind, id }
    }

    /// Display the item.
    pub fn display(&self, global: &Global) -> String {
        match &self.kind {
            ItemKind::ConstItem(item) => {
                let ident = item
                    .ident
                    .map_or_else(|| "<ERROR>", |ident| ident.display(global));
                let ty = match &item.ty {
                    Some(Some(ty)) => format!(": {}", ty.display(global)),
                    Some(None) => String::from(": <ERROR>"),
                    None => String::new(),
                };
                let expr = item
                    .expr
                    .as_ref()
                    .map_or_else(|| String::from("<ERROR>"), |expr| expr.display(global));
                format!(
                    "{}const {ident}{ty} = {expr};",
                    if item.exported { "exp " } else { "" }
                )
            }
            ItemKind::FuncItem(item) => {
                let exp = if item.exported { "exp " } else { "" };
                let ident = item.ident.map_or("<ERROR>", |ident| ident.display(global));
                let params = item
                    .params
                    .iter()
                    .map(|(ident, ty)| {
                        format!(
                            "{}: {}",
                            ident.map_or("<ERROR>", |ident| ident.display(global)),
                            ty.as_ref().map_or_else(
                                || String::from("<ERROR>"),
                                |expr| expr.display(global)
                            )
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let block = item
                    .block
                    .as_ref()
                    .map_or_else(|| String::from("<ERROR>"), |block| block.display(global));

                format!("{exp}func {ident}({params}) {block}")
            }
            ItemKind::ImpItem(item) => {
                let exp = if item.exported { "exp " } else { "" };
                let path = item
                    .path
                    .as_ref()
                    .map_or_else(|| String::from("<ERROR>"), |path| path.display(global));

                format!("{exp}imp {path};")
            }
        }
    }

    /// Displays the 'identifier' version of this item. This includes whether the item is exported, the kind of item it is, and it's name or path.
    pub fn display_ident(&self, global: &Global) -> String {
        match &self.kind {
            ItemKind::ConstItem(item) => {
                (if item.exported {
                    String::from("exp ")
                } else {
                    String::new()
                }) + "const "
                    + item
                        .ident
                        .as_ref()
                        .map_or("<ERROR>", |sym| sym.display(global))
            }
            ItemKind::FuncItem(item) => {
                (if item.exported {
                    String::from("exp ")
                } else {
                    String::new()
                }) + "func "
                    + item
                        .ident
                        .as_ref()
                        .map_or("<ERROR>", |sym| sym.display(global))
            }
            ItemKind::ImpItem(item) => {
                (if item.exported {
                    String::from("exp ")
                } else {
                    String::new()
                }) + "imp "
                    + item
                        .path
                        .as_ref()
                        .map_or_else(|| String::from("<ERROR>"), |sym| sym.display(global))
                        .as_str()
            }
        }
    }
}

impl TreeItem for Item {
    fn children(&self) -> Vec<&'_ dyn TreeItem> {
        vec![]
    }

    fn name<'a>(&'a self, global: &'a Global) -> Cow<'a, str> {
        self.display_ident(global).into()
    }
}

/// Different kinds of items.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ItemKind {
    /// Constant definition.
    ConstItem(Box<ConstItem>),
    /// Function definition.
    FuncItem(Box<FuncItem>),
    /// Import declaration.
    ImpItem(Box<ImpItem>),
}

/// Constant definition.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ConstItem {
    /// Whether this item is exported.
    pub exported: bool,
    /// The optional type annotation of this definition.
    pub ty: Option<Option<Expression>>,
    /// The expression of this definition.
    pub expr: Option<Expression>,
    /// The identifier used for this definition.
    pub ident: Option<Symbol>,
}

/// Function definition.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FuncItem {
    /// Whether this item is exported.
    pub exported: bool,
    /// The identifier of this function.
    pub ident: Option<Symbol>,
    /// The parameters for this function.
    pub params: Vec<(Option<Symbol>, Option<Expression>)>,
    /// The return type of this function.
    pub ret_type: Option<Option<Expression>>,
    /// The block for this function.
    pub block: Option<Block>,
}

/// Import declaration.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImpItem {
    /// Whether this item is exported.
    pub exported: bool,
    /// The path to be imported.
    pub path: Option<AstPath>,
}
