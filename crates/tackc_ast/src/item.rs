use serde::{Deserialize, Serialize};
use tackc_global::Global;

use crate::{
    NodeId,
    Block, Expression, Path, Symbol,
};

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Item {
    pub kind: ItemKind,
    pub id: NodeId,
}

impl Item {
    pub const fn new(kind: ItemKind, id: NodeId) -> Self {
        Self { kind, id }
    }

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
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ItemKind {
    ConstItem(Box<ConstItem>),
    FuncItem(Box<FuncItem>),
    ImpItem(Box<ImpItem>),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ConstItem {
    pub exported: bool,
    pub ty: Option<Option<Expression>>,
    pub expr: Option<Expression>,
    pub ident: Option<Symbol>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FuncItem {
    pub exported: bool,
    pub ident: Option<Symbol>,
    pub params: Vec<(Option<Symbol>, Option<Expression>)>,
    pub ret_type: Option<Option<Expression>>,
    pub block: Option<Block>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImpItem {
    pub exported: bool,
    pub path: Option<Path>,
}
