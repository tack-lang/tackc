use tackc_global::Global;

use crate::{
    NodeId,
    ast::{Expression, Symbol},
};

#[derive(Debug, PartialEq, Eq)]
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
                format!("const {ident}{ty} = {expr};")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemKind {
    ConstItem(Box<ConstItem>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConstItem {
    pub ty: Option<Option<Expression>>,
    pub expr: Option<Expression>,
    pub ident: Option<Symbol>,
}
