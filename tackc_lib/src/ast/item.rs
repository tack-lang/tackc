//! Items in tackc.

use std::borrow::Cow;
use std::fmt::Write;

use crate::{
    global::{Global, Interned},
    utils::tree::TreeItem,
};
use serde::Serialize;
use thin_vec::ThinVec;

use crate::ast::{AstPath, Block, Expression, NodeId, Symbol};

/// An item.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Item<'src> {
    /// The kind of item this is.
    pub kind: &'src ItemKind<'src>,
    /// The ID of this item.
    pub id: NodeId,
}

impl<'src> Item<'src> {
    /// Create a new item using an item kind, and an ID.
    pub const fn new(kind: &'src ItemKind<'src>, id: NodeId) -> Self {
        Self { kind, id }
    }

    /// Display the item.
    pub fn display(&self, global: &Global) -> String {
        match &self.kind {
            ItemKind::ConstItem(item) => item.display(global),
            ItemKind::FuncItem(item) => item.display(global),
            ItemKind::ImpItem(item) => item.display(global),
        }
    }

    /// Displays the 'identifier' version of this item. This includes whether the item is exported, the kind of item it is, and it's name or path.
    pub fn display_ident(&self, global: &Global) -> String {
        match &self.kind {
            ItemKind::ConstItem(item) => item.display_ident(global),
            ItemKind::FuncItem(item) => item.display_ident(global),
            ItemKind::ImpItem(item) => item.display_ident(global),
        }
    }
}

impl TreeItem for Item<'_> {
    fn children(&self) -> Vec<&'_ dyn TreeItem> {
        vec![]
    }

    fn name<'a>(&'a self, global: &'a Global) -> Cow<'a, str> {
        self.display_ident(global).into()
    }
}

/// Different kinds of items.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum ItemKind<'src> {
    /// Constant definition.
    ConstItem(&'src ConstItem<'src>),
    /// Function definition.
    FuncItem(&'src FuncItem<'src>),
    /// Import declaration.
    ImpItem(&'src ImpItem<'src>),
}

/// Constant definition.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ConstItem<'src> {
    /// Whether this item is exported.
    pub exported: bool,
    /// The optional type annotation of this definition.
    pub ty: Option<Option<&'src Expression<'src>>>,
    /// The expression of this definition.
    pub expr: Option<&'src Expression<'src>>,
    /// The identifier used for this definition.
    pub ident: Option<Interned<Symbol>>,
}

impl ConstItem<'_> {
    fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let ident = match self.ident {
            Some(ident) => ident.get(global).display(global),
            None => "<ERROR>",
        };
        let ty = match &self.ty {
            Some(Some(ty)) => format!(": {}", ty.display(global)),
            Some(None) => String::from(": <ERROR>"),
            None => String::new(),
        };
        let expr = match self.expr {
            Some(expr) => expr.display(global),
            None => String::from("<ERROR>"),
        };
        format!("{exp}const {ident}{ty} = {expr};")
    }

    fn display_ident(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let ident = match self.ident {
            Some(sym) => sym.get(global).display(global),
            None => "<ERROR>",
        };

        format!("{exp}const {ident}")
    }
}

/// Function definition.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct FuncItem<'src> {
    /// Whether this item is exported.
    pub exported: bool,
    /// The identifier of this function.
    pub ident: Option<Interned<Symbol>>,
    /// The parameters for this function.
    pub params: ThinVec<(Option<Interned<Symbol>>, Option<&'src Expression<'src>>)>,
    /// The return type of this function.
    pub ret_type: Option<Option<&'src Expression<'src>>>,
    /// The block for this function.
    pub block: Option<&'src Block<'src>>,
}

impl FuncItem<'_> {
    fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let ident = match self.ident {
            Some(ident) => ident.get(global).display(global),
            None => "<ERROR>",
        };
        let mut params = String::new();
        for (ident, ty) in &self.params {
            let ident = match ident {
                Some(ident) => ident.get(global).display(global),
                None => "<ERROR>",
            };
            let ty = match ty {
                Some(expr) => expr.display(global),
                None => String::from("<ERROR>"),
            };
            _ = write!(params, "{ident}: {ty}, ");
        }

        params.truncate(params.len().saturating_sub(2));

        let block = match self.block {
            Some(block) => block.display(global),
            None => String::from("<ERROR>"),
        };

        format!("{exp}func {ident}({params}) {block}")
    }

    fn display_ident(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let ident = match self.ident {
            Some(sym) => sym.get(global).display(global),
            None => "<ERROR>",
        };

        format!("{exp}func {ident}")
    }
}

/// Import declaration.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ImpItem<'src> {
    /// Whether this item is exported.
    pub exported: bool,
    /// The path to be imported.
    pub path: Option<&'src AstPath>,
}

impl ImpItem<'_> {
    fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let path = match self.path {
            Some(path) => path.display(global),
            None => String::from("<ERROR>"),
        };

        format!("{exp}imp {path};")
    }

    fn display_ident(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let path = match self.path {
            Some(sym) => sym.display(global),
            None => String::from("<ERROR>"),
        };

        format!("{exp}imp {path}")
    }
}
