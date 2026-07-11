//! Items in tackc.

use std::borrow::Cow;
use std::fmt::Write;

use crate::{
    global::{Global, Interned},
    span::Span,
    utils::tree::TreeItem,
};
use serde::Serialize;
use thin_vec::ThinVec;

use crate::frontend::ast::{AstPath, Block, Expression, NodeId, Symbol};

/// An item.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Item {
    /// The kind of item this is.
    pub kind: ItemKind,
    /// The ID of this item.
    pub id: NodeId,
    /// The span of this item.
    pub span: Span,
}

impl Item {
    /// Create a new item using an item kind, and an ID.
    pub const fn new(kind: ItemKind, id: NodeId, span: Span) -> Self {
        Self { kind, id, span }
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

impl TreeItem for Item {
    fn children(&self) -> Vec<&'_ dyn TreeItem> {
        vec![]
    }

    fn name<'a>(&'a self, global: &'a Global) -> Cow<'a, str> {
        self.display_ident(global).into()
    }
}

/// Different kinds of items.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum ItemKind {
    /// Constant definition.
    ConstItem(ConstItem),
    /// Function definition.
    FuncItem(FuncItem),
    /// Import declaration.
    ImpItem(ImpItem),
}

/// Constant definition.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ConstItem {
    /// Whether this item is exported.
    pub exported: bool,
    /// The optional type annotation of this definition.
    pub ty: Option<Option<Expression>>,
    /// The expression of this definition.
    pub expr: Option<Expression>,
    /// The identifier used for this definition.
    pub ident: Option<Interned<Symbol>>,
}

impl ConstItem {
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
        let expr = match &self.expr {
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
pub struct FuncItem {
    /// Whether this item is exported.
    pub exported: bool,
    /// The identifier of this function.
    pub ident: Option<Interned<Symbol>>,
    /// The parameters for this function.
    pub params: ThinVec<(Option<Interned<Symbol>>, Option<Expression>)>,
    /// The return type of this function.
    pub ret_type: Option<Option<Expression>>,
    /// The block for this function.
    pub block: Option<Block>,
}

impl FuncItem {
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

        let block = match &self.block {
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
pub struct ImpItem {
    /// Whether this item is exported.
    pub exported: bool,
    /// The path to be imported.
    pub path: Option<AstPath>,
}

impl ImpItem {
    fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let path = match &self.path {
            Some(path) => path.display(global),
            None => String::from("<ERROR>"),
        };

        format!("{exp}imp {path};")
    }

    fn display_ident(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let path = match &self.path {
            Some(sym) => sym.display(global),
            None => String::from("<ERROR>"),
        };

        format!("{exp}imp {path}")
    }
}
