//! Items in tackc.

use std::{borrow::Cow, fmt::Write};

use serde::Serialize;
use thin_vec::ThinVec;

use crate::{
    frontend::ast::{AstPath, Block, Expression, NodeId, Symbol, TriState},
    global::Global,
    span::Span,
    utils::{intern::Interned, tree::TreeItem},
};

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
        match self.kind {
            ItemKind::ConstItem(ref item) => item.display(global),
            ItemKind::FuncItem(ref item) => item.display(global),
            ItemKind::ImpItem(ref item) => item.display(global),
        }
    }

    /// Displays the 'identifier' version of this item. This includes whether the item is exported, the kind of item it is, and it's name or path.
    pub fn display_ident(&self, global: &Global) -> String {
        match self.kind {
            ItemKind::ConstItem(ref item) => item.display_ident(global),
            ItemKind::FuncItem(ref item) => item.display_ident(global),
            ItemKind::ImpItem(ref item) => item.display_ident(global),
        }
    }

    /// Gets the name of this item. This is the symbol that it's accesible by inside of it's module.
    pub fn get_name(&self, global: &Global) -> Option<Interned<str>> {
        match self.kind {
            ItemKind::ConstItem(ref item) => Some(item.ident?.get(&global.interner).0),
            ItemKind::FuncItem(ref func) => Some(func.ident?.get(&global.interner).0),
            ItemKind::ImpItem(ref imp) => imp.name(global),
        }
    }
}

impl TreeItem for Item {
    fn children(&self) -> Cow<'_, [&dyn TreeItem]> {
        (&[]).into()
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
    pub ty: TriState<Expression>,
    /// The expression of this definition.
    pub expr: Option<Expression>,
    /// The identifier used for this definition.
    pub ident: Option<Interned<Symbol>>,
}

impl ConstItem {
    /// Displays this item.
    pub fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let ident = match self.ident {
            Some(ident) => ident.get(&global.interner).display(global),
            None => "<ERROR>",
        };
        let ty = match self.ty {
            TriState::Some(ref ty) => format!(": {}", ty.display(global)),
            TriState::Error => String::from(": <ERROR>"),
            TriState::None => String::new(),
        };
        let expr = match self.expr {
            Some(ref expr) => expr.display(global),
            None => String::from("<ERROR>"),
        };
        format!("{exp}const {ident}{ty} = {expr};")
    }

    /// Displays the identifier and visibillity of this item.
    pub fn display_ident(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let ident = match self.ident {
            Some(sym) => sym.get(&global.interner).display(global),
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
    pub ret_type: TriState<Expression>,
    /// The block for this function.
    pub block: Option<Block>,
}

impl FuncItem {
    /// Displays this item.
    pub fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let ident = match self.ident {
            Some(ident) => ident.get(&global.interner).display(global),
            None => "<ERROR>",
        };
        let mut params = String::new();
        for &(ident, ref ty) in &self.params {
            let ident = match ident {
                Some(ident) => ident.get(&global.interner).display(global),
                None => "<ERROR>",
            };
            let ty = match *ty {
                Some(ref expr) => expr.display(global),
                None => String::from("<ERROR>"),
            };
            _ = write!(params, "{ident}: {ty}, ");
        }

        params.truncate(params.len().saturating_sub(2));

        let block = match self.block {
            Some(ref block) => block.display(global),
            None => String::from("<ERROR>"),
        };

        format!("{exp}func {ident}({params}) {block}")
    }

    /// Displays the identifier and visibillity of this item.
    pub fn display_ident(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let ident = match self.ident {
            Some(sym) => sym.get(&global.interner).display(global),
            None => "<ERROR>",
        };

        format!("{exp}func {ident}")
    }

    /// Gets the name of this function.
    pub fn get_name(&self, global: &Global) -> Option<Interned<str>> {
        Some(self.ident?.get(&global.interner).0)
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
    /// Displays this item.
    pub fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let path = match self.path {
            Some(ref path) => path.display(global),
            None => String::from("<ERROR>"),
        };

        format!("{exp}imp {path};")
    }

    /// Displays the identifier and visibillity of this item.
    pub fn display_ident(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let path = match self.path {
            Some(ref sym) => sym.display(global),
            None => String::from("<ERROR>"),
        };

        format!("{exp}imp {path}")
    }

    /// Gets the name of this item. For [`ImpItem`], this is defined as the last component of the path.
    pub fn name(&self, global: &Global) -> Option<Interned<str>> {
        Some(self.path.as_ref()?.last()?.get(&global.interner).0)
    }
}
