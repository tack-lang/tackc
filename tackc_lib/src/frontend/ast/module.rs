//! Modules in tackc.

use std::{fmt::Write, iter::Copied, slice::Iter};

use serde::{Deserialize, Serialize};
use thin_vec::ThinVec;

use crate::{
    file::FileId,
    frontend::ast::{Item, NodeId, Symbol},
    global::Global,
    span::Span,
    utils::{UnwrapExt, intern::Interned},
};

/// A module represented in the AST.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct AstModule {
    /// The file of this module.
    pub file: FileId,
    /// The module statement of this module.
    pub mod_stmt: Option<ModStatement>,
    /// The items of this module.
    pub items: ThinVec<Option<Item>>,
}

impl AstModule {
    /// Displays this module.
    pub fn display(&self, global: &Global) -> String {
        let mod_stmt = match self.mod_stmt {
            Some(ref stmt) => stmt.display(global),
            None => String::from("<ERROR>;"),
        };

        let mut stmts = String::new();
        for item in &self.items {
            let displayed = match *item {
                Some(ref item) => item.display(global),
                None => String::from("<ERROR>"),
            };
            _ = writeln!(stmts, "{displayed}");
        }
        stmts.truncate(stmts.len().saturating_sub(1));

        format!("{mod_stmt}\n{stmts}")
    }
}

/// A module statement, belonging at the start of a module.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ModStatement {
    /// Whether this module is expected.
    pub exported: bool,
    /// The path of this module.
    pub path: Option<AstPath>,
    /// The ID of this module statement.
    pub id: NodeId,
    /// The span of this module statement.
    pub span: Span,
}

impl ModStatement {
    /// Displays this mod statement.
    pub fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let path = match self.path {
            Some(ref path) => path.display(global),
            None => String::from("<ERROR>"),
        };
        format!("{exp}mod {path};")
    }
}

/// A path represented in the AST.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AstPath {
    /// The components of the path.
    components: ThinVec<Option<Interned<Symbol>>>,
    /// The ID of this AST node.
    pub id: NodeId,
    /// The span of this AST node.
    pub span: Span,
}

impl AstPath {
    /// Creates a new [`AstPath`].
    ///
    /// # Panics
    /// This function panics if the components is empty.
    pub fn new(components: ThinVec<Option<Interned<Symbol>>>, id: NodeId, span: Span) -> Self {
        assert!(
            !components.is_empty(),
            "AstPath.components cannot be empty!"
        );

        Self {
            components,
            id,
            span,
        }
    }

    /// Gets this path's `components` value.
    pub fn components(&self) -> &[Option<Interned<Symbol>>] {
        &self.components
    }

    /// Gets the first component of this path.
    pub fn first(&self) -> Option<Interned<Symbol>> {
        debug_assert!(
            !self.components.is_empty(),
            "AstPath.components shouldn't be empty!"
        );

        self.components
            .first()
            // This is an invariant.
            .expect_unreachable() // CHECKED(Chloe)
            .as_ref()
            .copied()
    }

    /// Gets the last component of this path.
    pub fn last(&self) -> Option<Interned<Symbol>> {
        debug_assert!(
            !self.components.is_empty(),
            "AstPath.components shouldn't be empty!"
        );

        self.components
            .last()
            // This is an invariant.
            .expect_unreachable() // CHECKED(Chloe)
            .as_ref()
            .copied()
    }

    /// Displays this path.
    pub fn display(&self, global: &Global) -> String {
        let mut str = String::new();

        for component in &self.components {
            let comp = match *component {
                Some(comp) => comp.get(&global.interner).display(global),
                None => "<ERROR>",
            };
            _ = write!(str, "{comp}.");
        }

        str.truncate(str.len().saturating_sub(1));
        str
    }

    /// Creates an iterator using the components of this path.
    pub fn iter(&self) -> Copied<Iter<'_, Option<Interned<Symbol>>>> {
        self.components.iter().copied()
    }
}

impl IntoIterator for AstPath {
    type IntoIter = thin_vec::IntoIter<Option<Interned<Symbol>>>;
    type Item = Option<Interned<Symbol>>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

impl<'a> IntoIterator for &'a AstPath {
    type IntoIter = Copied<Iter<'a, Option<Interned<Symbol>>>>;
    type Item = Option<Interned<Symbol>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
