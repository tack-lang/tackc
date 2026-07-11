//! Modules in tackc.

use std::fmt::Write;

use crate::file::FileId;
use crate::global::{Global, Interned};
use crate::span::Span;
use serde::{Deserialize, Serialize};
use thin_vec::ThinVec;

use crate::frontend::ast::{Item, NodeId, Symbol};

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
        let mod_stmt = match &self.mod_stmt {
            Some(stmt) => stmt.display(global),
            None => String::from("<ERROR>;"),
        };

        let mut stmts = String::new();
        for item in &self.items {
            let displayed = match item {
                Some(item) => item.display(global),
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
        let path = match &self.path {
            Some(path) => path.display(global),
            None => String::from("<ERROR>"),
        };
        format!("{exp}mod {path};")
    }
}

/// A path represented in the AST.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AstPath {
    /// The components of the path.
    pub components: ThinVec<Option<Interned<Symbol>>>,
    /// The ID of this AST node.
    pub id: NodeId,
    /// The span of this AST node.
    pub span: Span,
}

impl AstPath {
    /// Displays this path.
    pub fn display(&self, global: &Global) -> String {
        let mut str = String::new();

        for component in &self.components {
            let comp = match component {
                Some(comp) => comp.get(global).display(global),
                None => "<ERROR>",
            };
            _ = write!(str, "{comp}.");
        }

        str.truncate(str.len().saturating_sub(1));
        str
    }
}
