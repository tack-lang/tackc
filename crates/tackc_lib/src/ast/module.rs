//! Modules in tackc.

use crate::global::Global;
use crate::span::Span;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use thin_vec::ThinVec;

use crate::ast::{Item, NodeId, Symbol};

/// A module represented in the AST.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct AstModule<'src> {
    /// The module statement of this module.
    pub mod_stmt: Option<&'src ModStatement>,
    /// The items of this module.
    pub items: ThinVec<Option<&'src Item<'src>>>,
    /// The spans of this module.
    pub spans: Box<FxHashMap<NodeId, Span>>,
}

impl AstModule<'_> {
    /// Displays this module.
    pub fn display(&self, global: &Global) -> String {
        let mod_stmt = self.mod_stmt.as_ref().map_or_else(
            || String::from("<ERROR>;"),
            |mod_stmt| mod_stmt.display(global),
        );
        let stmts = self
            .items
            .iter()
            .map(|opt| {
                opt.as_ref()
                    .map_or_else(|| String::from("<ERROR>"), |sym| sym.display(global))
            })
            .collect::<Vec<_>>()
            .join("\n");
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
}

impl ModStatement {
    /// Displays this mod statement.
    pub fn display(&self, global: &Global) -> String {
        let exported = if self.exported { "exp " } else { "" };
        let path = self
            .path
            .as_ref()
            .map_or_else(|| String::from("<ERROR>"), |path| path.display(global));
        format!("{exported}mod {path};")
    }
}

/// A path represented in the AST.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AstPath {
    /// The components of the path.
    pub components: ThinVec<Option<Symbol>>,
    /// The ID of this AST node.
    pub id: NodeId,
}

impl AstPath {
    /// Displays this path.
    pub fn display(&self, global: &Global) -> String {
        self.components
            .iter()
            .map(|opt| opt.map_or("<ERROR>", |sym| sym.display(global)))
            .collect::<Vec<_>>()
            .join(".")
    }
}
