use std::collections::HashMap;

use crate::global::Global;
use crate::span::Span;
use serde::{Deserialize, Serialize};
use thin_vec::ThinVec;

use crate::ast::{Item, NodeId, Symbol};

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AstModule {
    pub mod_stmt: Option<ModStatement>,
    pub items: ThinVec<Option<Item>>,
    pub spans: HashMap<NodeId, Span>,
}

impl AstModule {
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

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ModStatement {
    pub exported: bool,
    pub path: Option<AstPath>,
    pub id: NodeId,
}

impl ModStatement {
    pub fn display(&self, global: &Global) -> String {
        let exported = if self.exported { "exp " } else { "" };
        let path = self
            .path
            .as_ref()
            .map_or_else(|| String::from("<ERROR>"), |path| path.display(global));
        format!("{exported}mod {path};")
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AstPath {
    pub components: ThinVec<Option<Symbol>>,
    pub id: NodeId,
}

impl AstPath {
    pub fn display(&self, global: &Global) -> String {
        self.components
            .iter()
            .map(|opt| opt.map_or("<ERROR>", |sym| sym.display(global)))
            .collect::<Vec<_>>()
            .join(".")
    }
}
