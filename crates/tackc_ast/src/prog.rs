use serde::{Deserialize, Serialize};
use tackc_global::Global;
use thin_vec::ThinVec;

use crate::{
    NodeId,
    Item, Symbol,
};

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Program {
    pub mod_stmt: Option<ModStatement>,
    pub items: ThinVec<Option<Item>>,
}

impl Program {
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
    pub path: Option<Path>,
    pub id: NodeId,
}

impl ModStatement {
    pub fn display(&self, global: &Global) -> String {
        format!(
            "mod {};",
            self.path
                .as_ref()
                .map_or_else(|| String::from("<ERROR>"), |path| path.display(global))
        )
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Path {
    pub components: ThinVec<Option<Symbol>>,
    pub id: NodeId,
}

impl Path {
    pub fn display(&self, global: &Global) -> String {
        self.components
            .iter()
            .map(|opt| opt.map_or("<ERROR>", |sym| sym.display(global)))
            .collect::<Vec<_>>()
            .join(".")
    }
}
