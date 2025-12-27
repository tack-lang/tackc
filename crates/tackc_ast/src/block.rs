use serde::{Deserialize, Serialize};
use tackc_global::Global;

use crate::{Expression, NodeId, Statement};

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Option<Statement>>,
    pub expr: Option<Option<Expression>>,
    pub id: NodeId,
}

impl Block {
    pub fn display(&self, global: &Global) -> String {
        let stmts = self
            .stmts
            .iter()
            .map(|stmt| {
                stmt.as_ref().map_or_else(
                    || String::from("<ERROR>;"),
                    |s| s.display(global).replace('\n', "\n    "),
                )
            })
            .collect::<Vec<_>>()
            .join("\n    ");

        let expr = match &self.expr {
            Some(Some(val)) => val.display(global),
            Some(None) => String::from("<ERROR>"),
            None => String::new(),
        };

        match (stmts.is_empty(), expr.is_empty()) {
            (true, true) => String::from("{}"),
            (false, true) => format!("{{\n    {stmts}\n}}"),
            (true, false) => format!("{{\n    {expr}\n}}"),
            (false, false) => format!("{{\n    {stmts}\n    {expr}\n}}"),
        }
    }
}
