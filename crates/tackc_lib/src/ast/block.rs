//! Blocks in tackc.

use crate::global::Global;
use serde::{Deserialize, Serialize};

use crate::ast::{Expression, NodeId, Statement};

/// Code blocks.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Block {
    /// The statements in this block.
    pub stmts: Vec<Option<Statement>>,
    /// The optional tail expression of this block.
    pub expr: Option<Option<Expression>>,
    /// The ID of this node.
    pub id: NodeId,
}

impl Block {
    /// Displays this block.
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
            Some(Some(val)) => val.display(global).replace('\n', "\n    "),
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
