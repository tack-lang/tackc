use std::fmt::Write;

use tackc_global::Global;

use crate::{
    NodeId,
    ast::{Expression, Statement},
};

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Option<Statement>>,
    pub expr: Option<Option<Expression>>,
    pub id: NodeId,
}

impl Block {
    pub fn display(&self, global: &Global) -> String {
        let mut f = String::from("{");
        for i in &self.stmts {
            _ = write!(
                f,
                " {}",
                i.as_ref()
                    .map_or_else(|| String::from("<ERROR>;"), |stmt| stmt.display(global))
            );
        }
        match &self.expr {
            Some(Some(value)) => _ = write!(f, " {}", value.display(global)),
            Some(None) => _ = write!(f, " <ERROR>"),
            None => {}
        }
        _ = write!(f, " }}");
        f
    }
}
