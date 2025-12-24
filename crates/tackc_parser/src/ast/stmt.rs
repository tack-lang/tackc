use tackc_global::Global;

use crate::{
    NodeId,
    ast::{Expression, Item, Symbol},
};

pub struct Statement {
    pub kind: StatementKind,
    pub id: NodeId,
}

impl Statement {
    pub const fn new(kind: StatementKind, id: NodeId) -> Self {
        Self { kind, id }
    }

    pub fn display(&self, global: &Global) -> String {
        match &self.kind {
            StatementKind::LetStatement(stmt) => {
                let ident = stmt
                    .ident
                    .map_or_else(|| "<ERROR>", |ident| ident.display(global));
                let ty = match &stmt.ty {
                    Some(Some(ty)) => format!(": {}", ty.display(global)),
                    Some(None) => String::from(": <ERROR>"),
                    None => String::new(),
                };
                let expr = match &stmt.expr {
                    Some(Some(expr)) => format!(" = {}", expr.display(global)),
                    Some(None) => String::from(" <ERROR>"),
                    None => String::new(),
                };
                format!("let {ident}{ty}{expr};")
            },
            StatementKind::Item(item) => item.display(global),
        }
    }
}

pub enum StatementKind {
    LetStatement(Box<LetStatement>),
    Item(Item),
}

pub struct LetStatement {
    pub ty: Option<Option<Expression>>,
    pub expr: Option<Option<Expression>>,
    pub ident: Option<Symbol>,
}
