use serde::{Deserialize, Serialize};
use tackc_global::Global;
use tackc_lexer::Token;

use crate::{
    NodeId,
    Expression, Item, Symbol,
};

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
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
                    Some(None) => String::from(" = <ERROR>"),
                    None => String::new(),
                };
                format!("let {ident}{ty}{expr};")
            }
            StatementKind::AssignmentStatement(stmt) => {
                let lhs = stmt.lhs.display(global);
                let rhs = stmt
                    .rhs
                    .as_ref()
                    .map_or_else(|| String::from("<ERROR>"), |expr| expr.display(global));
                format!("{lhs} = {rhs};")
            }
            StatementKind::Item(item) => item.display(global),
            StatementKind::ExpressionStatement(stmt) => {
                let stmt_str = stmt.expr.display(global);
                let semi = stmt.semi.map_or("", |_| ";");
                format!("{stmt_str}{semi}")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum StatementKind {
    LetStatement(Box<LetStatement>),
    AssignmentStatement(Box<AssignmentStatement>),
    Item(Item),
    ExpressionStatement(Box<ExpressionStatement>),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LetStatement {
    pub ty: Option<Option<Expression>>,
    pub expr: Option<Option<Expression>>,
    pub ident: Option<Symbol>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AssignmentStatement {
    pub lhs: Expression,
    pub rhs: Option<Expression>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExpressionStatement {
    pub expr: Expression,
    pub semi: Option<Option<Token>>,
}
