//! Statements in tackc.

use crate::global::Global;
use crate::lexer::Token;
use serde::{Deserialize, Serialize};

use crate::ast::{Expression, Item, NodeId, Symbol};

/// A statement.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Statement {
    /// The kind of statement this is.
    pub kind: StatementKind,
    /// The ID of this AST node.
    pub id: NodeId,
}

impl Statement {
    /// Creates a new statement.
    pub const fn new(kind: StatementKind, id: NodeId) -> Self {
        Self { kind, id }
    }

    /// Displays this statement.
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

/// Different kinds of statements.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum StatementKind {
    /// Let statement.
    LetStatement(Box<LetStatement>),
    /// Assignment statement.
    AssignmentStatement(Box<AssignmentStatement>),
    /// Item, in the place of a statement.
    Item(Item),
    /// Expression statement.
    ExpressionStatement(Box<ExpressionStatement>),
}

/// Let statement.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LetStatement {
    /// Type annotation for this let statement.
    pub ty: Option<Option<Expression>>,
    /// The default expression of this let statement.
    pub expr: Option<Option<Expression>>,
    /// The identifier of this let statement.
    pub ident: Option<Symbol>,
}

/// Assignment statement.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AssignmentStatement {
    /// The left hand side.
    pub lhs: Expression,
    /// The right hand side.
    pub rhs: Option<Expression>,
}

/// Expression statement.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExpressionStatement {
    /// The inner expression.
    pub expr: Expression,
    /// The optional semicolon.
    pub semi: Option<Option<Token>>,
}
