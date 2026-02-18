//! Statements in tackc.

use crate::global::Global;
use crate::lexer::Token;
use serde::Serialize;

use crate::ast::{Expression, Item, NodeId, Symbol};

/// A statement.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Statement<'src> {
    /// The kind of statement this is.
    pub kind: &'src StatementKind<'src>,
    /// The ID of this AST node.
    pub id: NodeId,
}

impl<'src> Statement<'src> {
    /// Creates a new statement.
    pub const fn new(kind: &'src StatementKind<'src>, id: NodeId) -> Self {
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
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum StatementKind<'src> {
    /// Let statement.
    LetStatement(&'src LetStatement<'src>),
    /// Assignment statement.
    AssignmentStatement(&'src AssignmentStatement<'src>),
    /// Item, in the place of a statement.
    Item(&'src Item<'src>),
    /// Expression statement.
    ExpressionStatement(&'src ExpressionStatement<'src>),
}

/// Let statement.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct LetStatement<'src> {
    /// Type annotation for this let statement.
    pub ty: Option<Option<&'src Expression<'src>>>,
    /// The default expression of this let statement.
    pub expr: Option<Option<&'src Expression<'src>>>,
    /// The identifier of this let statement.
    pub ident: Option<Symbol>,
}

/// Assignment statement.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct AssignmentStatement<'src> {
    /// The left hand side.
    pub lhs: &'src Expression<'src>,
    /// The right hand side.
    pub rhs: Option<&'src Expression<'src>>,
}

/// Expression statement.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ExpressionStatement<'src> {
    /// The inner expression.
    pub expr: &'src Expression<'src>,
    /// The optional semicolon.
    pub semi: Option<Option<Token>>,
}
