//! Statements in tackc.

use crate::frontend::lexer::Token;
use crate::global::{Global, Interned};
use crate::span::Span;
use serde::Serialize;

use crate::frontend::ast::{Expression, Item, NodeId, Symbol};

/// A statement.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Statement {
    /// The kind of statement this is.
    pub kind: StatementKind,
    /// The ID of this AST node.
    pub id: NodeId,
    /// The span of this AST node.
    pub span: Span,
}

impl Statement {
    /// Creates a new statement.
    pub const fn new(kind: StatementKind, id: NodeId, span: Span) -> Self {
        Self { kind, id, span }
    }

    /// Displays this statement.
    pub fn display(&self, global: &Global) -> String {
        match &self.kind {
            StatementKind::LetStatement(stmt) => stmt.display(global),
            StatementKind::AssignmentStatement(stmt) => stmt.display(global),
            StatementKind::Item(item) => item.display(global),
            StatementKind::ExpressionStatement(stmt) => stmt.display(global),
        }
    }
}

/// Different kinds of statements.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum StatementKind {
    /// Let statement.
    LetStatement(LetStatement),
    /// Assignment statement.
    AssignmentStatement(AssignmentStatement),
    /// Item, in the place of a statement.
    Item(Item),
    /// Expression statement.
    ExpressionStatement(ExpressionStatement),
}

/// Let statement.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct LetStatement {
    /// Type annotation for this let statement.
    pub ty: Option<Option<Expression>>,
    /// The default expression of this let statement.
    pub expr: Option<Option<Expression>>,
    /// The identifier of this let statement.
    pub ident: Option<Interned<Symbol>>,
}

impl LetStatement {
    fn display(&self, global: &Global) -> String {
        let ident = match self.ident {
            Some(ident) => ident.get(global).display(global),
            None => "<ERROR>",
        };
        let ty = match &self.ty {
            Some(Some(ty)) => format!(": {}", ty.display(global)),
            Some(None) => String::from(": <ERROR>"),
            None => String::new(),
        };
        let expr = match &self.expr {
            Some(Some(expr)) => format!(" = {}", expr.display(global)),
            Some(None) => String::from(" = <ERROR>"),
            None => String::new(),
        };
        format!("let {ident}{ty}{expr};")
    }
}

/// Assignment statement.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct AssignmentStatement {
    /// The left hand side.
    pub lhs: Expression,
    /// The right hand side.
    pub rhs: Option<Expression>,
}

impl AssignmentStatement {
    fn display(&self, global: &Global) -> String {
        let lhs = self.lhs.display(global);
        let rhs = match &self.rhs {
            Some(expr) => expr.display(global),
            None => String::from("<ERROR>"),
        };
        format!("{lhs} = {rhs};")
    }
}

/// Expression statement.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ExpressionStatement {
    /// The inner expression.
    pub expr: Expression,
    /// The optional semicolon.
    pub semi: Option<Option<Token>>,
}

impl ExpressionStatement {
    fn display(&self, global: &Global) -> String {
        let stmt_str = self.expr.display(global);
        let semi = match self.semi {
            Some(_) => ";",
            None => "",
        };
        format!("{stmt_str}{semi}")
    }
}
