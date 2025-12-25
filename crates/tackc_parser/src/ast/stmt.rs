use tackc_global::Global;
use tackc_lexer::Token;

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
            }
            StatementKind::AssignmentStatement(stmt) => format!(
                "{} = {};",
                stmt.lhs.display(global),
                stmt.rhs
                    .as_ref()
                    .map_or_else(|| String::from("<ERROR>"), |expr| expr.display(global))
            ),
            StatementKind::Item(item) => item.display(global),
            StatementKind::ExpressionStatement(stmt) => format!(
                "{}{}",
                stmt.expr.display(global),
                stmt.semi.map_or("", |_| ";")
            ),
        }
    }
}

pub enum StatementKind {
    LetStatement(Box<LetStatement>),
    AssignmentStatement(Box<AssignmentStatement>),
    Item(Item),
    ExpressionStatement(Box<ExpressionStatement>),
}

pub struct LetStatement {
    pub ty: Option<Option<Expression>>,
    pub expr: Option<Option<Expression>>,
    pub ident: Option<Symbol>,
}

pub struct AssignmentStatement {
    pub lhs: Expression,
    pub rhs: Option<Expression>,
}

pub struct ExpressionStatement {
    pub expr: Expression,
    pub semi: Option<Token>,
}
