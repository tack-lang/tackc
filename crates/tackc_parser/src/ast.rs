use std::fmt::Display;

use tackc_global::{Global, Interned};

use crate::NodeId;

#[derive(Debug, PartialEq, Eq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub id: NodeId,
}

impl Expression {
    pub const fn new(kind: ExpressionKind, id: NodeId) -> Self {
        Self { kind, id }
    }

    pub fn display(&self, global: &Global) -> String {
        match &self.kind {
            ExpressionKind::IntLit(sym)
            | ExpressionKind::FloatLit(sym)
            | ExpressionKind::Ident(sym) => sym.display(global).to_string(),
            ExpressionKind::Grouping(inner) => inner.as_ref().map_or_else(
                || String::from("(<ERROR>)"),
                |expr| format!("({})", expr.display(global)),
            ),

            ExpressionKind::Binary(op, lhs, rhs) => {
                format!("({op} {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::Unary(op, rhs) => format!("({op} {})", rhs.display(global)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    IntLit(Interned<str>),
    FloatLit(Interned<str>),
    Ident(Interned<str>),
    Grouping(Option<Box<Expression>>),

    Binary(BinOp, Box<Expression>, Box<Expression>),
    Unary(UnOp, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnOp {
    Neg,
    Not,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}
