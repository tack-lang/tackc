use tackc_global::Interned;

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

#[derive(Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}
