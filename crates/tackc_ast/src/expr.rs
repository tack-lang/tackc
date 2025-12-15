use std::fmt::Debug;
use std::hash::Hash;

use crate::{Block, NodeId, Primary, Symbol};
use serde::{Deserialize, Serialize};
use tackc_span::Span;

/// The parser's representation of an expression
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Expression {
    #[allow(missing_docs)]
    pub span: Span,
    #[allow(missing_docs)]
    pub kind: Box<ExpressionKind>,
    #[allow(missing_docs)]
    pub id: NodeId,
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ExpressionKind {
    Grouping(Box<Expression>),

    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Neg(Box<Expression>),

    Call(Box<Expression>, Vec<Option<Expression>>),
    Index(Box<Expression>, Box<Expression>),
    Member(Box<Expression>, Symbol),

    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Lt(Box<Expression>, Box<Expression>),
    GtEq(Box<Expression>, Box<Expression>),
    LtEq(Box<Expression>, Box<Expression>),

    Primary(Primary),

    Block(Box<Block>),
}
