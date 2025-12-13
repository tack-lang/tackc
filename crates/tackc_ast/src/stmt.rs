use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{
    Expression, Item, NodeId, Symbol,
};

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum StatementOrExpression {
    Expression(Expression),
    Statement(Statement),
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    LetStatement(LetStatement),
    AssignmentStatement(AssignmentStatement),
    Item(Item),
}

/// An expression ending with a semicolon.
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExpressionStatement {
    #[allow(missing_docs)]
    pub span: Span,
    /// The inner expression for this expression statement
    pub inner: Expression,
    #[allow(missing_docs)]
    pub id: NodeId,
}

/// A let statement, e.g. `let x = 5;`
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LetStatement {
    #[allow(missing_docs)]
    pub span: Span,
    /// The symbol being defined
    pub ident: Symbol,
    /// The type annotation given
    pub ty: Option<Expression>,
    /// The initial value given
    pub expr: Option<Expression>,
    #[allow(missing_docs)]
    pub id: NodeId,
}

/// An assignment statement
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AssignmentStatement {
    #[allow(missing_docs)]
    pub span: Span,
    /// The value on the left of the `=`
    pub lvalue: Expression,
    /// The value on the right of the `=`
    pub rvalue: Expression,
    #[allow(missing_docs)]
    pub id: NodeId,
}
