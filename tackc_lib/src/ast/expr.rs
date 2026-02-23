//! Expressions in tackc.

use std::fmt::{Display, Write};

use crate::global::{Global, Interned};
use serde::{Deserialize, Serialize};
use thin_vec::ThinVec;

use super::{Block, NodeId, Symbol};

/// An expression in parsed form.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Expression<'src> {
    /// The kind of expression this expression is.
    pub kind: &'src ExpressionKind<'src>,
    /// The ID of this node.
    pub id: NodeId,
}

impl<'src> Expression<'src> {
    /// Creates a new expression.
    pub const fn new(kind: &'src ExpressionKind<'src>, id: NodeId) -> Self {
        Self { kind, id }
    }

    /// Displays an expression.
    pub fn display(&self, global: &Global) -> String {
        match &self.kind {
            ExpressionKind::IntLit(sym) | ExpressionKind::FloatLit(sym) => {
                sym.display(global).to_string()
            }
            ExpressionKind::Ident(sym) => sym.get(global).display(global).to_string(),
            ExpressionKind::GlobalIdent(sym) => {
                let sym = match sym {
                    Some(sym) => sym.get(global).display(global),
                    None => "<ERROR>",
                };
                format!(".{sym}")
            }
            ExpressionKind::StringLit(sym) => format!("\"{}\"", sym.display(global)),
            ExpressionKind::Grouping(inner) => match inner {
                Some(expr) => format!("({})", expr.display(global)),
                None => String::from("(<ERROR>)"),
            },
            ExpressionKind::Member(lhs, sym) => {
                let sym = match sym {
                    Some(sym) => sym.get(global).display(global),
                    None => "<ERROR>",
                };
                format!("(. {} {sym})", lhs.display(global))
            }
            ExpressionKind::Call(lhs, args) => {
                let mut arg_list = String::new();
                for expr in args {
                    let expr = match expr {
                        Some(e) => e.display(global),
                        None => String::from("<ERROR>"),
                    };
                    _ = write!(arg_list, "{expr}, ");
                }
                arg_list.truncate(arg_list.len().saturating_sub(2));

                let formatted_args = if arg_list.is_empty() {
                    String::new()
                } else {
                    format!(" {arg_list}")
                };

                format!("(call {}{})", lhs.display(global), formatted_args)
            }
            ExpressionKind::Index(lhs, index) => {
                let index = match index {
                    Some(expr) => expr.display(global),
                    None => String::from("<ERROR>"),
                };
                format!("(index {} {index})", lhs.display(global))
            }
            ExpressionKind::Block(block) => block.display(global),

            ExpressionKind::Binary(op, lhs, rhs) => {
                format!("({op} {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::Unary(op, rhs) => format!("({op} {})", rhs.display(global)),
        }
    }
}

/// Different kinds of expressions.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum ExpressionKind<'src> {
    /// Integer literal.
    IntLit(Interned<str>),
    /// Float literal.
    FloatLit(Interned<str>),
    /// String literal.
    StringLit(Interned<str>),
    /// Identifier.
    Ident(Interned<Symbol>),
    /// Global identifier (starting with `.`, and referencing global scope.)
    GlobalIdent(Option<Interned<Symbol>>),
    /// Grouped expression.
    Grouping(Option<&'src Expression<'src>>),
    /// Member access of an expression.
    Member(&'src Expression<'src>, Option<Interned<Symbol>>),
    /// Call expression.
    Call(
        &'src Expression<'src>,
        ThinVec<Option<&'src Expression<'src>>>,
    ),
    /// Indexing of an expression.
    Index(&'src Expression<'src>, Option<&'src Expression<'src>>),
    /// Block expression.
    Block(&'src Block<'src>),

    /// Binary expression.
    Binary(BinOp, &'src Expression<'src>, &'src Expression<'src>),
    /// Unary expression.
    Unary(UnOp, &'src Expression<'src>),
}

impl ExpressionKind<'_> {
    /// Returns whether this expression kind ends with a block.
    pub const fn is_block(&self) -> bool {
        matches!(self, Self::Block(_))
    }
}

/// Binary operations.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
#[repr(u8)]
// Docs shouldn't have punctuation for non-sentences.
#[expect(clippy::doc_paragraphs_missing_punctuation)] // CHECKED(Chloe)
pub enum BinOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,

    /// `>`
    Gt,
    /// `<`
    Lt,
    /// `>=`
    GtEq,
    /// `<=`
    LtEq,
    /// `==`
    Eq,
    /// `!=`
    NotEq,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::GtEq => write!(f, ">="),
            Self::LtEq => write!(f, "<="),
            Self::Eq => write!(f, "=="),
            Self::NotEq => write!(f, "!="),
        }
    }
}

/// Unary operator.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
#[repr(u8)]
// Docs shouldn't have punctuation for non-sentences.
#[expect(clippy::doc_paragraphs_missing_punctuation)] // CHECKED(Chloe)
pub enum UnOp {
    /// `-`
    Neg,
    /// `!`
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
