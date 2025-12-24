use std::fmt::Display;

use tackc_global::{Global, Interned};
use tackc_lexer::Token;
use tackc_span::Span;
use thin_vec::ThinVec;

use crate::NodeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub Interned<str>, pub Span);

impl Symbol {
    pub fn display<'a>(&self, global: &'a Global) -> &'a str {
        self.0.display(global)
    }
}

impl From<Token> for Symbol {
    fn from(value: Token) -> Self {
        Self(value.lexeme, value.span)
    }
}

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
            ExpressionKind::Member(lhs, sym) => format!(
                "(. {} {})",
                lhs.display(global),
                sym.as_ref().map_or("<ERROR>", |sym| sym.display(global))
            ),
            ExpressionKind::Call(lhs, args) => {
                let arg_list = args
                    .iter()
                    .map(|expr| {
                        expr.as_ref()
                            .map_or_else(|| "<ERROR>".to_string(), |e| e.display(global))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                let formatted_args = if arg_list.is_empty() {
                    String::default()
                } else {
                    format!(" {arg_list}")
                };

                format!("(call {}{})", lhs.display(global), formatted_args)
            }
            ExpressionKind::Index(lhs, index) => format!(
                "(index {} {})",
                lhs.display(global),
                index
                    .as_ref()
                    .map_or_else(|| String::from("<ERROR>"), |expr| expr.display(global))
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
    Member(Box<Expression>, Option<Box<Symbol>>),
    Call(Box<Expression>, ThinVec<Option<Expression>>),
    Index(Box<Expression>, Option<Box<Expression>>),

    Binary(BinOp, Box<Expression>, Box<Expression>),
    Unary(UnOp, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,

    Gt,
    Lt,
    GtEq,
    LtEq,
    Eq,
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
