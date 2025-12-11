use tackc_global::Global;
use tackc_lexer::{IntegerBase, Token, TokenKind};
use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::Parser;
use crate::ast::{AstNode, NodeId, Symbol, Visitor};
use crate::error::{ParseError, ParseErrors, Result};

/// A primary expression
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct Primary {
    #[allow(missing_docs)]
    pub span: Span,
    #[allow(missing_docs)]
    pub kind: PrimaryKind,
    #[allow(missing_docs)]
    pub id: NodeId,
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub enum PrimaryKind {
    Binding(Symbol),
    IntLit(Symbol, IntegerBase),
    FloatLit(Symbol),

    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}

impl AstNode for Primary {
    fn parse<I>(p: &mut Parser<I>, _: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let tok = p.expect_token(None)?;
        match tok.kind {
            TokenKind::Ident(ident) => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::Binding(Symbol::new(tok.span, ident)),
                id: p.node_id(),
            }),
            TokenKind::IntLit(str, base) => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::IntLit(Symbol::new(tok.span, str), base),
                id: p.node_id(),
            }),
            TokenKind::FloatLit(str) => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::FloatLit(Symbol::new(tok.span, str)),
                id: p.node_id(),
            }),
            TokenKind::U8 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::U8,
                id: p.node_id(),
            }),
            TokenKind::U16 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::U16,
                id: p.node_id(),
            }),
            TokenKind::U32 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::U32,
                id: p.node_id(),
            }),
            TokenKind::U64 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::U64,
                id: p.node_id(),
            }),
            TokenKind::I8 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::I8,
                id: p.node_id(),
            }),
            TokenKind::I16 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::I16,
                id: p.node_id(),
            }),
            TokenKind::I32 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::I32,
                id: p.node_id(),
            }),
            TokenKind::I64 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::I64,
                id: p.node_id(),
            }),
            _ => Err(ParseErrors::new(ParseError::new(None, tok))),
        }
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        match self.kind {
            PrimaryKind::Binding(sym) | PrimaryKind::FloatLit(sym) => {
                sym.display(global).to_string()
            }
            PrimaryKind::IntLit(sym, base) => format!("{base}{}", sym.display(global)),
            PrimaryKind::U8 => "u8".to_string(),
            PrimaryKind::U16 => "u16".to_string(),
            PrimaryKind::U32 => "u32".to_string(),
            PrimaryKind::U64 => "u64".to_string(),
            PrimaryKind::I8 => "i8".to_string(),
            PrimaryKind::I16 => "i16".to_string(),
            PrimaryKind::I32 => "i32".to_string(),
            PrimaryKind::I64 => "i64".to_string(),
        }
    }

    fn id(&self) -> super::NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_primary(self);
    }
}
