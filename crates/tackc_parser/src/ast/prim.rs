use tackc_global::Global;
use tackc_lexer::{IntegerBase, Token, TokenKind};
use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::Parser;
use crate::ast::{AstNode, Symbol};
use crate::error::{ParseError, ParseErrors, Result};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct Primary {
    pub span: Span,
    pub kind: PrimaryKind,
}

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
            }),
            TokenKind::IntLit(str, base) => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::IntLit(Symbol::new(tok.span, str), base),
            }),
            TokenKind::FloatLit(str) => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::FloatLit(Symbol::new(tok.span, str)),
            }),
            TokenKind::U8 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::U8,
            }),
            TokenKind::U16 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::U16,
            }),
            TokenKind::U32 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::U32,
            }),
            TokenKind::U64 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::U64,
            }),
            TokenKind::I8 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::I8,
            }),
            TokenKind::I16 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::I16,
            }),
            TokenKind::I32 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::I32,
            }),
            TokenKind::I64 => Ok(Primary {
                span: tok.span,
                kind: PrimaryKind::I64,
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
}
