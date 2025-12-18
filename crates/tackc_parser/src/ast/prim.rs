use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::Parser;
use crate::ast::{AstNode, Visitor, VisitorMut};
use crate::error::{ParseError, ParseErrors, Result};

use tackc_ast::{NodeId, Primary, PrimaryKind, Symbol};

impl AstNode for Primary {
    fn parse<I, F: File>(p: &mut Parser<I, F>, _: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let tok = p.expect_token(None)?;
        match tok.kind {
            TokenKind::Ident(ident) => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::Binding(Symbol::new(tok.span, ident), None),
                id: p.node_id(),
            }),
            TokenKind::IntLit(str, base) => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::IntLit(Symbol::new(tok.span, str), base),
                id: p.node_id(),
            }),
            TokenKind::FloatLit(str) => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::FloatLit(Symbol::new(tok.span, str)),
                id: p.node_id(),
            }),
            TokenKind::U8 => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::U8,
                id: p.node_id(),
            }),
            TokenKind::U16 => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::U16,
                id: p.node_id(),
            }),
            TokenKind::U32 => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::U32,
                id: p.node_id(),
            }),
            TokenKind::U64 => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::U64,
                id: p.node_id(),
            }),
            TokenKind::I8 => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::I8,
                id: p.node_id(),
            }),
            TokenKind::I16 => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::I16,
                id: p.node_id(),
            }),
            TokenKind::I32 => Ok(Self {
                span: tok.span,
                kind: PrimaryKind::I32,
                id: p.node_id(),
            }),
            TokenKind::I64 => Ok(Self {
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
            PrimaryKind::FloatLit(sym) => sym.display(global).to_string(),
            PrimaryKind::Binding(sym, binding) => {
                format!(
                    "{}{}",
                    sym.display(global),
                    binding.map_or(String::new(), |bind| String::from("[")
                        + &bind.inner().to_string()
                        + "]")
                )
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

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_primary(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_primary_mut(self);
    }
}
