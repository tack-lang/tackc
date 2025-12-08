use tackc_global::Global;
use tackc_lexer::{IntegerBase, Token, TokenKind};
use tackc_span::Span;

use crate::Parser;
use crate::ast::{AstNode, Symbol};
use crate::error::{ParseError, ParseErrors, Result};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Primary {
    Binding(Symbol),
    IntLit(Symbol, IntegerBase),
    FloatLit(Symbol),
}

impl AstNode for Primary {
    fn parse<I>(p: &mut Parser<I>, _: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let tok = p.expect_token(None)?;
        match tok.kind {
            TokenKind::Ident(ident) => Ok(Primary::Binding(Symbol::new(tok.span, ident))),
            TokenKind::IntLit(str, base) => Ok(Primary::IntLit(Symbol::new(tok.span, str), base)),
            TokenKind::FloatLit(str) => Ok(Primary::FloatLit(Symbol::new(tok.span, str))),
            _ => Err(ParseErrors::new(ParseError::new(None, tok))),
        }
    }

    fn span(&self) -> Span {
        match self {
            Primary::Binding(sym) | Primary::FloatLit(sym) | Primary::IntLit(sym, _) => sym.span,
        }
    }

    fn display(&self, global: &Global) -> String {
        match self {
            Primary::Binding(sym) | Primary::FloatLit(sym) => sym.display(global).to_string(),
            Primary::IntLit(sym, base) => format!("{base}{}", sym.display(global)),
        }
    }
}

// ExpressionKind::Primary(ident) => ident.display(global).to_string(),
// ExpressionKind::IntLit(str, base) => format!("{base}{}", str.display(global)),
// ExpressionKind::FloatLit(str) => str.display(global).to_string(),
