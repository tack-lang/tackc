use tackc_global::{Global, Interned};
use tackc_lexer::TokenKind;
use tackc_span::Span;

use crate::{
    ast::{AstNode, Expr},
    error::{DiagResult, Result},
};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Declaration {
    Constant(Constant),
}

impl AstNode for Declaration {
    fn parse<I>(p: &mut crate::Parser<I>, recursion: u32) -> crate::error::Result<Self>
    where
        I: Iterator<Item = tackc_lexer::Token> + Clone,
    {
        p.parse::<Constant>(recursion).map(Declaration::Constant)
    }

    fn span(&self) -> Span {
        match self {
            Declaration::Constant(constant) => constant.span(),
        }
    }

    fn display(&self, global: &Global) -> String {
        match self {
            Declaration::Constant(constant) => constant.display(global),
        }
    }
}


#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Constant {
    pub identifier: Interned<str>,
    pub ty: Option<Expr>,
    pub expr: Expr,
    pub span: Span,
}

impl AstNode for Constant {
    fn parse<I>(p: &mut crate::Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = tackc_lexer::Token> + Clone,
    {
        let const_key = p.expect_token_kind(None, token_kind!(TokenKind::Const))?;
        let (identifier, _) = p.identifier()?;
        let ty = if p.consume(token_kind!(TokenKind::Colon)) {
            Some(p.parse::<Expr>(recursion + 1).expected("type")?)
        } else {
            None
        };
        let _eq = p.expect_token_kind(Some("'='"), token_kind!(TokenKind::Eq))?;
        let expr = p.parse::<Expr>(recursion + 1).expected("expression")?;
        let semi = p.expect_token_kind(Some("';'"), token_kind!(TokenKind::Semicolon))?;

        Ok(Constant {
            identifier,
            ty,
            expr,
            span: Span::new_from(const_key.span.start, semi.span.end),
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        let mut str = String::from("const ");
        str.push_str(self.identifier.display(global));
        if let Some(ty) = &self.ty {
            str.push_str(": ");
            str.push_str(&ty.display(global));
        }
        str.push_str(" = ");
        str.push_str(&self.expr.display(global));
        str.push(';');

        str
    }
}
