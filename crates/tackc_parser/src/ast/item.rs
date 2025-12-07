use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, Expression, Symbol},
    error::{DiagResult, Result},
};

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Item {
    ConstItem(ConstItem),
}

impl AstNode for Item {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        p.parse::<ConstItem>(recursion + 1).map(Item::ConstItem)
    }

    fn span(&self) -> Span {
        match self {
            Item::ConstItem(item) => item.span(),
        }
    }

    fn display(&self, global: &Global) -> String {
        match self {
            Item::ConstItem(item) => item.display(global),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ConstItem {
    pub ident: Symbol,
    pub ty: Option<Expression>,
    pub expr: Option<Expression>,
    pub span: Span,
}

impl AstNode for ConstItem {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let const_tok = p.expect_token_kind(None, token_kind!(TokenKind::Const))?;
        let ident = p.identifier()?;
        let ty = if p.consume(token_kind!(TokenKind::Colon)).is_some() {
            Some(p.parse::<Expression>(recursion + 1).expected("type")?)
        } else {
            None
        };

        let expr = if p.consume(token_kind!(TokenKind::Eq)).is_some() {
            Some(
                p.parse::<Expression>(recursion + 1)
                    .expected("expression")?,
            )
        } else {
            None
        };

        let semi = p.expect_token_kind(Some("';'"), token_kind!(TokenKind::Semicolon))?;

        Ok(ConstItem {
            ident,
            ty,
            expr,
            span: Span::new_from(const_tok.span.start, semi.span.end),
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        format!(
            "const {}{}{};",
            self.ident.display(global),
            self.ty
                .as_ref()
                .map(|ty| format!(": {}", ty.display(global)))
                .unwrap_or_default(),
            self.expr
                .as_ref()
                .map(|expr| format!(" = {}", expr.display(global)))
                .unwrap_or_default()
        )
    }
}
