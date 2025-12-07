use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, BindingPower, Block, Expression, ParseMode, Symbol, parse_expression},
    error::{DiagResult, ParseError, ParseErrors, Result},
};

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Item {
    ConstItem(ConstItem),
    FuncItem(FuncItem),
    // When adding to `Item`, update `prog::sync_prog`.
}

impl AstNode for Item {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        p.check_recursion(recursion + 1)?;
        let tok = p.expect_peek_token(None)?;
        match tok.kind {
            TokenKind::Const => p.parse::<ConstItem>(recursion + 1).map(Item::ConstItem),
            TokenKind::Func => p.parse::<FuncItem>(recursion + 1).map(Item::FuncItem),
            _ => Err(ParseErrors::new(ParseError::new(None, tok))),
        }
    }

    fn span(&self) -> Span {
        match self {
            Item::ConstItem(item) => item.span,
            Item::FuncItem(item) => item.span,
        }
    }

    fn display(&self, global: &Global) -> String {
        match self {
            Item::ConstItem(item) => item.display(global),
            Item::FuncItem(item) => item.display(global),
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

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FuncItem {
    pub ident: Symbol,
    pub params: Vec<(Symbol, Expression)>,
    pub ret_ty: Option<Expression>,
    pub block: Block,
    pub span: Span,
}

impl AstNode for FuncItem {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let func = p.expect_token_kind(None, token_kind!(TokenKind::Func))?;
        let ident = p.identifier()?;
        p.expect_token_kind(Some("'('"), token_kind!(TokenKind::LParen))?;

        // Parameter list
        let mut params = Vec::new();
        while let Some(tok) = p.peek_token()
            && tok.kind != TokenKind::RParen
        {
            let ident = p.identifier()?;
            p.expect_token_kind(Some("':'"), token_kind!(TokenKind::Colon))?;
            let ty = parse_expression(p, BindingPower::None, recursion + 1, ParseMode::NoBlocks)
                .expected("type")?;
            params.push((ident, ty));
            if p.consume(token_kind!(TokenKind::Comma)).is_none() {
                break;
            }
        }

        p.expect_token_kind(Some("')'"), token_kind!(TokenKind::RParen))?;

        let ret_ty = if p.peek_is(token_kind!(TokenKind::LBrace)) {
            None
        } else {
            Some(
                parse_expression(p, BindingPower::None, recursion + 1, ParseMode::NoBlocks)
                    .expected("type")?,
            )
        };
        let block = p.parse::<Block>(recursion + 1).expected("block")?;

        Ok(FuncItem {
            span: Span::new_from(func.span.start, block.span.end),
            ident,
            params,
            ret_ty,
            block,
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        let mut parts = Vec::with_capacity(self.params.len());
        for (ident, ty) in &self.params {
            parts.push(format!("{}: {}", ident.display(global), ty.display(global)));
        }
        let ret = self
            .ret_ty
            .as_ref()
            .map(|ty| ty.display(global) + " ")
            .unwrap_or_default();
        if parts.is_empty() {
            format!(
                "func {}() {ret}{}",
                self.ident.display(global),
                self.block.display(global)
            )
        } else {
            format!(
                "func {}({}) {ret}{}",
                self.ident.display(global),
                parts.join(", "),
                self.block.display(global)
            )
        }
    }
}
