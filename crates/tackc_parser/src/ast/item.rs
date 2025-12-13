use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{
    Parser,
    ast::{
        AstNode, BindingPower, Block, Expression, NodeId, ParseMode, Symbol, Visitor, VisitorMut, parse_expression
    },
    error::{DiagResult, ParseError, ParseErrors, Result},
};

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Item {
    ConstItem(ConstItem),
    FuncItem(FuncItem),
    // When adding to `Item`, update `prog::sync_item`.
}

impl AstNode for Item {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
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

    fn id(&self) -> super::NodeId {
        match self {
            Item::ConstItem(item) => item.id,
            Item::FuncItem(item) => item.id,
        }
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        match self {
            Item::ConstItem(item) => item.accept(v),
            Item::FuncItem(item) => item.accept(v),
        }
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        match self {
            Item::ConstItem(item) => item.accept_mut(v),
            Item::FuncItem(item) => item.accept_mut(v),
        }
    }
}

/// A constant declaration
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ConstItem {
    #[allow(missing_docs)]
    pub span: Span,
    /// The identifier being bound to
    pub ident: Symbol,
    /// The type annotation given
    pub ty: Option<Expression>,
    /// The value given
    pub expr: Expression,
    #[allow(missing_docs)]
    pub id: NodeId,
}

impl AstNode for ConstItem {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let const_tok = p.expect_token_kind(None, kind!(TokenKind::Const))?;
        let ident = p.identifier()?;
        let ty = if p.consume(kind!(TokenKind::Colon)).is_some() {
            Some(p.parse::<Expression>(recursion + 1).expected("type")?)
        } else {
            None
        };

        let _eq = p.expect_token(Some("'='"))?;

        let expr = p
            .parse::<Expression>(recursion + 1)
            .expected("expression")?;

        let semi = p.expect_token_kind(Some("';'"), kind!(TokenKind::Semicolon))?;

        Ok(ConstItem {
            span: Span::new_from(const_tok.span.start, semi.span.end),
            ident,
            ty,
            expr,
            id: p.node_id(),
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        format!(
            "const {}{} = {};",
            self.ident.display(global),
            self.ty
                .as_ref()
                .map(|ty| format!(": {}", ty.display(global)))
                .unwrap_or_default(),
            self.expr.display(global),
        )
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_const_item(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_const_item_mut(self);
    }
}

/// A function declaration
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FuncItem {
    #[allow(missing_docs)]
    pub span: Span,
    /// The name of this function
    pub ident: Symbol,
    /// The parameters of this function
    pub params: Vec<(Symbol, Expression)>,
    /// The return type of this function
    pub ret_ty: Option<Expression>,
    /// The code of this function
    pub block: Block,
    #[allow(missing_docs)]
    pub id: NodeId,
}

impl AstNode for FuncItem {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let func = p.expect_token_kind(None, kind!(TokenKind::Func))?;
        let ident = p.identifier()?;
        p.expect_token_kind(Some("'('"), kind!(TokenKind::LParen))?;

        // Parameter list
        let mut params = Vec::new();
        while let Some(tok) = p.peek_token()
            && tok.kind != TokenKind::RParen
        {
            let ident = p.identifier()?;
            p.expect_token_kind(Some("':'"), kind!(TokenKind::Colon))?;
            let ty = parse_expression(p, BindingPower::None, recursion + 1, ParseMode::NoBlocks)
                .expected("type")?;
            params.push((ident, ty));
            if p.consume(kind!(TokenKind::Comma)).is_none() {
                break;
            }
        }

        p.expect_token_kind(Some("')'"), kind!(TokenKind::RParen))?;

        let ret_ty = if p.peek_is(kind!(TokenKind::LBrace)) {
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
            id: p.node_id(),
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

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_func_item(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_func_item_mut(self);
    }
}
