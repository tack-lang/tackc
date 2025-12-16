use tackc_ast::{Block, ConstItem, Expression, FuncItem, Item, MaybeError, NodeId};
use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, BindingPower, ParseMode, Visitor, VisitorMut, parse_expression},
    error::{DiagResult, ParseError, ParseErrors, Result, collect_error},
};

impl AstNode for Item {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        p.check_recursion(recursion + 1)?;
        let tok = if p.peek_is(kind!(TokenKind::Exp)) {
            p.expect_peek_token2(None)?
        } else {
            p.expect_peek_token(None)?
        };
        match tok.kind {
            TokenKind::Const => p
                .parse::<ConstItem>(recursion + 1)
                .map(Box::new)
                .map(Item::ConstItem),
            TokenKind::Func => p
                .parse::<FuncItem>(recursion + 1)
                .map(Box::new)
                .map(Item::FuncItem),
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

    fn id(&self) -> NodeId {
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

impl AstNode for ConstItem {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let exported = p.consume(kind!(TokenKind::Exp)).is_some();
        let const_tok = p.expect_token_kind(None, kind!(TokenKind::Const))?;
        let ident = p.identifier()?;
        let ty = if p.consume(kind!(TokenKind::Colon)).is_some() {
            let ty = p.parse::<Expression>(recursion + 1).expected("type")?;
            Some(ty)
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
            exported,
            ident,
            ty,
            expr,
            binding: None,
            id: p.node_id(),
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        format!(
            "{}const {}{} = {};",
            if self.exported { "exp " } else { "" },
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

pub(crate) fn expr_list_sync<I, F: File>(p: &mut Parser<I, F>)
where
    I: Iterator<Item = Token> + Clone,
{
    let mut depth = 0;
    loop {
        let Some(tok) = p.peek_token() else {
            return;
        };
        match tok.kind {
            TokenKind::RParen if depth == 0 => {
                return;
            }
            TokenKind::Comma if depth == 0 => {
                p.next_token();
                return;
            }
            TokenKind::LBrace | TokenKind::LBracket | TokenKind::LParen => {
                depth += 1;
                p.next_token();
            }
            TokenKind::RBrace | TokenKind::RBracket | TokenKind::RParen => {
                depth -= 1;
                p.next_token();
            }
            _ => {
                p.next_token();
            }
        }
    }
}

fn ret_type_sync<I, F: File>(p: &mut Parser<I, F>)
where
    I: Iterator<Item = Token> + Clone,
{
    let mut depth = 0;
    loop {
        let Some(tok) = p.peek_token() else {
            return;
        };
        match tok.kind {
            TokenKind::LBrace if depth == 0 => {
                return;
            }
            TokenKind::LBrace => {
                depth += 1;
                p.next_token();
            }
            TokenKind::RBrace => {
                depth -= 1;
                p.next_token();
            }
            _ => {
                p.next_token();
            }
        }
    }
}

impl AstNode for FuncItem {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let exported = p.consume(kind!(TokenKind::Exp)).is_some();
        let func = p.expect_token_kind(None, kind!(TokenKind::Func))?;
        let ident = p.identifier()?;
        p.expect_token_kind(Some("'('"), kind!(TokenKind::LParen))?;
        let mut errors: Option<ParseErrors> = None;

        // Parameter list
        let mut params = Vec::new();
        while let Some(tok) = p.peek_token()
            && tok.kind != TokenKind::RParen
        {
            let ident = p.identifier()?;
            p.expect_token_kind(Some("':'"), kind!(TokenKind::Colon))?;

            let ty = match p.try_run(|p| {
                parse_expression(p, BindingPower::None, recursion + 1, ParseMode::NoBlocks)
                    .expected("type")
            }) {
                Ok(ty) => ty,
                Err(e) => {
                    collect_error(&mut errors, e);
                    expr_list_sync(p);
                    params.push((ident, None, None));
                    continue;
                }
            };

            params.push((ident, Some(ty), None));
            if p.consume(kind!(TokenKind::Comma)).is_none() {
                break;
            }
        }

        p.expect_token_kind(Some("')'"), kind!(TokenKind::RParen))?;

        let ret_ty = if p.peek_is(kind!(TokenKind::LBrace)) {
            MaybeError::None
        } else {
            let res = p.try_run(|p| {
                parse_expression(p, BindingPower::None, recursion + 1, ParseMode::NoBlocks)
                    .expected("type")
            });
            match res {
                Ok(expr) => MaybeError::Some(expr),
                Err(e) => {
                    collect_error(&mut errors, e);
                    ret_type_sync(p);
                    MaybeError::Err
                }
            }
        };

        let block = match p.parse::<Block>(recursion + 1).expected("block") {
            Ok(block) => block,
            Err(e) => {
                if let Some(mut err) = errors {
                    err.merge(e);
                    return Err(err);
                }
                return Err(e);
            }
        };

        if let Some(err) = errors {
            return Err(err);
        }

        Ok(FuncItem {
            span: Span::new_from(func.span.start, block.span.end),
            exported,
            ident,
            params,
            ret_ty,
            block,
            binding: None,
            id: p.node_id(),
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        let mut parts = Vec::with_capacity(self.params.len());
        for (ident, ty, _) in &self.params {
            parts.push(format!(
                "{}: {}",
                ident.display(global),
                match ty {
                    Some(ty) => ty.display(global),
                    None => String::from("<ERROR>"),
                }
            ));
        }
        let ret = self
            .ret_ty
            .as_ref()
            .map(|ty| ty.display(global) + " ")
            .unwrap_or_default();
        if parts.is_empty() {
            format!(
                "{}func {}() {ret}{}",
                if self.exported { "exp " } else { "" },
                self.ident.display(global),
                self.block.display(global)
            )
        } else {
            format!(
                "{}func {}({}) {ret}{}",
                if self.exported { "exp " } else { "" },
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
