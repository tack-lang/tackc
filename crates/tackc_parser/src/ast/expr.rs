use std::fmt::Debug;
use std::hash::Hash;

use crate::error::{ParseError, ParseErrors, Result};
use tackc_global::{Global, Interned};
use tackc_lexer::{IntegerBase, Token, TokenKind};
use tackc_span::Span;

use super::AstNode;
use crate::Parser;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExprKind {
    Atomic(Atom),

    Call(Box<Expr>, Vec<Expr>),
    Member(Box<Expr>, Interned<str>),

    Grouping(Box<Expr>),
    Block(Block),

    Neg(Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

pub type NudCallback<I> = fn(p: &mut Parser<I>, tok: Token, recursion: u32) -> Result<Expr>;
pub type InfixLedCallback = fn(lhs: Expr, rhs: Expr) -> Expr;
pub type PostfixLedCallback<I> =
    fn(p: &mut Parser<I>, tok: Token, lhs: Expr, recursion: u32) -> Result<Expr>;

macro_rules! wrapping_nud {
    ($fn:expr, $r_bp:expr) => {
        |p, tok, recursion| {
            let rhs = parse_bp(p, $r_bp, recursion + 1)?;
            Ok(Expr::new(
                Span::new_from(tok.span.start, rhs.span.end),
                $fn(Box::new(rhs)),
            ))
        }
    };
}

macro_rules! infix_led {
    ($fn:expr) => {
        |lhs, rhs| {
            Expr::new(
                Span::new_from(lhs.span.start, rhs.span.end),
                $fn(Box::new(lhs), Box::new(rhs)),
            )
        }
    };
}

fn prefix_nud<I>(tok: &TokenKind) -> Option<NudCallback<I>>
where
    I: Iterator<Item = Token> + Clone,
{
    match tok {
        TokenKind::Plus => Some(wrapping_nud!(|x: Box<Expr>| { x.kind }, 80)),
        TokenKind::Dash => Some(wrapping_nud!(ExprKind::Neg, 80)),
        TokenKind::OpenParen => Some(grouping),
        TokenKind::OpenBrace => Some(|p, tok, recursion| {
            block(p, tok, recursion).map(|b| Expr::new(b.span, ExprKind::Block(b)))
        }),
        _ => None,
    }
}

fn infix_led(tok: &TokenKind) -> Option<InfixLedCallback> {
    match tok {
        TokenKind::Plus => Some(infix_led!(ExprKind::Add)),
        TokenKind::Dash => Some(infix_led!(ExprKind::Sub)),
        TokenKind::Star => Some(infix_led!(ExprKind::Mul)),
        TokenKind::Slash => Some(infix_led!(ExprKind::Div)),
        _ => None,
    }
}

const fn infix_bp(tok: &TokenKind) -> Option<(u32, u32)> {
    match tok {
        TokenKind::Plus | TokenKind::Dash => Some((59, 60)),
        TokenKind::Star | TokenKind::Slash => Some((69, 70)),
        _ => None,
    }
}

const fn postfix_bp(tok: &TokenKind) -> Option<u32> {
    match tok {
        TokenKind::OpenParen | TokenKind::Dot => Some(90),
        _ => None,
    }
}

fn postfix_led<I>(tok: &TokenKind) -> Option<PostfixLedCallback<I>>
where
    I: Iterator<Item = Token> + Clone,
{
    match tok {
        TokenKind::OpenParen => Some(call),
        TokenKind::Dot => Some(member),
        _ => None,
    }
}

#[allow(clippy::needless_pass_by_value)]
fn grouping<I>(p: &mut Parser<I>, opening: Token, recursion: u32) -> Result<Expr>
where
    I: Iterator<Item = Token> + Clone,
{
    let opening_span = opening.span;

    let inner = parse_bp(p, 0, recursion + 1)?;
    let closing_span = p.expect_token_kind(Some("')'"), token_kind!(TokenKind::CloseParen))?;

    Ok(Expr::new(
        Span::new_from(opening_span.start, closing_span.span.end),
        ExprKind::Grouping(Box::new(inner)),
    ))
}

#[allow(clippy::needless_pass_by_value)]
fn block<I>(p: &mut Parser<I>, opening: Token, recursion: u32) -> Result<Block>
where
    I: Iterator<Item = Token> + Clone,
{
    let opening_span = opening.span;
    let mut exprs = Vec::new();

    while !p.is_eof() {
        if p.peek_is(token_kind!(TokenKind::CloseBrace)) {
            break;
        }

        let expr = parse_bp(p, 0, recursion + 1)?;
        exprs.push(expr);
        if !p.consume(token_kind!(TokenKind::Semicolon)) {
            break;
        }
    }
    let closing_span = p.expect_token_kind(Some("'}'"), token_kind!(TokenKind::CloseBrace))?;

    let block = Block {
        exprs,
        span: Span::new_from(opening_span.start, closing_span.span.end),
    };

    Ok(block)
}

fn prefix<I>(p: &mut Parser<I>, recursion: u32) -> Result<Expr>
where
    I: Iterator<Item = Token> + Clone,
{
    let snapshot = p.snapshot();

    let tok = p.expect_token(None)?;
    let Some(nud) = prefix_nud(&tok.kind) else {
        p.restore(snapshot);
        let atom = p.try_parse::<Atom>(recursion + 1)?;
        let expr = Expr::new(atom.span, ExprKind::Atomic(atom));
        return Ok(expr);
    };

    nud(p, tok, recursion + 1)
}

fn member<I>(p: &mut Parser<I>, _: Token, lhs: Expr, _: u32) -> Result<Expr>
where
    I: Iterator<Item = Token> + Clone,
{
    let (ident, span) = p.identifier()?;

    Ok(Expr::new(
        Span::new_from(lhs.span.start, span.end),
        ExprKind::Member(Box::new(lhs), ident),
    ))
}

fn call<I>(p: &mut Parser<I>, _: Token, lhs: Expr, recursion: u32) -> Result<Expr>
where
    I: Iterator<Item = Token> + Clone,
{
    let tok = p.expect_peek_token(Some("')', or expression"))?;

    if tok.kind == TokenKind::CloseParen {
        p.next_token();
        return Ok(Expr::new(
            Span::new_from(lhs.span.start, tok.span.end),
            ExprKind::Call(Box::new(lhs), Vec::new()),
        ));
    }

    let mut args = Vec::new();
    let closing_span = loop {
        let arg = parse_bp(p, 0, recursion + 1)?;
        args.push(arg);

        let tok = p.expect_peek_token(Some("',', or ')'"))?;
        if tok.kind == TokenKind::CloseParen {
            p.next_token();
            break tok.span;
        } else if tok.kind == TokenKind::Comma {
            p.next_token();
        } else {
            return Err(ParseErrors::new(ParseError::new(Some("',', or ')'"), tok)));
        }
    };

    Ok(Expr::new(
        Span::new_from(lhs.span.start, closing_span.end),
        ExprKind::Call(Box::new(lhs), args),
    ))
}

fn parse_bp<I>(p: &mut Parser<I>, min_bp: u32, recursion: u32) -> Result<Expr>
where
    I: Iterator<Item = Token> + Clone,
{
    p.check_recursion(recursion)?;

    let mut lhs = prefix(p, recursion + 1)?;

    loop {
        let Some(op) = p.peek_token() else {
            break;
        };

        if let (Some(l_bp), Some(led)) = (postfix_bp(&op.kind), postfix_led(&op.kind)) {
            if l_bp < min_bp {
                break;
            }

            p.next_token();

            lhs = led(p, op, lhs, recursion + 1)?;

            continue;
        }

        if let (Some((l_bp, r_bp)), Some(led)) = (infix_bp(&op.kind), infix_led(&op.kind)) {
            if l_bp < min_bp {
                break;
            }

            p.next_token();

            let rhs = parse_bp(p, r_bp, recursion + 1)?;
            lhs = led(lhs, rhs);
        } else {
            break;
        }
    }

    Ok(lhs)
}

impl Expr {
    fn new(span: Span, kind: ExprKind) -> Self {
        Expr { span, kind }
    }
}

impl AstNode for Expr {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        parse_bp(p, 0, recursion + 1)
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        match &self.kind {
            ExprKind::Atomic(value) => value.display(global),
            ExprKind::Call(lhs, args) => format!(
                "(call {} {})",
                lhs.display(global),
                args.iter()
                    .map(|arg| arg.display(global))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ExprKind::Member(lhs, field) => {
                format!("(. {} {})", lhs.display(global), field.display(global))
            }
            ExprKind::Grouping(value) => value.display(global),
            ExprKind::Block(b) => match &b.exprs[..] {
                [] => "()".to_string(),
                [expr] => format!("({})", expr.display(global)),
                [exprs @ .., last] => format!(
                    "{{{}; {}}}",
                    exprs
                        .iter()
                        .map(|arg| arg.display(global))
                        .collect::<Vec<_>>()
                        .join("; "),
                    last.display(global)
                ),
            },
            ExprKind::Neg(rhs) => format!("(- {})", rhs.display(global)),
            ExprKind::Add(lhs, rhs) => {
                format!("(+ {} {})", lhs.display(global), rhs.display(global))
            }
            ExprKind::Sub(lhs, rhs) => {
                format!("(- {} {})", lhs.display(global), rhs.display(global))
            }
            ExprKind::Mul(lhs, rhs) => {
                format!("(* {} {})", lhs.display(global), rhs.display(global))
            }
            ExprKind::Div(lhs, rhs) => {
                format!("(/ {} {})", lhs.display(global), rhs.display(global))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Atom {
    pub span: Span,
    pub kind: AtomKind,
}

impl Atom {
    fn new(span: Span, kind: AtomKind) -> Self {
        Atom { span, kind }
    }
}

impl AstNode for Atom {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        p.check_recursion(recursion)?;

        let tok = p.expect_token(None)?;
        match tok.kind {
            TokenKind::Ident(ident) => Ok(Atom::new(tok.span, AtomKind::Identifier(ident))),
            TokenKind::IntLit(literal, base) => {
                Ok(Atom::new(tok.span, AtomKind::IntLit(literal, base)))
            }
            TokenKind::FloatLit(literal) => Ok(Atom::new(tok.span, AtomKind::FloatLit(literal))),
            _ => Err(ParseErrors::new(ParseError::new(None, tok))),
        }
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        match &self.kind {
            AtomKind::Identifier(interned) => interned.display(global).to_string(),
            AtomKind::FloatLit(lit) => lit.display(global).to_string(),
            AtomKind::IntLit(lit, base) => format!("{base}{}", lit.display(global)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AtomKind {
    IntLit(Interned<str>, IntegerBase),
    FloatLit(Interned<str>),
    Identifier(Interned<str>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Block {
    pub exprs: Vec<Expr>,
    pub span: Span,
}

impl AstNode for Block {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let opening = p.expect_token_kind(None, token_kind!(TokenKind::OpenBrace))?;
        block(p, opening, recursion + 1)
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        match &self.exprs[..] {
            [] => "()".to_string(),
            [expr] => format!("({})", expr.display(global)),
            [exprs @ .., last] => format!(
                "{{{}; {}}}",
                exprs
                    .iter()
                    .map(|arg| arg.display(global))
                    .collect::<Vec<_>>()
                    .join("; "),
                last.display(global)
            ),
        }
    }
}
