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
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    fn new(kind: ExpressionKind, span: Span) -> Self {
        Expression {
            kind,
            span,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExpressionKind {
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Neg(Box<Expression>),

    Binding(Interned<str>),
    IntLit(Interned<str>, IntegerBase),
    FloatLit(Interned<str>),
}

impl AstNode for Expression {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        parse_expression(p, BindingPower::None, recursion + 1)
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        match &self.kind {
            ExpressionKind::Add(lhs, rhs) => format!("(+ {} {})", lhs.display(global), rhs.display(global)),
            ExpressionKind::Sub(lhs, rhs) => format!("(- {} {})", lhs.display(global), rhs.display(global)),
            ExpressionKind::Mul(lhs, rhs) => format!("(* {} {})", lhs.display(global), rhs.display(global)),
            ExpressionKind::Div(lhs, rhs) => format!("(/ {} {})", lhs.display(global), rhs.display(global)),
            ExpressionKind::Neg(rhs) => format!("(- {})", rhs.display(global)),

            ExpressionKind::Binding(ident) => ident.display(global).to_string(),
            ExpressionKind::IntLit(str, base) => format!("{base}{}", str.display(global)),
            ExpressionKind::FloatLit(str) => str.display(global).to_string(),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum BindingPower {
    None = 0,

    TermLeft = 10,
    TermRight = 11,

    FactorLeft = 20,
    FactorRight = 21,

    Prefix = 50,
}

fn parse_primary<I>(p: &mut Parser<I>) -> Result<Expression>
where
    I: Iterator<Item = Token> + Clone,
{
    let tok = p.expect_token(None)?;
    match tok.kind {
        TokenKind::Ident(ident) => Ok(Expression::new(ExpressionKind::Binding(ident), tok.span)),
        TokenKind::IntLit(str, base) => Ok(Expression::new(ExpressionKind::IntLit(str, base), tok.span)),
        TokenKind::FloatLit(str) => Ok(Expression::new(ExpressionKind::FloatLit(str), tok.span)),
        _ => Err(ParseErrors::new(ParseError::new(None, tok))),
    }
}

fn parse_prefix<I>(p: &mut Parser<I>, recursion: u32) -> Result<Expression>
where
    I: Iterator<Item = Token> + Clone,
{
    let snapshot = p.snapshot();
    let tok = p.expect_token(None)?;
    match tok.kind {
        TokenKind::Plus => {
            // Drop unary `+`, does nothing
            let mut rhs = parse_expression(p, BindingPower::Prefix, recursion + 1)?;
            rhs.span.start = tok.span.start;
            Ok(rhs)
        },
        TokenKind::Dash => {
            let rhs = parse_expression(p, BindingPower::Prefix, recursion + 1)?;
            let rhs_span = rhs.span;
            Ok(Expression::new(ExpressionKind::Neg(Box::new(rhs)), Span::new_from(tok.span.start, rhs_span.end)))
        },
        TokenKind::OpenParen => {
            let mut rhs = parse_expression(p, BindingPower::None, recursion + 1)?;
            let closing = p.expect_token_kind(Some("')'"), token_kind!(TokenKind::CloseParen))?;
            rhs.span.start = tok.span.start;
            rhs.span.end = closing.span.end;
            Ok(rhs)
        }
        _ => {
            p.restore(snapshot);
            parse_primary(p)
        },
    }
}

fn infix_and_postfix_binding_power(kind: TokenKind) -> Option<(BindingPower, BindingPower)> {
    use BindingPower as P;
    match kind {
        TokenKind::Plus | TokenKind::Dash => Some((P::TermLeft, P::TermRight)),
        TokenKind::Star | TokenKind::Slash => Some((P::FactorLeft, P::FactorRight)),
        _ => None,
    }
}

fn led_binary<I>(p: &mut Parser<I>, lhs: Expression, recursion: u32, rbp: BindingPower, construct: impl Fn(Box<Expression>, Box<Expression>) -> ExpressionKind) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone
{
    let rhs = parse_expression(p, rbp, recursion + 1)?;
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    Ok(OperatorResult::Continue(Expression::new(construct(Box::new(lhs), Box::new(rhs)), Span::new_from(lhs_span.start, rhs_span.end))))
}

enum OperatorResult {
    Continue(Expression),
    Break(Expression),
}

fn parse_postfix_or_infix<I>(p: &mut Parser<I>, lhs: Expression, tok: Token, min_bp: BindingPower, recursion: u32) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    // find infix binding power for this operator
    let Some((lbp, rbp)) = infix_and_postfix_binding_power(tok.kind) else {
        return Ok(OperatorResult::Break(lhs))
    };
    if lbp < min_bp {
        return Ok(OperatorResult::Break(lhs));
    }

    p.next_token(); // now consume the operator

    match tok.kind {
        TokenKind::Plus => led_binary(p, lhs, recursion, rbp, ExpressionKind::Add),
        TokenKind::Dash => led_binary(p, lhs, recursion, rbp, ExpressionKind::Sub),
        TokenKind::Star => led_binary(p, lhs, recursion, rbp, ExpressionKind::Mul),
        TokenKind::Slash => led_binary(p, lhs, recursion, rbp, ExpressionKind::Div),
        _ => unreachable!(),
    }
}

fn parse_expression<I>(p: &mut Parser<I>, min_bp: BindingPower, recursion: u32) -> Result<Expression>
where
    I: Iterator<Item = Token> + Clone,
{
    p.check_recursion(recursion)?;

    // --- prefix / nud ---
    let mut lhs = parse_prefix(p, recursion + 1)?;

    // --- infix/postfix loop ---
    loop {
        let tok = p.peek_token();       // lookahead, do NOT consume yet
        let Some(tok) = tok else { break Ok(lhs) };

        match parse_postfix_or_infix(p, lhs, tok, min_bp, recursion + 1)? {
            OperatorResult::Continue(new) => lhs = new,
            OperatorResult::Break(new) => break Ok(new),
        }
    }
}
