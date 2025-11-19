use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::error::{DiagResult, ParseError, ParseErrors, Result};
use tackc_global::{Global, Interned};
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::Parser;

#[cfg(feature = "serde")]
pub trait Serde: serde::Serialize + for<'a> serde::Deserialize<'a> {}
#[cfg(feature = "serde")]
impl<T: serde::Serialize + for<'a> serde::Deserialize<'a>> Serde for T {}
#[cfg(not(feature = "serde"))]
pub trait Serde {}
#[cfg(not(feature = "serde"))]
impl<T> Serde for T {}

pub trait AstNode: Debug + Display + PartialEq + Eq + Hash + Clone + Sized + Serde {
    /// Parse the AST node using the given parser.
    ///
    /// # Errors
    /// If the node cannot be parsed from the given parser, this function will return an error.
    /// The parser may be in any state; do not trust it.
    /// If you need to, the parser can be cloned previous to calling this function.
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone;
    fn span(&self) -> Span;
}

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
        (|p, tok, recursion| {
            let rhs = parse_bp(p, $r_bp, recursion + 1)?;
            Ok(Expr::new(
                Span::new_from(tok.span.start, rhs.span.end),
                $fn(Box::new(rhs)),
            ))
        })
    };
}

macro_rules! infix_led {
    ($fn:expr) => {
        (|lhs, rhs| {
            Expr::new(
                Span::new_from(lhs.span.start, rhs.span.end),
                $fn(Box::new(lhs), Box::new(rhs)),
            )
        })
    };
}

fn prefix_nud<I>(tok: &TokenKind) -> Option<NudCallback<I>>
where
    I: Iterator<Item = Token> + Clone,
{
    match tok {
        TokenKind::Plus => Some(wrapping_nud!(|x: Box<Expr>| { x.kind }, 50)),
        TokenKind::Dash => Some(wrapping_nud!(ExprKind::Neg, 50)),
        TokenKind::OpenParen => Some(grouping),
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
        TokenKind::Plus | TokenKind::Dash => Some((10, 20)),
        TokenKind::Star | TokenKind::Slash => Some((30, 40)),
        _ => None,
    }
}

const fn postfix_bp(tok: &TokenKind) -> Option<u32> {
    match tok {
        TokenKind::OpenParen => Some(60),
        TokenKind::Dot => Some(70),
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
    let closing = p.expect_token(Some("')'"))?;
    let Token {
        span: closing_span,
        kind: TokenKind::CloseParen,
    } = closing
    else {
        return Err(ParseErrors::new(ParseError::new(Some("')'"), closing)));
    };
    Ok(Expr::new(
        Span::new_from(opening_span.start, closing_span.end),
        ExprKind::Grouping(Box::new(inner)),
    ))
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
    let tok = p.expect_token(Some("identifier"))?;

    let Token {
        span,
        kind: TokenKind::Ident(ident),
    } = tok
    else {
        return Err(ParseErrors::new(ParseError::new(Some("identifier"), tok)));
    };

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

    let snapshot = p.snapshot();
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
            p.restore(snapshot);
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

    let mut lhs = prefix(p, recursion + 1).expected("expression")?;

    loop {
        let Some(op) = p.peek_token() else {
            break;
        };

        if let (Some(l_bp), Some(led)) = (postfix_bp(&op.kind), postfix_led(&op.kind)) {
            if l_bp < min_bp {
                break;
            }

            p.next_token();

            lhs = led(p, op, lhs, recursion + 1).expected("expression")?;

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

    pub fn display(&self, global: &Global) -> impl Display {
        match &self.kind {
            ExprKind::Atomic(value) => format!("{}", value.display(global)),
            ExprKind::Call(lhs, args) => format!(
                "(call {} {})",
                lhs.display(global),
                args.iter()
                    .map(|arg| arg.display(global).to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ExprKind::Member(lhs, field) => {
                format!("(. {} {})", lhs.display(global), field.display(global))
            }
            ExprKind::Grouping(value) => format!("{}", value.display(global)),
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

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExprKind::Atomic(value) => write!(f, "{value}"),
            ExprKind::Call(lhs, args) => write!(
                f,
                "call ({} {})",
                lhs,
                args.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ExprKind::Member(lhs, field) => write!(f, "(. {lhs} {field:?})"),
            ExprKind::Grouping(value) => write!(f, "{value}"),
            ExprKind::Neg(rhs) => write!(f, "(- {rhs})"),
            ExprKind::Add(lhs, rhs) => write!(f, "(+ {lhs} {rhs})"),
            ExprKind::Sub(lhs, rhs) => write!(f, "(- {lhs} {rhs})"),
            ExprKind::Mul(lhs, rhs) => write!(f, "(* {lhs} {rhs})"),
            ExprKind::Div(lhs, rhs) => write!(f, "(/ {lhs} {rhs})"),
        }
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

    fn display(&self, global: &Global) -> impl Display {
        match &self.kind {
            AtomKind::Identifier(interned) => format!("{}", interned.display(global)),
            AtomKind::FloatLit(lit) | AtomKind::IntLit(lit) => format!("{}", lit.display(global)),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            AtomKind::Identifier(interned) => write!(f, "{interned:?}"),
            AtomKind::FloatLit(lit) | AtomKind::IntLit(lit) => write!(f, "{lit:?}"),
        }
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
            TokenKind::IntLit(literal) => Ok(Atom::new(tok.span, AtomKind::IntLit(literal))),
            TokenKind::FloatLit(literal) => Ok(Atom::new(tok.span, AtomKind::FloatLit(literal))),
            _ => Err(ParseErrors::new(ParseError::new(None, tok))),
        }
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AtomKind {
    IntLit(Interned<str>),
    FloatLit(Interned<str>),
    Identifier(Interned<str>),
}
