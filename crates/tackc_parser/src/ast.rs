use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::error::{DiagResult, ParseError, ParseErrors, Result};
use tackc_global::{Global, Interned};
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::Parser;

const MAX_RECURSION_DEPTH: u32 = 256;

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
    fn parse<I>(p: &mut Parser<I>) -> Result<Self>
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

    Grouping(Box<Expr>),

    Neg(Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

pub type NudCallback<I> = fn(p: &mut Parser<I>, recursion: u32, tok: Token) -> Result<Expr>;

macro_rules! wrapping_nud {
    ($fn:expr, $r_bp:expr) => {
        (|p, recursion, tok| {
            let rhs = parse_bp(p, $r_bp, recursion + 1)?;
            Ok(Expr::new(
                Span::new_from(tok.span.start, rhs.span.end),
                $fn(Box::new(rhs)),
            ))
        })
    };
}

const fn prefix_nud<I>(tok: &TokenKind) -> Option<NudCallback<I>>
where
    I: Iterator<Item = Token> + Clone,
{
    match tok {
        TokenKind::Plus => Some(wrapping_nud!(|x: Box<Expr>| { x.kind }, 50)),
        TokenKind::Dash => Some(wrapping_nud!(ExprKind::Neg, 50)),
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

#[inline]
fn wrap_infix(lhs: Expr, tok: &Token, rhs: Expr) -> Expr {
    let rhs_span = rhs.span;
    let lhs_span = lhs.span;
    let kind = match tok.kind {
        TokenKind::Plus => ExprKind::Add(Box::new(lhs), Box::new(rhs)),
        TokenKind::Dash => ExprKind::Sub(Box::new(lhs), Box::new(rhs)),
        TokenKind::Star => ExprKind::Mul(Box::new(lhs), Box::new(rhs)),
        TokenKind::Slash => ExprKind::Div(Box::new(lhs), Box::new(rhs)),
        _ => unreachable!("should not be reached, code was written wrong"),
    };
    Expr::new(Span::new_from(lhs_span.start, rhs_span.end), kind)
}

#[allow(clippy::all)]
const fn postfix_bp(tok: &TokenKind) -> Option<(u32, ())> {
    match tok {
        _ => None,
    }
}

#[inline]
#[allow(clippy::all)]
#[allow(unused)]
#[allow(clippy::needless_pass_by_value)]
fn wrap_postfix(lhs: Expr, tok: &Token) -> Expr {
    let span = lhs.span;
    let kind = match tok.kind {
        _ => unreachable!("should not be reached, code was written wrong"),
    };
    Expr::new(Span::new_from(span.start, tok.span.end), kind)
}

fn grouping<I>(p: &mut Parser<I>, opening: Span, recursion: u32) -> Result<Expr>
where
    I: Iterator<Item = Token> + Clone,
{
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
        Span::new_from(opening.start, closing_span.end),
        ExprKind::Grouping(Box::new(inner)),
    ))
}

fn atom<I>(p: &mut Parser<I>, recursion: u32) -> Result<Expr>
where
    I: Iterator<Item = Token> + Clone,
{
    p.try_parse::<Atom>()
        .map(|v| Expr::new(v.span, ExprKind::Atomic(v)))
        .or_else(|_| {
            let snapshot = p.snapshot();

            let tok = p.expect_token(None)?;
            if tok.kind == TokenKind::OpenParen {
                grouping(p, tok.span, recursion + 1)
            } else {
                p.restore(snapshot);
                Err(ParseErrors::new(ParseError::new(None, tok)))
            }
        })
}

fn prefix<I>(p: &mut Parser<I>, recursion: u32) -> Result<Expr>
where
    I: Iterator<Item = Token> + Clone,
{
    let snapshot = p.snapshot();

    let tok = p.expect_token(None)?;
    let Some(nud) = prefix_nud(&tok.kind) else {
        p.restore(snapshot);
        return atom(p, recursion + 1);
    };

    nud(p, recursion + 1, tok)
}

fn call<I>(p: &mut Parser<I>, lhs: Expr, recursion: u32) -> Result<Expr>
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
    if recursion > MAX_RECURSION_DEPTH {
        return Err(ParseErrors::new(ParseError::recursion()));
    }

    let mut lhs = prefix(p, recursion + 1).expected("expression")?;

    loop {
        let Some(op) = p.peek_token() else {
            break;
        };

        if let Some((l_bp, ())) = postfix_bp(&op.kind) {
            if l_bp < min_bp {
                break;
            }

            p.next_token();

            lhs = wrap_postfix(lhs, &op);

            continue;
        }

        if let Some((l_bp, r_bp)) = infix_bp(&op.kind) {
            if l_bp < min_bp {
                break;
            }

            p.next_token();

            let rhs = parse_bp(p, r_bp, recursion + 1)?;
            lhs = wrap_infix(lhs, &op, rhs);

            continue;
        }

        match op.kind {
            TokenKind::OpenParen => {
                // Skip OpenParen
                p.next_token();

                lhs = call(p, lhs, recursion + 1)?;
            }
            _ => break,
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
                "{}({})",
                lhs.display(global),
                args.iter()
                    .map(|arg| arg.display(global).to_string())
                    .fold(String::new(), |mut a, arg| {
                        a += &arg;
                        a += ", ";
                        a
                    })
                    .strip_suffix(", ")
                    .unwrap_or_default()
            ),
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
                "{}({})",
                lhs,
                args.iter()
                    .map(ToString::to_string)
                    .fold(String::new(), |mut a, arg| {
                        a += &arg;
                        a += ", ";
                        a
                    })
                    .strip_suffix(", ")
                    .unwrap_or_default()
            ),
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
    fn parse<I>(p: &mut Parser<I>) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        parse_bp(p, 0, 0)
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
    fn parse<I>(p: &mut Parser<I>) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
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
