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
    const NAME: &str;

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

    Grouping(Box<Expr>),

    Neg(Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn new(span: Span, kind: ExprKind) -> Self {
        Expr { span, kind }
    }

    const fn prefix_bp(tok: &TokenKind) -> Option<((), u32)> {
        match tok {
            TokenKind::Plus | TokenKind::Dash => Some(((), 5)),
            _ => None,
        }
    }

    #[inline]
    fn wrap_prefix(tok: &Token, rhs: Expr) -> Expr {
        let span = rhs.span;
        let kind = match tok.kind {
            TokenKind::Plus => rhs.kind,
            TokenKind::Dash => ExprKind::Neg(Box::new(rhs)),
            _ => unreachable!("should not be reached, code was written wrong"),
        };
        Expr::new(Span::new_from(tok.span.start, span.end), kind)
    }

    const fn infix_bp(tok: &TokenKind) -> Option<(u32, u32)> {
        match tok {
            TokenKind::Plus | TokenKind::Dash => Some((1, 2)),
            TokenKind::Star | TokenKind::Slash => Some((3, 4)),
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

    fn grouping<I>(p: &mut Parser<I>, opening: Span) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let inner = Self::parse_bp(p, 0)?;
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

    fn atom<I>(p: &mut Parser<I>) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        p.try_parse::<Atom>()
            .map(|v| Expr::new(v.span, ExprKind::Atomic(v)))
            .or_else(|_| {
                let snapshot = p.snapshot();

                let tok = p.expect_token(None)?;
                if tok.kind == TokenKind::OpenParen {
                    Self::grouping(p, tok.span)
                } else {
                    p.restore(snapshot);
                    Err(ParseErrors::new(ParseError::new(None, tok)))
                }
            })
    }

    fn prefix<I>(p: &mut Parser<I>) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let snapshot = p.snapshot();

        let tok = p.expect_token(None)?;
        let Some(((), r_bp)) = Self::prefix_bp(&tok.kind) else {
            p.restore(snapshot);
            return Self::atom(p);
        };
        let rhs = Self::parse_bp(p, r_bp)?;
        Ok(Self::wrap_prefix(&tok, rhs))
    }

    fn parse_bp<I>(p: &mut Parser<I>, min_bp: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let mut lhs = Self::prefix(p).expected("expression")?;

        loop {
            let Some(op) = p.peek_token() else {
                break;
            };

            if let Some((l_bp, ())) = Self::postfix_bp(&op.kind) {
                if l_bp < min_bp {
                    break;
                }
                p.next_token();

                lhs = Self::wrap_postfix(lhs, &op);
                continue;
            }

            let Some((l_bp, r_bp)) = Self::infix_bp(&op.kind) else {
                break;
            };

            if l_bp < min_bp {
                break;
            }

            p.next_token();
            let rhs = Self::parse_bp(p, r_bp)?;

            lhs = Self::wrap_infix(lhs, &op, rhs);
        }

        Ok(lhs)
    }

    pub fn display(&self, global: &Global) -> impl Display {
        match &self.kind {
            ExprKind::Atomic(value) => format!("{}", value.display(global)),
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
    const NAME: &str = "expression";

    fn parse<I>(p: &mut Parser<I>) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        Self::parse_bp(p, 0)
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
            AtomKind::FloatLit(lit) | AtomKind::IntLit(lit) => format!("{lit}"),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            AtomKind::Identifier(interned) => write!(f, "{interned:?}"),
            AtomKind::FloatLit(lit) | AtomKind::IntLit(lit) => write!(f, "{lit}"),
        }
    }
}

impl AstNode for Atom {
    const NAME: &str = "atomic value";

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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AtomKind {
    IntLit(Box<str>),
    FloatLit(Box<str>),
    Identifier(Interned<str>),
}
