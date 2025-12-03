use std::fmt::Debug;
use std::hash::Hash;

use super::AstNode;
use crate::Parser;
use crate::error::{DiagResult, ParseError, ParseErrors, Result};
use tackc_global::{Global, Interned};
use tackc_lexer::{IntegerBase, Token, TokenKind};
use tackc_span::Span;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ParseMode {
    Normal,
    NoBlocks,
}

impl ParseMode {
    pub fn is_normal(self) -> bool {
        matches!(self, Self::Normal)
    }

    pub fn is_no_blocks(self) -> bool {
        matches!(self, Self::NoBlocks)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[test]
#[cfg(feature = "serde")]
fn expr_test_glob() {
    use insta::glob;

    glob!("expr/*.tck", run_expr_test);
}

#[cfg(all(test, feature = "serde"))]
use std::path::Path;

#[cfg(all(test, feature = "serde"))]
fn run_expr_test(path: &Path) {
    use tackc_error::iter::IteratorExt;
    use tackc_file::OwnedFile;
    use tackc_lexer::Lexer;

    let global = Global::create_heap();
    let src = OwnedFile::try_from(path.to_path_buf())
        .unwrap_or_else(|_| panic!("Failed to open file {}", path.display()));
    let lexer = Lexer::new(&src, &global).consume_reporter(drop);
    let mut p = Parser::new(lexer);
    let expr = Expression::parse(&mut p, 0).expected("expression");
    insta::assert_ron_snapshot!(expr);
}

impl Expression {
    fn new(kind: ExpressionKind, span: Span) -> Self {
        Expression { kind, span }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExpressionKind {
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Neg(Box<Expression>),

    Call(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Member(Box<Expression>, Interned<str>),

    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Lt(Box<Expression>, Box<Expression>),
    GtEq(Box<Expression>, Box<Expression>),
    LtEq(Box<Expression>, Box<Expression>),

    Binding(Interned<str>),
    IntLit(Interned<str>, IntegerBase),
    FloatLit(Interned<str>),
}

impl AstNode for Expression {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        parse_expression(p, BindingPower::None, recursion + 1, ParseMode::Normal)
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        match &self.kind {
            ExpressionKind::Add(lhs, rhs) => {
                format!("(+ {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::Sub(lhs, rhs) => {
                format!("(- {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::Mul(lhs, rhs) => {
                format!("(* {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::Div(lhs, rhs) => {
                format!("(/ {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::Neg(rhs) => format!("(- {})", rhs.display(global)),

            ExpressionKind::Call(lhs, args) => {
                let mut parts = Vec::with_capacity(args.len());
                for a in args {
                    parts.push(a.display(global));
                }
                if parts.is_empty() {
                    format!("(call {})", lhs.display(global))
                } else {
                    format!("(call {} {})", lhs.display(global), parts.join(", "))
                }
            }
            ExpressionKind::Index(lhs, rhs) => {
                format!("(index {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::Member(lhs, name) => {
                format!("(. {} {})", lhs.display(global), name.display(global))
            }

            ExpressionKind::Equal(lhs, rhs) => {
                format!("(eq {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::NotEqual(lhs, rhs) => {
                format!("(ne {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::Gt(lhs, rhs) => {
                format!("(gt {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::Lt(lhs, rhs) => {
                format!("(lt {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::GtEq(lhs, rhs) => {
                format!("(gteq {} {})", lhs.display(global), rhs.display(global))
            }
            ExpressionKind::LtEq(lhs, rhs) => {
                format!("(lteq {} {})", lhs.display(global), rhs.display(global))
            }

            ExpressionKind::Binding(ident) => ident.display(global).to_string(),
            ExpressionKind::IntLit(str, base) => format!("{base}{}", str.display(global)),
            ExpressionKind::FloatLit(str) => str.display(global).to_string(),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum BindingPower {
    None = 0,

    EqualityLeft = 10,
    EqualityRight = 11,

    TermLeft = 20,
    TermRight = 21,

    FactorLeft = 30,
    FactorRight = 31,

    Prefix = 50,

    Postfix = 60,
}

fn parse_primary<I>(p: &mut Parser<I>) -> Result<Expression>
where
    I: Iterator<Item = Token> + Clone,
{
    let tok = p.expect_token(None)?;
    match tok.kind {
        TokenKind::Ident(ident) => Ok(Expression::new(ExpressionKind::Binding(ident), tok.span)),
        TokenKind::IntLit(str, base) => {
            Ok(Expression::new(ExpressionKind::IntLit(str, base), tok.span))
        }
        TokenKind::FloatLit(str) => Ok(Expression::new(ExpressionKind::FloatLit(str), tok.span)),
        _ => Err(ParseErrors::new(ParseError::new(None, tok))),
    }
}

fn parse_prefix<I>(p: &mut Parser<I>, recursion: u32, mode: ParseMode) -> Result<Expression>
where
    I: Iterator<Item = Token> + Clone,
{
    let snapshot = p.snapshot();
    let tok = p.expect_token(None)?;
    match tok.kind {
        TokenKind::Plus => {
            // Drop unary `+`, does nothing
            let mut rhs = parse_expression(p, BindingPower::Prefix, recursion + 1, mode)?;
            rhs.span.start = tok.span.start;
            Ok(rhs)
        }
        TokenKind::Minus => {
            let rhs = parse_expression(p, BindingPower::Prefix, recursion + 1, mode)?;
            let rhs_span = rhs.span;
            Ok(Expression::new(
                ExpressionKind::Neg(Box::new(rhs)),
                Span::new_from(tok.span.start, rhs_span.end),
            ))
        }
        TokenKind::LParen => {
            // Ignore parse mode
            let mut rhs =
                parse_expression(p, BindingPower::None, recursion + 1, ParseMode::Normal)?;
            let closing = p.expect_token_kind(Some("')'"), token_kind!(TokenKind::RParen))?;
            rhs.span.start = tok.span.start;
            rhs.span.end = closing.span.end;
            Ok(rhs)
        }
        _ => {
            p.restore(snapshot);
            parse_primary(p)
        }
    }
}

fn infix_and_postfix_binding_power(kind: TokenKind) -> Option<BindingPower> {
    use BindingPower as P;
    match kind {
        TokenKind::EqEq | TokenKind::BangEq => Some(P::EqualityLeft),
        TokenKind::Plus | TokenKind::Minus => Some(P::TermLeft),
        TokenKind::Star | TokenKind::Slash => Some(P::FactorLeft),
        TokenKind::LParen | TokenKind::LBracket | TokenKind::Dot => Some(P::Postfix),
        _ => None,
    }
}

fn led_binary<I>(
    p: &mut Parser<I>,
    lhs: Expression,
    recursion: u32,
    rbp: BindingPower,
    construct: impl Fn(Box<Expression>, Box<Expression>) -> ExpressionKind,
    mode: ParseMode,
) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    let rhs = parse_expression(p, rbp, recursion + 1, mode)?;
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    Ok(OperatorResult::Continue(Expression::new(
        construct(Box::new(lhs), Box::new(rhs)),
        Span::new_from(lhs_span.start, rhs_span.end),
    )))
}

enum OperatorResult {
    Continue(Expression),
    Break(Expression),
}

fn member<I>(p: &mut Parser<I>, lhs: Expression) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    let (ident, span) = p.identifier()?;
    let lhs_span = lhs.span;

    Ok(OperatorResult::Continue(Expression::new(
        ExpressionKind::Member(Box::new(lhs), ident),
        Span::new_from(lhs_span.start, span.end),
    )))
}

fn index<I>(p: &mut Parser<I>, lhs: Expression, recursion: u32) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    let lhs_span = lhs.span;
    // Ignore parse mode
    let rhs = parse_expression(p, BindingPower::None, recursion + 1, ParseMode::Normal)
        .expected("expression")?;
    let closing = p.expect_token_kind(Some("']'"), token_kind!(TokenKind::RBracket))?;
    Ok(OperatorResult::Continue(Expression::new(
        ExpressionKind::Index(Box::new(lhs), Box::new(rhs)),
        Span::new_from(lhs_span.start, closing.span.end),
    )))
}

fn call<I>(
    p: &mut Parser<I>,
    lhs: Expression,
    recursion: u32,
    mode: ParseMode,
) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    let tok = p.expect_peek_token(Some("')' or expression"))?;
    let lhs_span = lhs.span;

    if tok.kind == TokenKind::RParen {
        p.next_token();
        return Ok(OperatorResult::Continue(Expression::new(
            ExpressionKind::Call(Box::new(lhs), Vec::new()),
            Span::new_from(lhs_span.start, tok.span.end),
        )));
    }

    let mut args = Vec::new();
    let expr =
        parse_expression(p, BindingPower::None, recursion + 1, mode).expected("expression")?;
    args.push(expr);

    while let Some(tok) = p.peek_token()
        && tok.kind != TokenKind::RParen
    {
        let _comma = p.expect_token_kind(Some("',' or ')'"), token_kind!(TokenKind::Comma))?;
        // Ignore parse mode
        let expr = parse_expression(p, BindingPower::None, recursion + 1, ParseMode::Normal)
            .expected("expression")?;
        args.push(expr);
    }

    let tok = p.expect_token_kind(Some("')'"), token_kind!(TokenKind::RParen))?;

    Ok(OperatorResult::Continue(Expression::new(
        ExpressionKind::Call(Box::new(lhs), args),
        Span::new_from(lhs_span.start, tok.span.end),
    )))
}

fn parse_postfix_or_infix<I>(
    p: &mut Parser<I>,
    lhs: Expression,
    tok: Token,
    min_bp: BindingPower,
    recursion: u32,
    mode: ParseMode,
) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    // find infix binding power for this operator
    let Some(lbp) = infix_and_postfix_binding_power(tok.kind) else {
        return Ok(OperatorResult::Break(lhs));
    };
    if lbp < min_bp {
        return Ok(OperatorResult::Break(lhs));
    }

    p.next_token(); // now consume the operator

    match tok.kind {
        TokenKind::Plus => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::TermRight,
            ExpressionKind::Add,
            mode,
        ),
        TokenKind::Minus => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::TermRight,
            ExpressionKind::Sub,
            mode,
        ),
        TokenKind::Star => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::FactorRight,
            ExpressionKind::Mul,
            mode,
        ),
        TokenKind::Slash => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::FactorRight,
            ExpressionKind::Div,
            mode,
        ),
        TokenKind::EqEq => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::EqualityRight,
            ExpressionKind::Equal,
            mode,
        ),
        TokenKind::BangEq => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::EqualityRight,
            ExpressionKind::NotEqual,
            mode,
        ),
        TokenKind::LParen => call(p, lhs, recursion + 1, mode),
        TokenKind::LBracket => index(p, lhs, recursion + 1), // Parse mode is not passed, since `index` only parses value inside brackets
        TokenKind::Dot => member(p, lhs), // Parse mode is not passed, since `member` never parses expressions
        _ => unreachable!(),
    }
}

/// Parses an expression using the given parser, minimum binding power, and parsing mode.
///
/// # Errors
/// This function will return an error if it fails to parse an expression.
pub fn parse_expression<I>(
    p: &mut Parser<I>,
    min_bp: BindingPower,
    recursion: u32,
    mode: ParseMode,
) -> Result<Expression>
where
    I: Iterator<Item = Token> + Clone,
{
    p.check_recursion(recursion)?;

    // --- prefix / nud ---
    let mut lhs = parse_prefix(p, recursion + 1, mode)?;

    // --- infix/postfix loop ---
    loop {
        let tok = p.peek_token(); // lookahead, do NOT consume yet
        let Some(tok) = tok else { break Ok(lhs) };

        match parse_postfix_or_infix(p, lhs, tok, min_bp, recursion + 1, mode)? {
            OperatorResult::Continue(new) => lhs = new,
            OperatorResult::Break(new) => break Ok(new),
        }
    }
}
