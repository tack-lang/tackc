use std::fmt::Debug;
use std::hash::Hash;

use super::AstNode;
use crate::Parser;
use crate::ast::item::expr_list_sync;
use crate::ast::{Primary, Visitor, VisitorMut};
use crate::error::{DiagResult, Result, collect_error};
use tackc_ast::error::ParseErrors;
use tackc_ast::{Block, Expression, ExpressionKind, NodeId};
use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

/// Different ways to parse an expression
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ParseMode {
    /// Parse normally
    Normal,
    /// Parse without blocks
    NoBlocks,
}

impl ParseMode {
    #[allow(missing_docs)]
    pub const fn is_normal(self) -> bool {
        matches!(self, Self::Normal)
    }

    #[allow(missing_docs)]
    pub const fn is_no_blocks(self) -> bool {
        matches!(self, Self::NoBlocks)
    }
}

impl AstNode for Expression {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        parse_expression(p, BindingPower::None, recursion + 1, ParseMode::Normal)
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        match &*self.kind {
            ExpressionKind::Grouping(inner) => inner.display(global),
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
                for arg in args {
                    if let Some(expr) = arg {
                        parts.push(expr.display(global));
                    } else {
                        parts.push(String::from("<ERROR>"));
                    }
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

            ExpressionKind::Primary(prim) => prim.display(global),

            ExpressionKind::Block(block) => block.display(global),
        }
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_expression(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_expression_mut(self);
    }
}

#[test]
fn expr_test_glob() {
    use insta::glob;

    glob!("expr-parse/*.tck", run_expr_test);
}

#[cfg(test)]
use std::path::Path;

#[cfg(test)]
fn run_expr_test(path: &Path) {
    use tackc_error::iter::IteratorExt;
    use tackc_file::BasicFile;
    use tackc_lexer::Lexer;

    let global = Global::create_heap();
    let src = std::fs::read_to_string(path).unwrap();
    let path = Path::new(path.file_name().unwrap());
    let file = BasicFile::new(src, path);

    let lexer = Lexer::new(&file, &global).consume_reporter(drop);
    let mut p = Parser::new(lexer, &global, &file);
    let expr = Expression::parse(&mut p, 0).expected("expression");
    insta::assert_ron_snapshot!(expr);
}

/// The binding power of operators
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum BindingPower {
    /// Used for "any binding power" in minimum binding power
    None = 0,

    /// The left binding power for comparison operators (`>`, `<`, `==`, `!=`, `>=`, `<=`)
    ComparisonLeft = 10,
    /// The right binding power for comparison operators (`>`, `<`, `==`, `!=`, `>=`, `<=`)
    ComparisonRight = 11,

    /// The left binding power for `+` and `-`
    TermLeft = 20,
    /// The right binding power for `+` and `-`
    TermRight = 21,

    /// The left binding power for `*` and `/`
    FactorLeft = 30,
    /// The right binding power for `*` and `/`
    FactorRight = 31,

    /// The binding power of prefix operators
    Prefix = 50,

    /// The binding power of postfix operators
    Postfix = 60,
}

fn parse_prefix<I, F: File>(
    p: &mut Parser<I, F>,
    recursion: u32,
    mode: ParseMode,
) -> Result<Expression>
where
    I: Iterator<Item = Token> + Clone,
{
    let tok = p.expect_peek_token(None)?;
    match tok.kind {
        TokenKind::Plus => {
            p.next_token();

            // Drop unary `+`, does nothing
            let mut rhs = parse_expression(p, BindingPower::Prefix, recursion + 1, mode)
                .expected("expression")?;
            rhs.span.start = tok.span.start;
            Ok(rhs)
        }
        TokenKind::Minus => {
            p.next_token();

            let rhs = parse_expression(p, BindingPower::Prefix, recursion + 1, mode)
                .expected("expression")?;
            Ok(Expression {
                span: Span::new_from(tok.span.start, rhs.span.end),
                kind: Box::new(ExpressionKind::Neg(Box::new(rhs))),
                id: p.node_id(),
            })
        }
        TokenKind::LParen => {
            p.next_token();

            // Ignore parse mode
            let inner = parse_expression(p, BindingPower::None, recursion + 1, ParseMode::Normal)
                .expected("expression")?;
            let closing = p.expect_token_kind(Some("')'"), kind!(TokenKind::RParen))?;
            Ok(Expression {
                span: Span::new_from(tok.span.start, closing.span.end),
                kind: Box::new(ExpressionKind::Grouping(Box::new(inner))),
                id: p.node_id(),
            })
        }
        TokenKind::LBrace => {
            if mode == ParseMode::NoBlocks {
                return p.parse::<Primary>(recursion + 1).map(|prim| Expression {
                    span: prim.span(),
                    kind: Box::new(ExpressionKind::Primary(prim)),
                    id: p.node_id(),
                });
            }

            let block = p.parse::<Block>(recursion + 1)?;

            Ok(Expression {
                span: block.span,
                kind: Box::new(ExpressionKind::Block(Box::new(block))),
                id: p.node_id(),
            })
        }
        _ => p.parse::<Primary>(recursion + 1).map(|prim| Expression {
            span: prim.span(),
            kind: Box::new(ExpressionKind::Primary(prim)),
            id: p.node_id(),
        }),
    }
}

const fn infix_and_postfix_binding_power(kind: TokenKind) -> Option<BindingPower> {
    use BindingPower as P;
    match kind {
        TokenKind::EqEq
        | TokenKind::BangEq
        | TokenKind::Gt
        | TokenKind::Lt
        | TokenKind::GtEq
        | TokenKind::LtEq => Some(P::ComparisonLeft),
        TokenKind::Plus | TokenKind::Minus => Some(P::TermLeft),
        TokenKind::Star | TokenKind::Slash => Some(P::FactorLeft),
        TokenKind::LParen | TokenKind::LBracket | TokenKind::Dot => Some(P::Postfix),
        _ => None,
    }
}

fn led_binary<I, F: File>(
    p: &mut Parser<I, F>,
    lhs: Expression,
    recursion: u32,
    rbp: BindingPower,
    construct: impl Fn(Box<Expression>, Box<Expression>) -> ExpressionKind,
    mode: ParseMode,
) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    let rhs = parse_expression(p, rbp, recursion + 1, mode).expected("expression")?;
    Ok(OperatorResult::Continue(Expression {
        span: Span::new_from(lhs.span.start, rhs.span.end),
        kind: Box::new(construct(Box::new(lhs), Box::new(rhs))),
        id: p.node_id(),
    }))
}

enum OperatorResult {
    Continue(Expression),
    Break(Expression),
}

fn member<I, F: File>(p: &mut Parser<I, F>, lhs: Expression) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    let ident = p.identifier()?;

    Ok(OperatorResult::Continue(Expression {
        span: Span::new_from(lhs.span.start, ident.span.end),
        kind: Box::new(ExpressionKind::Member(Box::new(lhs), ident)),
        id: p.node_id(),
    }))
}

fn index<I, F: File>(
    p: &mut Parser<I, F>,
    lhs: Expression,
    recursion: u32,
) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    // Ignore parse mode
    let rhs = parse_expression(p, BindingPower::None, recursion + 1, ParseMode::Normal)
        .expected("expression")?;
    let closing = p.expect_token_kind(Some("']'"), kind!(TokenKind::RBracket))?;
    Ok(OperatorResult::Continue(Expression {
        span: Span::new_from(lhs.span.start, closing.span.end),
        kind: Box::new(ExpressionKind::Index(Box::new(lhs), Box::new(rhs))),
        id: p.node_id(),
    }))
}

fn call<I, F: File>(p: &mut Parser<I, F>, lhs: Expression, recursion: u32) -> Result<OperatorResult>
where
    I: Iterator<Item = Token> + Clone,
{
    let mut args = Vec::new();
    let mut errors: Option<ParseErrors> = None;
    while let Some(tok) = p.peek_token()
        && tok.kind != TokenKind::RParen
    {
        let res = parse_expression(p, BindingPower::None, recursion + 1, ParseMode::Normal)
            .expected("expression");
        let expr = match res {
            Ok(expr) => expr,
            Err(e) => {
                collect_error(&mut errors, e);
                args.push(None);
                expr_list_sync(p);
                continue;
            }
        };
        args.push(Some(expr));
        if p.consume(kind!(TokenKind::Comma)).is_none() {
            break;
        }
    }

    let tok = p.expect_token_kind(Some("')'"), kind!(TokenKind::RParen))?;

    Ok(OperatorResult::Continue(Expression {
        span: Span::new_from(lhs.span.start, tok.span.end),
        kind: Box::new(ExpressionKind::Call(Box::new(lhs), args)),
        id: p.node_id(),
    }))
}

fn parse_postfix_or_infix<I, F: File>(
    p: &mut Parser<I, F>,
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
            BindingPower::ComparisonRight,
            ExpressionKind::Equal,
            mode,
        ),
        TokenKind::BangEq => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::ComparisonRight,
            ExpressionKind::NotEqual,
            mode,
        ),
        TokenKind::Gt => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::ComparisonRight,
            ExpressionKind::Gt,
            mode,
        ),
        TokenKind::Lt => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::ComparisonRight,
            ExpressionKind::Lt,
            mode,
        ),
        TokenKind::GtEq => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::ComparisonRight,
            ExpressionKind::GtEq,
            mode,
        ),
        TokenKind::LtEq => led_binary(
            p,
            lhs,
            recursion + 1,
            BindingPower::ComparisonRight,
            ExpressionKind::LtEq,
            mode,
        ),
        TokenKind::LParen => call(p, lhs, recursion + 1),
        TokenKind::LBracket => index(p, lhs, recursion + 1), // Parse mode is not passed, since `index` only parses value inside brackets
        TokenKind::Dot => member(p, lhs), // Parse mode is not passed, since `member` never parses expressions
        _ => unreachable!(),
    }
}

/// Parses an expression using the given parser, minimum binding power, and parsing mode.
///
/// # Errors
/// This function will return an error if it fails to parse an expression.
pub fn parse_expression<I, F: File>(
    p: &mut Parser<I, F>,
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
        let Some(tok) = p.peek_token() else {
            break Ok(lhs);
        }; // lookahead, do NOT consume yet

        match parse_postfix_or_infix(p, lhs, tok, min_bp, recursion + 1, mode)? {
            OperatorResult::Continue(new) => lhs = new,
            OperatorResult::Break(new) => break Ok(new),
        }
    }
}
