use tackc_global::Global;
use tackc_lexer::TokenKind;
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, Expression, ExpressionKind},
    error::Result,
};

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
}

impl AstNode for Statement {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = tackc_lexer::Token> + Clone,
    {
        let tok = p.expect_peek_token(None)?;
        #[allow(clippy::match_single_binding)]
        match tok.kind {
            _ => p
                .parse::<ExpressionStatement>(recursion + 1)
                .map(Statement::ExpressionStatement),
        }
    }

    fn span(&self) -> Span {
        match self {
            Statement::ExpressionStatement(stmt) => stmt.span(),
        }
    }

    fn display(&self, global: &Global) -> String {
        match self {
            Statement::ExpressionStatement(stmt) => stmt.display(global),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExpressionStatement {
    pub inner: Expression,
    pub span: Span,
}

fn needs_semi(kind: &ExpressionKind) -> bool {
    match kind {
        ExpressionKind::Grouping(_)
        | ExpressionKind::Add(_, _)
        | ExpressionKind::Sub(_, _)
        | ExpressionKind::Mul(_, _)
        | ExpressionKind::Div(_, _)
        | ExpressionKind::Neg(_)
        | ExpressionKind::Call(_, _)
        | ExpressionKind::Index(_, _)
        | ExpressionKind::Member(_, _)
        | ExpressionKind::Equal(_, _)
        | ExpressionKind::NotEqual(_, _)
        | ExpressionKind::Gt(_, _)
        | ExpressionKind::Lt(_, _)
        | ExpressionKind::GtEq(_, _)
        | ExpressionKind::LtEq(_, _)
        | ExpressionKind::Binding(_)
        | ExpressionKind::IntLit(_, _)
        | ExpressionKind::FloatLit(_) => true,
    }
}

impl ExpressionStatement {
    fn new(inner: Expression, span: Span) -> Self {
        ExpressionStatement { inner, span }
    }
}

impl AstNode for ExpressionStatement {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = tackc_lexer::Token> + Clone,
    {
        let expr = p.parse::<Expression>(recursion + 1)?;
        if needs_semi(&expr.kind) {
            let semi = p.expect_token_kind(Some("';'"), token_kind!(TokenKind::Semicolon))?;
            let span = Span::new_from(expr.span.start, semi.span.end);

            Ok(ExpressionStatement::new(expr, span))
        } else {
            let span = expr.span;
            Ok(ExpressionStatement::new(expr, span))
        }
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        self.inner.display(global) + ";"
    }
}

#[test]
#[cfg(feature = "serde")]
fn stmt_test_glob() {
    use insta::glob;

    glob!("stmt-parse/*.tck", run_stmt_test);
}

#[cfg(all(test, feature = "serde"))]
use std::path::Path;

#[cfg(all(test, feature = "serde"))]
fn run_stmt_test(path: &Path) {
    use tackc_error::iter::IteratorExt;
    use tackc_file::OwnedFile;
    use tackc_lexer::Lexer;

    use crate::error::DiagResult;

    let global = Global::create_heap();
    let src = OwnedFile::try_from(path.to_path_buf())
        .unwrap_or_else(|_| panic!("Failed to open file {}", path.display()));
    let lexer = Lexer::new(&src, &global).consume_reporter(drop);
    let mut p = Parser::new(lexer);
    let expr = Statement::parse(&mut p, 0).expected("expression");
    insta::assert_ron_snapshot!(expr);
}
