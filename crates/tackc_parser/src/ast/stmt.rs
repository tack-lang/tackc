use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{
    Parser,
    ast::{AstNode, Expression, Item, Symbol},
    error::{DiagResult, Result},
};

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum StatementOrExpression {
    Expression(Expression),
    Statement(Statement),
}

impl AstNode for StatementOrExpression {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let tok = p.expect_peek_token(None)?;
        #[allow(clippy::single_match_else)]
        match tok.kind {
            TokenKind::Const => p
                .parse::<Item>(recursion + 1)
                .map(Statement::Item)
                .map(StatementOrExpression::Statement),
            TokenKind::Let => p
                .parse::<LetStatement>(recursion + 1)
                .map(Statement::LetStatement)
                .map(StatementOrExpression::Statement),
            _ => {
                let expr = p.parse::<Expression>(recursion + 1)?;
                if let Some(tok) = p.consume(token_kind!(TokenKind::Semicolon)) {
                    let stmt = ExpressionStatement {
                        span: Span::new_from(expr.span.start, tok.span.end),
                        inner: expr,
                    };
                    Ok(StatementOrExpression::Statement(
                        Statement::ExpressionStatement(stmt),
                    ))
                } else if let Some(_tok) = p.consume(token_kind!(TokenKind::Eq)) {
                    let rvalue = p.parse::<Expression>(recursion + 1)?;
                    let semi =
                        p.expect_token_kind(Some("';'"), token_kind!(TokenKind::Semicolon))?;
                    Ok(StatementOrExpression::Statement(Statement::Assignment(
                        Assignment {
                            span: Span::new_from(expr.span.start, semi.span.end),
                            lvalue: expr,
                            rvalue,
                        },
                    )))
                } else {
                    Ok(StatementOrExpression::Expression(expr))
                }
            }
        }
    }

    fn span(&self) -> Span {
        match self {
            StatementOrExpression::Expression(expr) => expr.span,
            StatementOrExpression::Statement(stmt) => stmt.span(),
        }
    }

    fn display(&self, global: &Global) -> String {
        match self {
            StatementOrExpression::Expression(expr) => expr.display(global),
            StatementOrExpression::Statement(stmt) => stmt.display(global),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    LetStatement(LetStatement),
    Assignment(Assignment),
    Item(Item),
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::ExpressionStatement(stmt) => stmt.span(),
            Statement::LetStatement(stmt) => stmt.span(),
            Statement::Item(item) => item.span(),
            Statement::Assignment(stmt) => stmt.span(),
        }
    }

    pub fn display(&self, global: &Global) -> String {
        match self {
            Statement::ExpressionStatement(stmt) => stmt.display(global),
            Statement::LetStatement(stmt) => stmt.display(global),
            Statement::Item(item) => item.display(global),
            Statement::Assignment(stmt) => stmt.display(global),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExpressionStatement {
    pub span: Span,
    pub inner: Expression,
}

impl ExpressionStatement {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn display(&self, global: &Global) -> String {
        self.inner.display(global) + ";"
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LetStatement {
    pub span: Span,
    pub ident: Symbol,
    pub ty: Option<Expression>,
    pub expr: Option<Expression>,
}

impl AstNode for LetStatement {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = tackc_lexer::Token> + Clone,
    {
        let let_tok = p.expect_token_kind(None, token_kind!(TokenKind::Let))?;
        let ident = p.identifier()?;
        let ty = if p.consume(token_kind!(TokenKind::Colon)).is_some() {
            Some(p.parse::<Expression>(recursion + 1).expected("type")?)
        } else {
            None
        };

        let expr = if p.consume(token_kind!(TokenKind::Eq)).is_some() {
            Some(
                p.parse::<Expression>(recursion + 1)
                    .expected("expression")?,
            )
        } else {
            None
        };

        let semi = p.expect_token_kind(Some("';'"), token_kind!(TokenKind::Semicolon))?;

        Ok(LetStatement {
            span: Span::new_from(let_tok.span.start, semi.span.end),
            ident,
            ty,
            expr,
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        format!(
            "let {}{}{};",
            self.ident.display(global),
            self.ty
                .as_ref()
                .map(|ty| format!(": {}", ty.display(global)))
                .unwrap_or_default(),
            self.expr
                .as_ref()
                .map(|expr| format!(" = {}", expr.display(global)))
                .unwrap_or_default()
        )
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Assignment {
    pub span: Span,
    pub lvalue: Expression,
    pub rvalue: Expression,
}

impl Assignment {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn display(&self, global: &Global) -> String {
        format!(
            "{} = {};",
            self.lvalue.display(global),
            self.rvalue.display(global)
        )
    }
}

#[test]
fn stmt_test_glob() {
    use insta::glob;

    glob!("stmt-parse/*.tck", run_stmt_test);
}

#[cfg(test)]
use std::path::Path;

#[cfg(test)]
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
    let expr = StatementOrExpression::parse(&mut p, 0)
        .expected("statement")
        .map(|stmt_or_expr| {
            if let StatementOrExpression::Statement(stmt) = stmt_or_expr {
                stmt
            } else {
                panic!("expected statement, found expression");
            }
        });
    insta::assert_ron_snapshot!(expr);
}
