use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{
    Parser,
    ast::{AstNode, Expression, Item, NodeId, Symbol, Visitor},
    error::{DiagResult, Result},
};

#[allow(missing_docs)]
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
                if let Some(tok) = p.consume(kind!(TokenKind::Semicolon)) {
                    let stmt = ExpressionStatement {
                        span: Span::new_from(expr.span.start, tok.span.end),
                        inner: expr,
                        id: p.node_id(),
                    };
                    Ok(StatementOrExpression::Statement(
                        Statement::ExpressionStatement(stmt),
                    ))
                } else if let Some(_tok) = p.consume(kind!(TokenKind::Eq)) {
                    let rvalue = p.parse::<Expression>(recursion + 1)?;
                    let semi = p.expect_token_kind(Some("';'"), kind!(TokenKind::Semicolon))?;
                    Ok(StatementOrExpression::Statement(
                        Statement::AssignmentStatement(AssignmentStatement {
                            span: Span::new_from(expr.span.start, semi.span.end),
                            lvalue: expr,
                            rvalue,
                            id: p.node_id(),
                        }),
                    ))
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

    fn id(&self) -> super::NodeId {
        match self {
            StatementOrExpression::Expression(expr) => expr.id,
            StatementOrExpression::Statement(stmt) => stmt.id(),
        }
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        match self {
            StatementOrExpression::Expression(expr) => expr.accept(v),
            StatementOrExpression::Statement(stmt) => stmt.accept(v),
        }
    }
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    LetStatement(LetStatement),
    AssignmentStatement(AssignmentStatement),
    Item(Item),
}

impl Statement {
    #[allow(missing_docs)]
    pub fn span(&self) -> Span {
        match self {
            Statement::ExpressionStatement(stmt) => stmt.span(),
            Statement::LetStatement(stmt) => stmt.span(),
            Statement::Item(item) => item.span(),
            Statement::AssignmentStatement(stmt) => stmt.span(),
        }
    }

    #[allow(missing_docs)]
    pub fn display(&self, global: &Global) -> String {
        match self {
            Statement::ExpressionStatement(stmt) => stmt.display(global),
            Statement::LetStatement(stmt) => stmt.display(global),
            Statement::Item(item) => item.display(global),
            Statement::AssignmentStatement(stmt) => stmt.display(global),
        }
    }

    #[allow(missing_docs)]
    pub fn id(&self) -> NodeId {
        match self {
            Statement::ExpressionStatement(stmt) => stmt.id,
            Statement::LetStatement(stmt) => stmt.id,
            Statement::AssignmentStatement(stmt) => stmt.id,
            Statement::Item(stmt) => stmt.id(),
        }
    }

    #[allow(missing_docs)]
    pub fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        match self {
            Statement::ExpressionStatement(stmt) => stmt.accept(v),
            Statement::LetStatement(stmt) => stmt.accept(v),
            Statement::AssignmentStatement(stmt) => stmt.accept(v),
            Statement::Item(item) => item.accept(v),
        }
    }
}

/// An expression ending with a semicolon.
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExpressionStatement {
    #[allow(missing_docs)]
    pub span: Span,
    /// The inner expression for this expression statement
    pub inner: Expression,
    #[allow(missing_docs)]
    pub id: NodeId,
}

impl ExpressionStatement {
    #[allow(missing_docs)]
    pub fn span(&self) -> Span {
        self.span
    }

    #[allow(missing_docs)]
    pub fn display(&self, global: &Global) -> String {
        self.inner.display(global) + ";"
    }

    #[allow(missing_docs)]
    pub fn id(&self) -> NodeId {
        self.id
    }

    #[allow(missing_docs)]
    pub fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_expression_statement(self);
    }
}

/// A let statement, e.g. `let x = 5;`
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LetStatement {
    #[allow(missing_docs)]
    pub span: Span,
    /// The symbol being defined
    pub ident: Symbol,
    /// The type annotation given
    pub ty: Option<Expression>,
    /// The initial value given
    pub expr: Option<Expression>,
    #[allow(missing_docs)]
    pub id: NodeId,
}

impl AstNode for LetStatement {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = tackc_lexer::Token> + Clone,
    {
        let let_tok = p.expect_token_kind(None, kind!(TokenKind::Let))?;
        let ident = p.identifier()?;
        let ty = if p.consume(kind!(TokenKind::Colon)).is_some() {
            Some(p.parse::<Expression>(recursion + 1).expected("type")?)
        } else {
            None
        };

        let expr = if p.consume(kind!(TokenKind::Eq)).is_some() {
            Some(
                p.parse::<Expression>(recursion + 1)
                    .expected("expression")?,
            )
        } else {
            None
        };

        let semi = p.expect_token_kind(Some("';'"), kind!(TokenKind::Semicolon))?;

        Ok(LetStatement {
            span: Span::new_from(let_tok.span.start, semi.span.end),
            ident,
            ty,
            expr,
            id: p.node_id(),
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

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_let_statement(self);
    }
}

/// An assignment statement
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AssignmentStatement {
    #[allow(missing_docs)]
    pub span: Span,
    /// The value on the left of the `=`
    pub lvalue: Expression,
    /// The value on the right of the `=`
    pub rvalue: Expression,
    #[allow(missing_docs)]
    pub id: NodeId,
}

impl AssignmentStatement {
    #[allow(missing_docs)]
    pub fn span(&self) -> Span {
        self.span
    }

    #[allow(missing_docs)]
    pub fn display(&self, global: &Global) -> String {
        format!(
            "{} = {};",
            self.lvalue.display(global),
            self.rvalue.display(global)
        )
    }

    #[allow(missing_docs)]
    pub fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_assignment_statement(self);
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
    let mut p = Parser::new(lexer, &global);
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
