use tackc_ast::{
    AssignmentStatement, Expression, ExpressionStatement, Item, LetStatement, NodeId, Statement,
    StatementOrExpression,
};
use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, Visitor, VisitorMut},
    error::{DiagResult, Result},
};

impl AstNode for StatementOrExpression {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let tok = p.expect_peek_token(None)?;
        match tok.kind {
            TokenKind::Const => p
                .parse::<Item>(recursion + 1)
                .map(Box::new)
                .map(Statement::Item)
                //.map(Box::new)
                .map(StatementOrExpression::Statement),
            TokenKind::Let => p
                .parse::<LetStatement>(recursion + 1)
                .map(Box::new)
                .map(Statement::LetStatement)
                //.map(Box::new)
                .map(StatementOrExpression::Statement),
            _ => {
                let expr = p.parse::<Expression>(recursion + 1)?;
                if let Some(tok) = p.consume(kind!(TokenKind::Semicolon)) {
                    let stmt = ExpressionStatement {
                        span: Span::new_from(expr.span.start, tok.span.end),
                        inner: expr,
                        id: p.node_id(),
                    };
                    Ok(Self::Statement(Statement::ExpressionStatement(
                        Box::new(stmt),
                    )))
                } else if let Some(_tok) = p.consume(kind!(TokenKind::Eq)) {
                    let rvalue = p.parse::<Expression>(recursion + 1)?;
                    let semi = p.expect_token_kind(Some("';'"), kind!(TokenKind::Semicolon))?;
                    Ok(Self::Statement(Statement::AssignmentStatement(
                        Box::new(AssignmentStatement {
                            span: Span::new_from(expr.span.start, semi.span.end),
                            lvalue: expr,
                            rvalue,
                            id: p.node_id(),
                        }),
                    )))
                } else {
                    Ok(Self::Expression(expr))
                }
            }
        }
    }

    fn span(&self) -> Span {
        match self {
            Self::Expression(expr) => expr.span,
            Self::Statement(stmt) => stmt.span(),
        }
    }

    fn display(&self, global: &Global) -> String {
        match self {
            Self::Expression(expr) => expr.display(global),
            Self::Statement(stmt) => stmt.display(global),
        }
    }

    fn id(&self) -> NodeId {
        match self {
            Self::Expression(expr) => expr.id,
            Self::Statement(stmt) => stmt.id(),
        }
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        match self {
            Self::Expression(expr) => expr.accept(v),
            Self::Statement(stmt) => stmt.accept(v),
        }
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        match self {
            Self::Expression(expr) => expr.accept_mut(v),
            Self::Statement(stmt) => stmt.accept_mut(v),
        }
    }
}

impl AstNode for Statement {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        _ = p;
        _ = recursion;
        unimplemented!()
    }

    fn span(&self) -> Span {
        match self {
            Self::ExpressionStatement(stmt) => stmt.span(),
            Self::LetStatement(stmt) => stmt.span(),
            Self::Item(item) => item.span(),
            Self::AssignmentStatement(stmt) => stmt.span(),
        }
    }

    fn display(&self, global: &Global) -> String {
        match self {
            Self::ExpressionStatement(stmt) => stmt.display(global),
            Self::LetStatement(stmt) => stmt.display(global),
            Self::Item(item) => item.display(global),
            Self::AssignmentStatement(stmt) => stmt.display(global),
        }
    }

    fn id(&self) -> NodeId {
        match self {
            Self::ExpressionStatement(stmt) => stmt.id,
            Self::LetStatement(stmt) => stmt.id,
            Self::AssignmentStatement(stmt) => stmt.id,
            Self::Item(stmt) => stmt.id(),
        }
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        match self {
            Self::ExpressionStatement(stmt) => stmt.accept(v),
            Self::LetStatement(stmt) => stmt.accept(v),
            Self::AssignmentStatement(stmt) => stmt.accept(v),
            Self::Item(item) => item.accept(v),
        }
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        match self {
            Self::ExpressionStatement(stmt) => stmt.accept_mut(v),
            Self::LetStatement(stmt) => stmt.accept_mut(v),
            Self::AssignmentStatement(stmt) => stmt.accept_mut(v),
            Self::Item(item) => item.accept_mut(v),
        }
    }
}

impl AstNode for ExpressionStatement {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        _ = p;
        _ = recursion;
        unimplemented!()
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        self.inner.display(global) + ";"
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_expression_statement(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_expression_statement_mut(self);
    }
}

impl AstNode for LetStatement {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = tackc_lexer::Token> + Clone,
    {
        let let_tok = p.expect_token_kind(None, kind!(TokenKind::Let))?;
        let ident = p.identifier()?;
        let ty = if p.consume(kind!(TokenKind::Colon)).is_some() {
            let ty = p.parse::<Expression>(recursion + 1).expected("type")?;
            Some(ty)
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

        Ok(Self {
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

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_let_statement_mut(self);
    }
}

impl AstNode for AssignmentStatement {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        _ = recursion;
        _ = p;
        unimplemented!()
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        format!(
            "{} = {};",
            self.lvalue.display(global),
            self.rvalue.display(global)
        )
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_assignment_statement(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_assignment_statement_mut(self);
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
    use tackc_file::BasicFile;
    use tackc_lexer::Lexer;

    use crate::error::DiagResult;

    let global = Global::create_heap();
    let src = std::fs::read_to_string(path).unwrap();
    let path = Path::new(path.file_name().unwrap());
    let file = BasicFile::new(src, path);

    let lexer = Lexer::new(&file, &global).consume_reporter(drop);
    let mut p = Parser::new(lexer, &global, &file);
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
