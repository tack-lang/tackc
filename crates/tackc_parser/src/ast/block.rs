use std::fmt::Write;

use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, Expression, Statement, StatementOrExpression},
    error::{DiagResult, ParseError, ParseErrors, Result},
};

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub expr: Option<Expression>,
    pub span: Span,
}

impl AstNode for Block {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        p.check_recursion(recursion)?;

        let l_brace = p.expect_token_kind(None, token_kind!(TokenKind::LBrace))?;

        let mut stmts = Vec::new();
        let expr = loop {
            if p.peek_is(token_kind!(TokenKind::RBrace)) {
                break None;
            }
            if p.is_eof() {
                return Err(ParseErrors::new(ParseError::eof(Some(
                    "expression, statement, or '}'",
                ))));
            }

            match p.parse::<StatementOrExpression>(recursion + 1).expected("expression, statement, or '}'") {
                Ok(StatementOrExpression::Expression(expr)) => break Some(expr),
                Ok(StatementOrExpression::Statement(stmt)) => stmts.push(stmt),
                Err(e) => {
                    return Err(e);
                }
            }
        };

        let r_brace = p.expect_token_kind(Some("'}'"), token_kind!(TokenKind::RBrace))?;

        Ok(Block {
            span: Span::new_from(l_brace.span.start, r_brace.span.end),
            stmts,
            expr,
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        let mut str = String::from("{");
        for stmt in &self.stmts {
            _ = write!(str, " {}", stmt.display(global));
        }
        if let Some(expr) = &self.expr {
            _ = write!(str, " {}", expr.display(global));
        }
        str.push_str(" }");
        str
    }
}
