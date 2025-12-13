use std::fmt::Write;

use tackc_ast::{Block, NodeId, StatementOrExpression};
use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, Visitor, VisitorMut},
    error::{DiagResult, ParseError, ParseErrors, Result},
};

fn sync_block<I, F: File>(p: &mut Parser<I, F>)
where
    I: Iterator<Item = Token> + Clone,
{
    let mut depth: u32 = 0;

    loop {
        let Some(tok) = p.peek_token() else { return };

        match tok.kind {
            TokenKind::LBrace => {
                depth += 1;
                p.next_token();
            }
            TokenKind::RBrace if depth == 0 => return,
            TokenKind::RBrace => {
                p.next_token();
                depth -= 1;
            }
            TokenKind::Semicolon if depth == 0 => {
                p.next_token();
                return;
            }
            _ => {
                p.next_token();
            }
        }
    }
}

impl AstNode for Block {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        p.check_recursion(recursion)?;

        let l_brace = p.expect_token_kind(None, kind!(TokenKind::LBrace))?;

        let mut errors: Option<ParseErrors> = None;

        let mut stmts = Vec::new();
        let expr = loop {
            if p.peek_is(kind!(TokenKind::RBrace)) {
                break None;
            }
            if p.is_eof() {
                return Err(ParseErrors::new(ParseError::eof(Some(
                    "expression, statement, or '}'",
                ))));
            }

            // Use try_parse to help with synchronization
            match p
                .try_parse::<StatementOrExpression>(recursion + 1)
                .expected("expression, statement, or '}'")
            {
                Ok(StatementOrExpression::Expression(expr)) => break Some(expr),
                Ok(StatementOrExpression::Statement(stmt)) => stmts.push(stmt),
                Err(e) => {
                    match &mut errors {
                        Some(err) => err.merge(e),
                        None => errors = Some(e),
                    }

                    sync_block(p);
                }
            }
        };

        let r_brace = p.expect_token_kind(Some("'}'"), kind!(TokenKind::RBrace))?;

        if let Some(e) = errors {
            return Err(e);
        }

        Ok(Block {
            span: Span::new_from(l_brace.span.start, r_brace.span.end),
            stmts,
            expr,
            id: p.node_id(),
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

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_block(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_block_mut(self);
    }
}
