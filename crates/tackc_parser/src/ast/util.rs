use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, Symbol},
    error::{DiagResult, Result},
};

#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Path {
    pub span: Span,
    pub components: Vec<Symbol>,
}

impl AstNode for Path {
    fn parse<I>(p: &mut Parser<I>, _recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let first = p.identifier().clear_expected()?;
        let mut components = vec![first];

        while p.consume(token_kind!(TokenKind::Dot)).is_some()
        {
            let next = p.identifier()?;
            components.push(next);
        }

        Ok(Path {
            span: Span::new_from(
                first.span.start,
                components.last().unwrap_or(&first).span.end,
            ),
            components,
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        self.components
            .iter()
            .map(|ident| ident.display(global))
            .collect::<Vec<_>>()
            .join(".")
    }
}
