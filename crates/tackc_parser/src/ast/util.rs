use tackc_ast::{NodeId, Path};
use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, Visitor, VisitorMut},
    error::{DiagResult, Result},
};

impl AstNode for Path {
    fn parse<I, F: File>(p: &mut Parser<I, F>, _recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let first = p.identifier().clear_expected()?;
        let mut components = vec![first];

        while p.consume(kind!(TokenKind::Dot)).is_some() {
            let next = p.identifier()?;
            components.push(next);
        }

        Ok(Self {
            span: Span::new_from(
                first.span.start,
                components.last().unwrap_or(&first).span.end,
            ),
            id: p.node_id(),
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

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_path(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_path_mut(self);
    }
}
