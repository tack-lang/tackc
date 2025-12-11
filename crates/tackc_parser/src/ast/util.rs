use serde::{Deserialize, Serialize};
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::{
    Parser,
    ast::{AstNode, NodeId, Symbol, Visitor},
    error::{DiagResult, Result},
};

/// A path to an item
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Path {
    #[allow(missing_docs)]
    pub span: Span,
    /// The components of this path, seperated by `.`
    pub components: Vec<Symbol>,
    #[allow(missing_docs)]
    pub id: NodeId,
}

impl AstNode for Path {
    fn parse<I>(p: &mut Parser<I>, _recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let first = p.identifier().clear_expected()?;
        let mut components = vec![first];

        while p.consume(kind!(TokenKind::Dot)).is_some() {
            let next = p.identifier()?;
            components.push(next);
        }

        Ok(Path {
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
}
