use std::fmt::Debug;
use std::hash::Hash;

use crate::error::{ParseErrors, Result};
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use crate::Parser;

#[macro_export]
macro_rules! token_kind {
    ($pat:pat) => {
        |kind| matches!(kind, $pat)
    };
}

#[cfg(feature = "serde")]
pub trait Serde: serde::Serialize + for<'a> serde::Deserialize<'a> {}
#[cfg(feature = "serde")]
impl<T: serde::Serialize + for<'a> serde::Deserialize<'a>> Serde for T {}
#[cfg(not(feature = "serde"))]
pub trait Serde {}
#[cfg(not(feature = "serde"))]
impl<T> Serde for T {}

pub trait AstNode: Debug + PartialEq + Eq + Hash + Clone + Sized + Serde {
    /// Parse the AST node using the given parser.
    ///
    /// # Errors
    /// If the node cannot be parsed from the given parser, this function will return an error.
    /// The parser may be in any state; do not trust it.
    /// If you need to, the parser can be cloned previous to calling this function.
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone;
    fn span(&self) -> Span;
    fn display(&self, global: &Global) -> String;
}

mod expr;
pub use expr::*;

mod decl;
pub use decl::*;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Program {
    pub decls: Vec<Declaration>,
}

impl AstNode for Program {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let mut decls = Vec::new();
        let mut diags: Option<ParseErrors> = None;

        while !p.is_eof() {
            let decl = p.parse::<Declaration>(recursion + 1);
            match decl {
                Ok(decl) => decls.push(decl),
                Err(mut decl_diags) => {
                    decl_diags.expected("declaration");
                    match diags.as_mut() {
                        Some(diags) => {
                            diags.merge(decl_diags);
                        }
                        None => {
                            diags = Some(decl_diags);
                        }
                    }

                    // Synchronize parser by consuming tokens until reaching declaration or statement start.
                    while let Some(tok) = p.peek_token() {
                        match tok.kind {
                            TokenKind::Const | TokenKind::Func => break,
                            TokenKind::Semicolon => {
                                p.next_token();
                                break;
                            }
                            _ => {
                                p.next_token();
                            }
                        }
                    }
                }
            }
        }

        if let Some(diags) = diags {
            Err(diags)
        } else {
            Ok(Program { decls })
        }
    }

    fn span(&self) -> Span {
        self.decls
            .first()
            .and_then(|first| {
                let last = self.decls.last()?;
                Some(Span::new_from(first.span().start, last.span().end))
            })
            .unwrap_or_default()
    }

    fn display(&self, global: &Global) -> String {
        self.decls
            .iter()
            .map(|decl| decl.display(global))
            .collect::<Vec<_>>()
            .join("\n")
    }
}
