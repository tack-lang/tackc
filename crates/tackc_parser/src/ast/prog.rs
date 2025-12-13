use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{
    Parser,
    ast::{AstNode, Item, NodeId, Path, Visitor, VisitorMut},
    error::{DiagResult, ParseErrors, Result},
};

fn sync_item<I, F: File>(p: &mut Parser<I, F>)
where
    I: Iterator<Item = Token> + Clone,
{
    p.consume(kind!(TokenKind::Func | TokenKind::Const)); // Don't stop on first item

    let mut depth: u32 = 0;

    loop {
        let Some(tok) = p.peek_token() else { return };

        match tok.kind {
            TokenKind::LBrace => {
                p.next_token();
                depth += 1;
            }
            TokenKind::RBrace => {
                p.next_token();
                depth = depth.saturating_sub(1);
            }
            TokenKind::Func | TokenKind::Const if depth == 0 => {
                return;
            }
            _ => {
                p.next_token();
            }
        }
    }
}

/// A full program
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Program {
    #[allow(missing_docs)]
    pub span: Span,
    /// The initial mod statement for this program.
    pub mod_stmt: ModStatement,
    /// The items of this program
    pub items: Vec<Item>,
    #[allow(missing_docs)]
    pub id: NodeId,
}

impl Program {
    /// This function will take an input of tokens, and parse a program from it.
    /// The inputted iterator should be easily clonable.
    ///
    /// # Errors
    /// This function will return an error if it fails to parse a full program.
    pub fn parse_file<I, F: File>(iter: I, global: &Global, file: &F) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let mut p = Parser::new(iter, global, file);
        p.parse(0)
    }
}

impl AstNode for Program {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let mut errors: Option<ParseErrors> = None;

        let mod_stmt = p
            .try_parse::<ModStatement>(recursion)
            .expected("`mod`")
            .map_err(|e| {
                match &mut errors {
                    Some(errs) => {
                        errs.merge(e);
                    }
                    None => errors = Some(e),
                }
                sync_item(p);
            })
            .ok();

        let mut items = Vec::new();

        while !p.is_eof() {
            match p.try_parse::<Item>(recursion).expected("item") {
                Ok(item) => items.push(item),
                Err(e) => {
                    match &mut errors {
                        Some(err) => {
                            err.merge(e);
                        }
                        None => errors = Some(e),
                    }
                    sync_item(p);
                }
            }
        }

        if let Some(e) = errors {
            Err(e)
        } else {
            Ok(Program {
                span: Span::full(p.file),
                mod_stmt: mod_stmt.expect("This is a bug. Please submit a bug report."),
                items,
                id: p.node_id(),
            })
        }
    }

    fn span(&self) -> Span {
        self.span
    }

    /// Get the string representation of this program
    fn display(&self, global: &Global) -> String {
        format!(
            "{}\n{}",
            self.mod_stmt.display(global),
            self.items
                .iter()
                .map(|item| item.display(global))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_program(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_program_mut(self);
    }
}

/// The mod statement of a program
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModStatement {
    #[allow(missing_docs)]
    pub span: Span,
    /// The path of this mod statement
    pub path: Path,
    #[allow(missing_docs)]
    pub id: NodeId,
}

impl AstNode for ModStatement {
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let mod_tok = p.expect_token_kind(None, kind!(TokenKind::Mod))?;
        let path = p.parse::<Path>(recursion + 1)?;
        let semi = p.expect_token_kind(Some("';'"), kind!(TokenKind::Semicolon))?;
        Ok(ModStatement {
            span: Span::new_from(mod_tok.span.start, semi.span.end),
            path,
            id: p.node_id(),
        })
    }

    fn span(&self) -> Span {
        self.span
    }

    fn display(&self, global: &Global) -> String {
        format!("mod {};", self.path.display(global))
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V) {
        v.visit_mod_statement(self);
    }

    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V) {
        v.visit_mod_statement_mut(self);
    }
}

#[test]
fn prog_test_glob() {
    use insta::glob;

    glob!("prog-parse/*.tck", run_prog_test);
}

#[cfg(test)]
use std::path::Path as StdPath;

#[cfg(test)]
fn run_prog_test(path: &StdPath) {
    use tackc_error::iter::IteratorExt;
    use tackc_file::OwnedFile;
    use tackc_lexer::Lexer;

    let global = Global::create_heap();
    let src = OwnedFile::try_from(path.to_path_buf())
        .unwrap_or_else(|_| panic!("Failed to open file {}", path.display()));
    let lexer = Lexer::new(&src, &global).consume_reporter(drop);
    let expr = Program::parse_file(lexer, &global, &src);
    insta::assert_ron_snapshot!(expr);
}
