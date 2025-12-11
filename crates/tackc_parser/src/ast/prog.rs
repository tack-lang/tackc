use tackc_global::Global;
use tackc_lexer::{Token, TokenKind};
use tackc_span::Span;

use serde::{Deserialize, Serialize};

use crate::{
    Parser,
    ast::{AstNode, Item, NodeId, Path},
    error::{DiagResult, ParseErrors, Result},
};

fn sync_item<I>(p: &mut Parser<I>)
where
    I: Iterator<Item = Token> + Clone,
{
    p.consume(token_kind!(TokenKind::Func | TokenKind::Const)); // Don't stop on first item

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

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Program {
    pub mod_stmt: ModStatement,
    pub items: Vec<Item>,
}

impl Program {
    /// This function will take an input of tokens, and parse a program from it.
    /// The inputted iterator should be easily clonable.
    ///
    /// # Errors
    /// This function will return an error if it fails to parse a full program.
    #[allow(clippy::missing_panics_doc)]
    pub fn parse<I>(iter: I, global: &Global) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let mut p = Parser::new(iter, global);
        let mut errors: Option<ParseErrors> = None;

        let mod_stmt = p
            .try_parse::<ModStatement>(0)
            .expected("`mod`")
            .map_err(|e| {
                match &mut errors {
                    Some(errs) => {
                        errs.merge(e);
                    }
                    None => errors = Some(e),
                }
                sync_item(&mut p);
            })
            .ok();

        let mut items = Vec::new();

        while !p.is_eof() {
            match p.try_parse::<Item>(0).expected("item") {
                Ok(item) => items.push(item),
                Err(e) => {
                    match &mut errors {
                        Some(err) => {
                            err.merge(e);
                        }
                        None => errors = Some(e),
                    }
                    sync_item(&mut p);
                }
            }
        }

        if let Some(e) = errors {
            Err(e)
        } else {
            Ok(Program {
                mod_stmt: mod_stmt.expect("This is a bug. Please submit a bug report."),
                items,
            })
        }
    }

    pub fn display(&self, global: &Global) -> String {
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
}

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModStatement {
    pub span: Span,
    pub path: Path,
    pub id: NodeId,
}

impl AstNode for ModStatement {
    fn parse<I>(p: &mut Parser<I>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone,
    {
        let mod_tok = p.expect_token_kind(None, token_kind!(TokenKind::Mod))?;
        let path = p.parse::<Path>(recursion + 1)?;
        let semi = p.expect_token_kind(Some("';'"), token_kind!(TokenKind::Semicolon))?;
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
    let expr = Program::parse(lexer, &global);
    insta::assert_ron_snapshot!(expr);
}
