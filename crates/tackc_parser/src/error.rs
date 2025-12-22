use std::{borrow::Cow, result::Result as StdResult};

use serde::{Deserialize, Serialize};
use tackc_error::Diag;
use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::Token;
use tackc_span::Span;

pub type Result<T, E = ParseError> = StdResult<T, E>;

#[derive(Debug, Serialize, Deserialize)]
pub enum ParseError {
    Expected(Option<Cow<'static, str>>, Token),
    Eof(Option<Cow<'static, str>>),
}

impl ParseError {
    pub fn expected(expected: Option<&'static str>, found: Token) -> Self {
        Self::Expected(expected.map(Into::into), found)
    }

    pub fn eof(expected: Option<&'static str>) -> Self {
        Self::Eof(expected.map(Into::into))
    }

    pub fn display<F: File>(&self, file: &F, global: &Global) -> String {
        match self {
            Self::Expected(expected, tok) => Diag::with_span(
                format!(
                    "expected {}, found '{}'",
                    expected.as_ref().map_or("<ERROR>", |v| v),
                    tok.display(global)
                ),
                tok.span,
            )
            .display(file),
            Self::Eof(expected) => Diag::with_span(
                format!(
                    "unexpected EOF, expected {}",
                    expected.as_ref().map_or("<ERROR>", |v| v)
                ),
                Span::eof(file),
            )
            .display(file),
        }
    }
}
