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
    Other(Cow<'static, str>, Vec<Span>),
}

impl ParseError {
    pub fn expected(expected: Option<&'static str>, found: Token) -> Self {
        Self::Expected(expected.map(Into::into), found)
    }

    pub fn eof(expected: Option<&'static str>) -> Self {
        Self::Eof(expected.map(Into::into))
    }

    pub fn other<S: Into<Cow<'static, str>>, Sp: Into<Span>, I: IntoIterator<Item = Sp>>(
        msg: S,
        tok: I,
    ) -> Self {
        Self::Other(msg.into(), tok.into_iter().map(Into::into).collect())
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
            Self::Other(msg, spans) => {
                Diag::with_spans(msg.to_string(), spans.clone()).display(file)
            }
        }
    }
}
