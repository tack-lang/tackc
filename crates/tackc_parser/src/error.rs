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
    Dead,
}

impl ParseError {
    pub fn set_expected(&mut self, new: &'static str) {
        match self {
            Self::Expected(expected, _) | Self::Eof(expected) => {
                expected.get_or_insert(new.into());
            }
            _ => {}
        }
    }

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

    pub const fn dead() -> Self {
        Self::Dead
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
            Self::Dead => {
                String::from("error limit reached. What are you doing?")
            }
        }
    }
}

pub trait ErrorExt {
    #[must_use]
    fn set_expected(self, expected: &'static str) -> Self;
}

impl<T> ErrorExt for Result<T> {
    fn set_expected(self, expected: &'static str) -> Self {
        self.map_err(|mut err| {
            err.set_expected(expected);
            err
        })
    }
}
