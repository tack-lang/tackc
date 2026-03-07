//! A module for parsing errors.

use std::{borrow::Cow, result::Result as StdResult};

use crate::error::Diag;
use crate::file::{File, FileId};
use crate::global::Global;
use crate::lexer::Token;
use crate::span::Span;
use crate::utils::UnwrapExt;

use serde::{Deserialize, Serialize};

/// A result alias that uses [`ParseError`] for its error type.
pub type Result<T, E = ParseError> = StdResult<T, E>;

/// Errors that may be produced during parsing.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ParseError {
    /// An error for when something is expected, but a different token is found.
    Expected(Option<Cow<'static, str>>, Token),
    /// An error for when something is expected, but the EOF is found.
    Eof(Option<Cow<'static, str>>, FileId),
    /// An error for when the error limit is reached.
    ErrorLimit,
    /// An error for when the recursion limit is reached.
    RecursionLimit,
    /// A generic error for when the parser needs to quickly exit.
    ///
    /// This error should not be [`display`](Self::display)ed.
    Failure,
    /// An error for when the path components limit is reached.
    PathComponentsLimit,
    /// An error for when the node ID limit is reached.
    NodeIdLimit,
    /// An error for anything else.
    Other(Cow<'static, str>, Vec<Span>),
}

impl ParseError {
    /// Sets the 'expected' value of this error, if it's an [`Expected`](Self::Expected).
    pub fn set_expected(&mut self, new: &'static str) {
        match self {
            Self::Expected(expected, _) | Self::Eof(expected, _) => {
                expected.get_or_insert(new.into());
            }
            _ => {}
        }
    }

    /// Creates an [`Expected`](Self::Expected) error.
    pub fn expected(expected: Option<&'static str>, found: Token) -> Self {
        Self::Expected(expected.map(Into::into), found)
    }

    /// Creates an [`Eof`](Self::Eof) error.
    pub fn eof(expected: Option<&'static str>, file: &File) -> Self {
        Self::Eof(expected.map(Into::into), file.id())
    }

    /// Creates an [`Other`](Self::Other) error.
    pub fn other<S: Into<Cow<'static, str>>, Sp: Into<Span>, I: IntoIterator<Item = Sp>>(
        msg: S,
        tok: I,
    ) -> Self {
        Self::Other(msg.into(), tok.into_iter().map(Into::into).collect())
    }

    /// Creates an [`ErrorLimit`](Self::ErrorLimit) error.
    pub const fn error_limit() -> Self {
        Self::ErrorLimit
    }

    /// Creates a [`RecursionLimit`](Self::RecursionLimit) error.
    pub const fn recursion_limit() -> Self {
        Self::RecursionLimit
    }

    /// Creates a [`Failure`](Self::Failure) error.
    pub const fn failed() -> Self {
        Self::Failure
    }

    /// Creates a [`PathComponentsLimit`](Self::PathComponentsLimit) error.
    pub const fn path_components_limit() -> Self {
        Self::PathComponentsLimit
    }

    /// Creates a [`NodeIdLimit`](Self::NodeIdLimit) error.
    pub const fn node_id_limit() -> Self {
        Self::NodeIdLimit
    }

    /// Displays the given error as a string, using a file list and global.
    ///
    /// # Panics
    /// This function panics if the file used to produce this error is not in `global`'s file list.
    pub fn display(&self, global: &Global) -> String {
        match self {
            Self::Expected(expected, tok) => {
                let expected = expected.as_ref().map_or("<ERROR>", |v| v);
                Diag::with_span(
                    format!("expected {expected}, found '{}'", tok.display(global)),
                    tok.span,
                )
                .display(global)
            }
            Self::Eof(expected, file_id) => {
                assert!(global.file_list().contains_key(file_id), "file not found!");

                let expected = expected.as_ref().map_or("<ERROR>", |v| v);
                Diag::with_span(
                    format!("unexpected EOF, expected {expected}"),
                    Span::eof(
                        // We already asserted that `global.file_list()` contains `file_id` as a key.
                        global.file_list().get(file_id).expect_unreachable(), // CHECKED(Chloe)
                    ),
                )
                .display(global)
            }
            Self::Other(msg, spans) => {
                Diag::with_spans(msg.to_string(), spans.clone()).display(global)
            }
            Self::ErrorLimit => String::from("error limit reached. What are you doing?"),
            Self::RecursionLimit => String::from("recursion limit reached. What are you doing?"),
            Self::PathComponentsLimit => {
                String::from("path components limit reached. What are you doing?")
            }
            Self::NodeIdLimit => String::from("node id limit reached. What are you doing?"),
            Self::Failure => {
                String::from("generic failure. Shouldn't be displayed under normal circumstances.")
            }
        }
    }
}

/// An extension trait for [`Result<T, ParseError>`](std::result::Result).
pub trait ErrorExt {
    /// Calls [`set_expected`](ParseError::set_expected) on the inner value, if it's an [`Err`].
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
