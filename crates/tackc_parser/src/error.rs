use std::borrow::Cow;
use std::fmt::Write;
use std::ops::Deref;
use std::result::Result as StdResult;

/// Result type using [`ParseErrors`] as the error type
pub type Result<T, E = ParseErrors> = StdResult<T, E>;

use serde::{Deserialize, Serialize};

use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::Token;
use tackc_lexer::TokenKind;
use tackc_span::Span;

/// A list of [`ParseError`]s. This is easily cloneable, since it uses a Clone-on-Write vector.
/// This struct should always contain at least one error.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParseErrors {
    // Length of errors will always be 1 or greater
    errors: Vec<ParseError>,
}

impl ParseErrors {
    /// Create a new [`ParseErrors`]
    pub fn new(error: ParseError) -> Self {
        ParseErrors {
            errors: Vec::from([error]),
        }
    }

    /// Add a new [`ParseError`] to the list
    pub fn push(&mut self, diag: ParseError) {
        self.errors.push(diag);
    }

    /*
    /// Returns a reference to the most recent [`ParseError`]
    fn most_recent(&self) -> &ParseError {
        debug_assert!(!self.errors.is_empty());

        self.errors.last().unwrap()
    }*/

    /// Returns a mutable reference to the most recent [`ParseError`]
    fn most_recent_mut(&mut self) -> &mut ParseError {
        self.errors.last_mut().unwrap()
    }

    /// Clears the `expected` field of the most recent error.
    pub fn clear_expected(&mut self) {
        let last = self.most_recent_mut();
        match &mut last.kind {
            ParseErrorKind::ExpectedFound {
                expected,
                found: _,
                span: _,
            } => {
                *expected = None;
            }
            ParseErrorKind::Recursion => {}
        }
    }

    /// This function takes the most recent error from `self` and changes it's `expected` field to `Some(str)` if it's `None`.
    pub fn expected(&mut self, str: &'static str) {
        let last = self.most_recent_mut();

        match &mut last.kind {
            ParseErrorKind::ExpectedFound {
                expected,
                found: _,
                span: _,
            } => {
                if expected.is_none() {
                    *expected = Some(str.into());
                }
            }
            ParseErrorKind::Recursion => {}
        }
    }

    /// Return a string representation of the errors.
    #[allow(clippy::missing_panics_doc)]
    pub fn display<F: File>(&self, file: &F, global: &Global) -> String {
        debug_assert!(!self.is_empty());

        let mut str = String::new();
        let mut errors = self.errors.iter();
        _ = write!(str, "{}", errors.next().unwrap().display(file, global));

        for i in errors {
            _ = write!(str, "\n\n{}", i.display(file, global));
        }

        str
    }

    /// Add a new list of parse errors to `self`.
    pub fn merge(&mut self, other: ParseErrors) {
        self.errors.extend(other.errors);
    }
}

impl Deref for ParseErrors {
    type Target = [ParseError];

    fn deref(&self) -> &Self::Target {
        &self.errors
    }
}

/// A single parsing error
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParseError {
    /// The kind of error this is
    pub kind: ParseErrorKind,
}

impl ParseError {
    /// Create a new expected-found parse error
    #[must_use]
    pub fn new(expected: Option<&'static str>, found: Token) -> Self {
        ParseError {
            kind: ParseErrorKind::ExpectedFound {
                expected: expected.map(Into::into),
                found: found.kind,
                span: Some(found.span),
            },
        }
    }

    /// Create a new expected-found-eof parse error
    #[must_use]
    pub fn eof(expected: Option<&'static str>) -> Self {
        ParseError {
            kind: ParseErrorKind::ExpectedFound {
                expected: expected.map(Into::into),
                found: TokenKind::Eof,
                span: None,
            },
        }
    }

    /// Create a new recursion parse error
    #[must_use]
    pub const fn recursion() -> Self {
        ParseError {
            kind: ParseErrorKind::Recursion,
        }
    }

    /// Returns a rendered version of the given error.
    ///
    /// # Panics
    /// This function will panic if the file supplied is too short to contain the token used for the error.
    /// This function will also panic if the expected field is set to `None`.
    pub fn display<F: File>(&self, file: &F, global: &Global) -> String {
        let ParseError {
            kind:
                ParseErrorKind::ExpectedFound {
                    expected,
                    found,
                    span,
                },
        } = self
        else {
            return String::from("recursion limit reached!");
        };

        let Some(expected) = expected else {
            panic!("expected() was not called before displaying this error.");
        };

        let mut f = String::new();

        let span = span.unwrap_or(Span::eof(file));

        _ = match found {
            TokenKind::Eof => write!(f, "unexpected EOF, expected {expected}"),
            _ => write!(f, "expected {expected}, found '{}'", found.display(global)),
        };

        _ = write!(f, "\n  --> {}", file.path().display());

        let (line, column) = file.line_and_column(span.start).expect("file is too short");
        _ = write!(f, ":{line}:{column}");

        f
    }
}

#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ParseErrorKind {
    ExpectedFound {
        expected: Option<Cow<'static, str>>,
        found: TokenKind,
        span: Option<Span>,
    },
    Recursion,
}

/// An extension trait to [`Result<T, ParseErrors>`].
pub trait DiagResult {
    /// If an `Err` is passed, this function applies [`ParseErrors::expected`] to the [`ParseErrors`].
    #[must_use]
    fn expected(self, str: &'static str) -> Self;
    /// If an `Err` is passed, this function applies [`ParseErrors::clear_expected`] to the [`ParseErrors`].
    #[must_use]
    fn clear_expected(self) -> Self;
}

impl<T> DiagResult for Result<T> {
    fn expected(self, str: &'static str) -> Self {
        self.map_err(|mut e| {
            e.expected(str);
            e
        })
    }

    fn clear_expected(self) -> Self {
        self.map_err(|mut e| {
            e.clear_expected();
            e
        })
    }
}
