use std::borrow::Cow;
use std::fmt::Write;
use std::ops::Deref;
use std::result::Result as StdResult;

pub type Result<T, E = ParseErrors> = StdResult<T, E>;

use ecow::EcoVec;
use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::Token;
use tackc_lexer::TokenKind;
use tackc_span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ParseErrors {
    // Length of errors will always be 1 or greater
    errors: EcoVec<ParseError>,
}

impl ParseErrors {
    pub fn new(error: ParseError) -> Self {
        ParseErrors {
            errors: EcoVec::from([error]),
        }
    }

    pub fn push(&mut self, diag: ParseError) {
        self.errors.push(diag);
    }

    /// This function takes the most recent error from `self` and changes it's `expected` field to `Some(str)` if it's `None`.
    #[allow(clippy::missing_panics_doc)]
    pub fn expected(&mut self, str: &'static str) {
        debug_assert!(!self.errors.is_empty());

        let errors = self.errors.make_mut();
        let last = errors.last_mut().unwrap();
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

    #[allow(clippy::missing_panics_doc)]
    pub fn display<F: File>(&self, file: &F, global: &Global) -> String {
        let mut str = String::new();
        let mut errors = self.errors.iter();
        _ = write!(str, "{}", errors.next().unwrap().display(file, global));

        for i in errors {
            _ = write!(str, "\n\n{}", i.display(file, global));
        }

        str
    }

    #[allow(clippy::needless_pass_by_value)]
    pub fn merge(&mut self, other: ParseErrors) {
        self.errors.extend_from_slice(&other);
    }
}

impl Deref for ParseErrors {
    type Target = [ParseError];

    fn deref(&self) -> &Self::Target {
        &self.errors
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ParseError {
    pub kind: ParseErrorKind,
}

impl ParseError {
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

    #[must_use]
    pub fn recursion() -> Self {
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

        let span = span.unwrap_or(Span::eof(file.src()));

        _ = match found {
            TokenKind::Eof => write!(f, "unexpected EOF, expected {expected}"),
            _ => write!(f, "expected {expected}, found '{}'", found.display(global)),
        };

        _ = write!(f, "\n  --> {}", file.path().display());

        let (line, column) = file
            .line_and_column(span.start)
            .unwrap_or_else(|| panic!("file is too short"));
        _ = write!(f, ":{line}:{column}");

        f
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ParseErrorKind {
    ExpectedFound {
        expected: Option<Cow<'static, str>>,
        found: TokenKind,
        span: Option<Span>,
    },
    Recursion,
}

pub trait DiagResult {
    #[must_use]
    fn pushed_with<F: FnOnce() -> ParseError>(self, diag: F) -> Self;
    #[must_use]
    fn expected(self, str: &'static str) -> Self;
}

impl<T> DiagResult for Result<T> {
    fn pushed_with<F: FnOnce() -> ParseError>(self, diag: F) -> Self {
        self.map_err(|mut e| {
            e.push(diag());
            e
        })
    }

    fn expected(self, str: &'static str) -> Self {
        self.map_err(|mut e| {
            e.expected(str);
            e
        })
    }
}
