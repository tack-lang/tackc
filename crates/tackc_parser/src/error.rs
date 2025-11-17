use std::borrow::Cow;
use std::fmt::Display;
use std::fmt::Write;
use std::ops::Deref;
use std::result::Result as StdResult;

pub type Result<T, E = ParseErrors> = StdResult<T, E>;

use ecow::EcoVec;
use tackc_file::File;
use tackc_lexer::Token;
use tackc_span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ParseErrors {
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

    #[allow(clippy::missing_panics_doc)]
    pub fn expected(&mut self, str: &'static str) {
        let errors = self.errors.make_mut();
        let first = errors.last_mut().unwrap();
        if first.expected.is_none() {
            first.expected = Some(str.into());
        }
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn display<F: File>(&self, file: &F) -> impl Display {
        let mut f = String::new();
        let mut errors = self.errors.iter();
        _ = write!(f, "{}", errors.next().unwrap().display(file));

        for i in errors {
            _ = write!(f, "\n\n{}", i.display(file));
        }

        f
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
    pub expected: Option<Cow<'static, str>>,
    pub found: Option<Token>,
}

impl ParseError {
    #[must_use]
    pub fn new(expected: Option<&'static str>, found: Token) -> Self {
        ParseError {
            expected: expected.map(Into::into),
            found: Some(found),
        }
    }

    #[must_use]
    pub fn eof(expected: Option<&'static str>) -> Self {
        ParseError {
            expected: expected.map(Into::into),
            found: None,
        }
    }

    /// Returns a rendered version of the given error.
    ///
    /// # Panics
    /// This function will panic if the file supplied is too short to contain the token used for the error.
    pub fn display<F: File>(&self, file: &F) -> impl Display {
        let Some(expected) = &self.expected else {
            panic!("expected was never set!");
        };

        let mut f = String::new();

        _ = match &self.found {
            Some(found) => write!(f, "expected {expected}, found '{found}'"),
            None => write!(f, "unexpected EOF, expected {expected}"),
        };

        let span = self
            .found
            .as_ref()
            .map_or(Span::eof(file.src()), |tok| tok.span);

        _ = write!(f, "\n  --> {}", file.path().display());

        let (line, column) = file
            .line_and_column(span.start)
            .unwrap_or_else(|| panic!("file is too short"));
        _ = write!(f, ":{line}:{column}");

        f
    }
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
