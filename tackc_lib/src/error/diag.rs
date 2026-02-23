use std::borrow::Cow;
use std::fmt::Write;

use crate::file::File;
use crate::span::Span;
use crate::utils::UnwrapExt;
use serde::{Deserialize, Serialize};

/// Diagnostic error struct.
#[derive(Serialize, Deserialize)]
pub struct Diag {
    // If `span` is `Some`, `span.0.is_empty()` should be `false`.
    span: Option<(Vec<Span>, Option<Cow<'static, str>>)>,
    msg: Cow<'static, str>,
}

impl Diag {
    /// Create a [`Diag`] without a span.
    pub fn without_span<S>(msg: S) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self {
            span: None,
            msg: msg.into(),
        }
    }

    /// Create a [`Diag`] with a span, but without a local message.
    pub fn with_span<S>(msg: S, span: Span) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self {
            span: Some((vec![span], None)),
            msg: msg.into(),
        }
    }

    /// Create a [`Diag`] with multiple spans, but without a local message.
    ///
    /// # Panics
    /// This function will panic if `spans` is empty.
    pub fn with_spans<S, I: IntoIterator<Item = Span>>(msg: S, spans: I) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        let spans_vec: Vec<Span> = spans.into_iter().collect();
        assert!(!spans_vec.is_empty(), "`spans` cannot be empty!");
        Self {
            span: Some((spans_vec, None)),
            msg: msg.into(),
        }
    }

    /// Create a [`Diag`] with a span, and a local message.
    pub fn with_local_msg<S1, S2>(msg: S1, span: Span, local_msg: S2) -> Self
    where
        S1: Into<Cow<'static, str>>,
        S2: Into<Cow<'static, str>>,
    {
        Self {
            span: Some((vec![span], Some(local_msg.into()))),
            msg: msg.into(),
        }
    }

    /// Returns the first span of this diagnostic.
    pub fn first_span(&self) -> Option<Span> {
        debug_assert!(!self.span.as_ref()?.0.is_empty(), "invariant violated!");

        Some(
            // `self.span.0` being non-empty is an invariant.
            *self.span.as_ref()?.0.first().expect_unreachable(), // CHECKED(Chloe)
        )
    }

    /// Returns the string representation of this `Diag`.
    ///
    /// # Panics
    /// This function panics if the file given is too short for the span inside of this `Diag`.
    pub fn display(&self, file: &File) -> String {
        let mut f = String::new();
        _ = write!(f, "{}", self.msg);
        _ = write!(f, "\n  --> {}", file.path().display());
        if let Some(span) = self.first_span() {
            assert!(span.fits(file.src()), "given file is too short!");

            let (line, column) = file.line_and_column(span.start).expect_unreachable(); // CHECKED(Chloe)
            _ = write!(f, ":{line}:{column}");
        }

        f
    }
}
