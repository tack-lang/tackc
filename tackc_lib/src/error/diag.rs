use std::borrow::Cow;

use crate::span::Span;
use crate::{global::Global, utils::UnwrapExt};
use serde::{Deserialize, Serialize};
use thin_vec::ThinVec;

/// Diagnostic error struct.
#[derive(Serialize, Deserialize)]
pub enum Diag {
    /// A [`Diag`] without a span.
    Message(Cow<'static, str>),
    /// A [`Diag`] with a span, but without a local message.
    WithSpan(Cow<'static, str>, Span),
    /// A [`Diag`] with multiple spans, but without a local message.
    ///
    /// The spans must be non-empty.
    WithSpans(Cow<'static, str>, ThinVec<Span>),
}

impl Diag {
    /// Create a [`Diag`] without a span.
    pub fn without_span<S>(msg: S) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self::Message(msg.into())
    }

    /// Create a [`Diag`] with a span, but without a local message.
    pub fn with_span<S>(msg: S, span: Span) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self::WithSpan(msg.into(), span)
    }

    /// Create a [`Diag`] with multiple spans, but without a local message.
    ///
    /// # Panics
    /// This function will panic if `spans` is empty.
    pub fn with_spans<S, I: IntoIterator<Item = Span>>(msg: S, spans: I) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        let vec = spans.into_iter().collect::<ThinVec<_>>();
        assert!(!vec.is_empty(), "`spans` is empty!");
        Self::WithSpans(msg.into(), vec)
    }

    /// Returns the string representation of this `Diag`.
    ///
    /// # Panics
    /// This function panics if the file given is too short for the span inside of this `Diag`.
    pub fn display(&self, global: &Global) -> String {
        match self {
            Self::Message(msg) => msg.to_string(),
            Self::WithSpan(msg, span) => {
                let Some(file) = global.file_list().get(&span.file) else {
                    panic!("missing file from global file list!");
                };
                let Some((line, column)) = file.line_and_column(span.start) else {
                    panic!("");
                };
                format!("{msg}\n  --> {}:{line}:{column}", file.path().display())
            }
            Self::WithSpans(msg, spans) => {
                // This is an invariant.
                let span = spans.first().expect_unreachable(); // CHECKED(Chloe)

                let Some(file) = global.file_list().get(&span.file) else {
                    panic!("missing file from global file list!");
                };
                let Some((line, column)) = file.line_and_column(span.start) else {
                    panic!("");
                };
                format!("{msg}\n  --> {}:{line}:{column}", file.path().display())
            }
        }
    }
}
