use std::borrow::Cow;
use std::fmt::Write;

use tackc_file::File;
use tackc_span::Span;

pub struct Diag {
    span: Option<(Span, Option<Cow<'static, str>>)>,
    msg: Cow<'static, str>,
}

impl Diag {
    pub fn without_span<S>(msg: S) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self {
            span: None,
            msg: msg.into(),
        }
    }

    pub fn with_span<S>(msg: S, span: Span) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self {
            span: Some((span, None)),
            msg: msg.into(),
        }
    }

    pub fn with_local_msg<S1, S2>(msg: S1, span: Span, local_msg: S2) -> Self
    where
        S1: Into<Cow<'static, str>>,
        S2: Into<Cow<'static, str>>,
    {
        Self {
            span: Some((span, Some(local_msg.into()))),
            msg: msg.into(),
        }
    }

    /// Returns the string representation of this `Diag`.
    /// 
    /// # Panics
    /// This function panics if the file given is too short for the span inside of this `Diag`.
    pub fn display<F: File>(&self, file: &F) -> String {
        let mut f = String::new();
        _ = write!(f, "{}", self.msg);
        _ = write!(f, "\n  --> {}", file.path().display());
        if let Some((span, _local_msg)) = &self.span {
            let (line, column) = file.line_and_column(span.start).expect("file is too short");
            _ = write!(f, ":{line}:{column}");
        }

        f
    }
}
