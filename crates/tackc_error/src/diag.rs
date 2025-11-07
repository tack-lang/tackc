use std::fs::File;

use tackc_span::Span;

pub struct Diag<'src> {
    file: &'src File,
    span: Option<Span>,
}
