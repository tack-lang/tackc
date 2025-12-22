use std::{borrow::Cow, result::Result as StdResult};

use serde::{Deserialize, Serialize};
use tackc_lexer::Token;

pub type Result<T, E = ParseError> = StdResult<T, E>;

#[derive(Debug, Serialize, Deserialize)]
pub enum ParseError {
    Expected(Option<Cow<'static, str>>, Token),
    Eof(Option<Cow<'static, str>>),
}

impl ParseError {
    pub fn expected(expected: Option<&'static str>, found: Token) -> Self {
        Self::Expected(expected.map(Into::into), found)
    }

    pub fn eof(expected: Option<&'static str>) -> Self {
        Self::Eof(expected.map(Into::into))
    }
}
