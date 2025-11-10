use std::fmt::Display;

use tackc_span::{Span, Spanned};
use thiserror::Error;

use tackc_file::File;

#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Token<'src> {
    Ident(&'src str),

    // Literals
    StringLit(Box<str>),
    IntLit(Box<IntegerLiteral>),
    FloatLit(Box<FloatLiteral>),

    Eof,

    // Keywords
    Func,
    Let,

    // Symbols
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,

    Eq,

    Semicolon,

    Plus,
    Dash,
    Star,
    Slash,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(ident) => write!(f, "{ident}"),

            Token::StringLit(string) => write!(f, "{string}"),
            Token::IntLit(int) => write!(f, "{int}"),
            Token::FloatLit(float) => write!(f, "{float}"),

            Token::Eof => write!(f, "<EOF>"),

            Token::Func => write!(f, "func"),
            Token::Let => write!(f, "let"),

            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),

            Token::Eq => write!(f, "="),

            Token::Semicolon => write!(f, ";"),

            Token::Plus => write!(f, "+"),
            Token::Dash => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Error {
    pub span: Span,
    pub ty: ErrorKind,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ErrorKind {
    #[error("unknown character {0}")]
    UnknownChar(char),
    #[error("unterminated string")]
    UnterminatedString,
    #[error("unexpected character {0}")]
    UnexpectedCharacter(char),
    #[error("expected digits after integer prefix")]
    MissingIntegerPrefixDigits,
    #[error("expected at least one digit in exponent")]
    MissingExponentDigits,
}

#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct IntegerLiteral {
    pub prefix: IntegerPrefix,
    pub digits: Box<str>,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.prefix, self.digits)
    }
}

#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum IntegerPrefix {
    /// No prefix
    Decimal,
    /// `0b` prefix
    Binary,
    /// `0o` prefix
    Octal,
    /// `0x` prefix
    Hex,
}

impl Display for IntegerPrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Decimal => write!(f, ""),
            Self::Binary => write!(f, "0b"),
            Self::Octal => write!(f, "0o"),
            Self::Hex => write!(f, "0x"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FloatLiteral {
    pub pre_dot_digits: Box<str>,
    pub post_dot_digits: Option<Box<str>>,
    // (sign_is_positive, digits)
    pub post_e: Option<(bool, Box<str>)>,
}

impl Display for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pre_dot_digits)?;
        if let Some(post_dot) = &self.post_dot_digits {
            write!(f, ".{post_dot}")?;
        }
        if let Some((sign, post_e)) = &self.post_e {
            write!(f, "e")?;
            if !*sign {
                write!(f, "-")?;
            }
            write!(f, "{post_e}")?;
        }
        Ok(())
    }
}

pub struct Lexer<'src, F: File> {
    src: &'src F,
    span: Span,
}

impl<'src, F: File> Lexer<'src, F> {
    pub fn new(src: &'src F) -> Self {
        Lexer {
            src,
            span: Span::new(),
        }
    }

    pub fn at_eof(&self) -> bool {
        self.src.len() <= self.span.end as usize
    }

    fn current_byte(&self) -> Option<u8> {
        self.src.as_bytes().get(self.span.end as usize).copied()
    }

    fn next_byte(&mut self) -> Option<u8> {
        if self.at_eof() {
            None
        } else {
            let temp = self.current_byte();
            self.span.end += 1;
            temp
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.at_eof() {
            match self.current_byte().unwrap() {
                c if c.is_ascii_whitespace() => {
                    self.next_byte();
                }
                _ => break,
            }
        }
        self.span.reset();
    }

    fn make_token(&mut self, ty: Token<'src>) -> Spanned<Token<'src>> {
        let span = self.span;
        self.span.reset();
        Spanned { span, data: ty }
    }

    fn current_lexeme(&self) -> &'src str {
        self.span.apply_bytes(self.src)
    }

    fn make_error(&mut self, ty: ErrorKind) -> Error {
        let span = self.span;
        self.span.reset();

        Error { span, ty }
    }

    fn handle_single_character_or_unknown(
        &mut self,
        c: char,
    ) -> Result<Spanned<Token<'src>>, Error> {
        let ty = match c {
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            '{' => Token::OpenBrace,
            '}' => Token::CloseBrace,

            '=' => Token::Eq,

            ';' => Token::Semicolon,

            '+' => Token::Plus,
            '-' => Token::Dash,
            '*' => Token::Star,
            '/' => Token::Slash,

            c => return Err(self.make_error(ErrorKind::UnknownChar(c))),
        };

        Ok(self.make_token(ty))
    }

    /// Handle a multi-byte UTF-8 character, given the first byte.
    ///
    /// # Returns
    /// Returns the character itself.
    fn handle_utf8_extras(&mut self, c: u8) -> char {
        let extra_bytes = match c {
            0b1100_0000..=0b1101_1111 => 1, // 2-byte
            0b1110_0000..=0b1110_1111 => 2, // 3-byte
            0b1111_0000..=0b1111_0111 => 3, // 4-byte
            _ => 0,
        };

        for _ in 0..extra_bytes {
            self.next_byte();
        }

        let lexeme = self.current_lexeme();
        let char_len = extra_bytes + 1;
        lexeme[lexeme.len() - char_len..].chars().next().unwrap()
    }

    fn handle_string_lit(&mut self) -> Result<Spanned<Token<'src>>, Error> {
        let mut string = String::new();
        loop {
            match self.next_byte() {
                None => return Err(self.make_error(ErrorKind::UnterminatedString)),
                Some(b'"') => {
                    break;
                }
                Some(c) if c & 0x80 != 0 => {
                    let char = self.handle_utf8_extras(c);

                    string.push(char);
                }
                Some(c) => string.push(c as char),
            }
        }
        Ok(self.make_token(Token::StringLit(string.into_boxed_str())))
    }

    fn handle_digits(&mut self) -> bool {
        #[inline]
        fn current_is_digit<F: File>(lexer: &mut Lexer<'_, F>) -> bool {
            matches!(lexer.current_byte(), Some(c) if c.is_ascii_digit())
        }
        #[inline]
        fn current_is_underscore<F: File>(lexer: &mut Lexer<'_, F>) -> bool {
            lexer.current_byte() == Some(b'_')
        }

        if current_is_digit(self) || current_is_underscore(self) {
            self.next_byte();
        } else {
            return false;
        }

        while current_is_digit(self) || current_is_underscore(self) {
            self.next_byte();
        }
        true
    }

    fn clean_digits(lexeme: &str) -> String {
        lexeme.chars().filter(|&c| c != '_').collect()
    }

    #[allow(clippy::unnecessary_wraps)]
    fn handle_int_lit_before_prefix(
        &mut self,
        prefix: IntegerPrefix,
    ) -> Result<Spanned<Token<'src>>, Error> {
        // Don't, `next_token` does this.
        //self.next_byte(); // skip '0'

        self.next_byte(); // skip prefix char
        if !self.handle_digits() {
            return Err(self.make_error(ErrorKind::MissingIntegerPrefixDigits));
        }

        let digits = Self::clean_digits(&self.current_lexeme()[2..]);
        Ok(self.make_token(Token::IntLit(Box::new(IntegerLiteral {
            prefix,
            digits: digits.into_boxed_str(),
        }))))
    }

    fn handle_num_lit(&mut self, c: u8) -> Result<Spanned<Token<'src>>, Error> {
        // Prefixed integer literals (0b, 0o, 0x)
        if let (b'0', Some(prefix)) = (c, self.current_byte()) {
            let prefix = match prefix {
                b'b' => Some(IntegerPrefix::Binary),
                b'o' => Some(IntegerPrefix::Octal),
                b'x' => Some(IntegerPrefix::Hex),
                _ => None,
            };
            if let Some(p) = prefix {
                return self.handle_int_lit_before_prefix(p);
            }
        }

        let start = self.span.start;
        // No check needed, the only way this can trigger is if the current lexeme already has at least one digit.
        // If this returns false, there's still a digit in the lexeme, since that's the only way for `handle_num_lit` to be called.
        self.handle_digits();

        let digits1 = Self::clean_digits(self.current_lexeme());
        match self.current_byte() {
            Some(b'.') => {
                // Skip `.`
                self.next_byte();
                self.span.reset();
                let digits2 = if self.handle_digits() {
                    Self::clean_digits(self.current_lexeme())
                } else {
                    // If we don't find any digits, make it zero.
                    String::from("0")
                };

                // No exponent -> simple float
                if !matches!(self.current_byte(), Some(b'e' | b'E')) {
                    self.span.start = start;
                    return Ok(self.make_token(Token::FloatLit(Box::new(FloatLiteral {
                        pre_dot_digits: digits1.into_boxed_str(),
                        post_dot_digits: Some(digits2.into_boxed_str()),
                        post_e: None,
                    }))));
                }

                // Exponent part
                self.next_byte();
                self.handle_float_with_exponent(start, digits1, Some(digits2.into_boxed_str()))
            }
            Some(b'e' | b'E') => {
                self.next_byte();
                self.handle_float_with_exponent(start, digits1, None)
            }
            _ => {
                let integer = IntegerLiteral {
                    prefix: IntegerPrefix::Decimal,
                    digits: digits1.into_boxed_str(),
                };
                Ok(self.make_token(Token::IntLit(Box::new(integer))))
            }
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn handle_float_with_exponent(
        &mut self,
        start: u32,
        pre_dot_digits: String,
        post_dot_digits: Option<Box<str>>,
    ) -> Result<Spanned<Token<'src>>, Error> {
        let sign = match self.current_byte() {
            Some(b'-') => {
                self.next_byte();
                false
            }
            Some(b'+') => {
                self.next_byte();
                true
            }
            _ => true,
        };

        self.span.reset();
        if !self.handle_digits() {
            return Err(self.make_error(ErrorKind::MissingExponentDigits));
        }
        let digits3 = Self::clean_digits(self.current_lexeme());
        self.span.start = start;

        Ok(self.make_token(Token::FloatLit(Box::new(FloatLiteral {
            pre_dot_digits: pre_dot_digits.into_boxed_str(),
            post_dot_digits,
            post_e: Some((sign, digits3.into_boxed_str())),
        }))))
    }

    #[allow(clippy::unnecessary_wraps)]
    fn handle_ident_or_keyword(&mut self) -> Result<Spanned<Token<'src>>, Error> {
        while let Some(c) = self.current_byte() {
            match c {
                b'_' => {}
                c if c.is_ascii_alphanumeric() => {}
                _ => break,
            }
            self.next_byte();
        }

        let ty = match self.current_lexeme() {
            "func" => Token::Func,
            "let" => Token::Let,
            ident => Token::Ident(ident),
        };

        Ok(self.make_token(ty))
    }

    #[allow(clippy::missing_panics_doc)]
    /// Gets the next token in the lexer. Nearly equivilant to [`next()`](`Self::next()`), but returns an [`TokenKind::Eof`] token instead of `None`.
    ///
    /// # Errors
    /// See [`ErrorKind`].
    pub fn next_token(&mut self) -> Result<Spanned<Token<'src>>, Error> {
        self.skip_whitespace();
        if self.at_eof() {
            return Ok(self.make_token(Token::Eof));
        }

        match self.next_byte().unwrap() {
            b'"' => self.handle_string_lit(),
            c if c.is_ascii_digit() => self.handle_num_lit(c),
            c if c.is_ascii_alphabetic() => self.handle_ident_or_keyword(),
            c if c & 0x80 != 0 => {
                let char = self.handle_utf8_extras(c);

                self.handle_single_character_or_unknown(char)
            }
            c => self.handle_single_character_or_unknown(c as char),
        }
    }
}

impl<'src, F: File> Iterator for Lexer<'src, F> {
    type Item = Result<Spanned<Token<'src>>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        match tok {
            Ok(Spanned {
                data: Token::Eof, ..
            }) => None,
            _ => Some(tok),
        }
    }
}

#[cfg(test)]
mod tests {
    #[cfg(test)]
    use tackc_file::OwnedFile;
    use tackc_macros::fuzz;

    use super::*;

    #[fuzz(20)]
    #[cfg(test)]
    #[allow(clippy::needless_pass_by_value)]
    fn lexer_fuzz(file: OwnedFile) {
        let lexer = Lexer::new(&file);
        for i in lexer {
            drop(std::hint::black_box(i));
        }
    }
}
