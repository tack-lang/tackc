use std::fmt::Display;

use proptest::prelude::*;
use proptest_derive::Arbitrary;
use tackc_span::Span;
use thiserror::Error;

use tackc_file::File;
use tackc_global::{Global, Interned};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Arbitrary)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Token { span, kind }
    }

    pub fn display(&self, global: &Global) -> String {
        self.kind.display(global)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

fn int_lit_strategy() -> impl Strategy<Value = TokenKind> {
    let int_lit_binary = "[0-1]".prop_map(|str| (str, IntegerBase::Binary));
    let int_lit_octal = "[0-7]".prop_map(|str| (str, IntegerBase::Octal));
    let int_lit_decimal = "[0-9]".prop_map(|str| (str, IntegerBase::Decimal));
    let int_lit_hex = "[0-9a-fA-F]".prop_map(|str| (str, IntegerBase::Hex));

    prop_oneof![int_lit_binary, int_lit_octal, int_lit_decimal, int_lit_hex,]
        .prop_map(|(str, base)| TokenKind::IntLit(Interned::<str>::get_interned(&str), base))
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Arbitrary)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum TokenKind {
    Ident(Interned<str>),

    // Literals
    StringLit(Interned<str>),
    #[proptest(strategy = "int_lit_strategy()")]
    IntLit(Interned<str>, IntegerBase),
    FloatLit(Interned<str>),

    Eof,

    // Keywords
    Func,
    Let,
    Const,

    // Delimeters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Assignment operators
    Eq,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,

    // Symbols
    Comma,
    Semicolon,
    Dot,
    Colon,
    Pipe,

    // Arithmatic operators
    Plus,
    Minus,
    Star,
    Slash,

    // Comparison operators
    EqEq,
    BangEq,

    // Bitwise operators
    PipePipe,
}

impl TokenKind {
    pub fn display(&self, global: &Global) -> String {
        match self {
            TokenKind::Ident(ident) => ident.display(global).to_string(),

            TokenKind::StringLit(string) => string.display(global).to_string(),
            TokenKind::IntLit(int, base) => format!("{base}{}", int.display(global)),
            TokenKind::FloatLit(float) => float.display(global).to_string(),

            ty => format!("{ty}"),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Ident(ident) => write!(f, "<Ident: {ident:?}>"),

            TokenKind::StringLit(string) => write!(f, "<StringLit: {string:?}>"),
            TokenKind::IntLit(int, base) => write!(f, "<IntLit: {int:?}, base: {}>", *base as u32),
            TokenKind::FloatLit(float) => write!(f, "<FloatLit: {float:?}>"),

            TokenKind::Eof => write!(f, "<EOF>"),

            TokenKind::Func => write!(f, "func"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Const => write!(f, "const"),

            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),

            TokenKind::Eq => write!(f, "="),
            TokenKind::PlusEq => write!(f, "+="),
            TokenKind::MinusEq => write!(f, "-="),
            TokenKind::StarEq => write!(f, "*="),
            TokenKind::SlashEq => write!(f, "/="),

            TokenKind::Comma => write!(f, ","),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Pipe => write!(f, "|"),

            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),

            TokenKind::EqEq => write!(f, "=="),
            TokenKind::BangEq => write!(f, "!="),

            TokenKind::PipePipe => write!(f, "||"),
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum IntegerBase {
    /// No prefix
    Decimal = 10,
    /// `0b` prefix
    Binary = 2,
    /// `0o` prefix
    Octal = 8,
    /// `0x` prefix
    Hex = 16,
}

impl Display for IntegerBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Decimal => write!(f, ""),
            Self::Binary => write!(f, "0b"),
            Self::Octal => write!(f, "0o"),
            Self::Hex => write!(f, "0x"),
        }
    }
}

pub struct Lexer<'src, F: File> {
    src: &'src F,
    span: Span,
    global: &'src Global,
}

impl<'src, F: File> Lexer<'src, F> {
    pub fn new(src: &'src F, global: &'src Global) -> Self {
        Lexer {
            src,
            span: Span::new(),
            global,
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

    fn peek_byte(&self) -> Option<u8> {
        self.src.as_bytes().get(self.span.end as usize + 1).copied()
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

    fn make_token(&mut self, ty: TokenKind) -> Token {
        let span = self.span;
        self.span.reset();
        Token { span, kind: ty }
    }

    fn current_lexeme(&self) -> &'src str {
        self.span.apply_bytes(self.src)
    }

    fn make_error(&mut self, ty: ErrorKind) -> Error {
        let span = self.span;
        self.span.reset();

        Error { span, kind: ty }
    }

    fn handle_double_character_or_unknown(&mut self, c1: char) -> Result<Token, Error> {
        let c2 = self.current_byte();
        let ty = match (c1, c2) {
            ('(', _) => TokenKind::LParen,
            (')', _) => TokenKind::RParen,
            ('{', _) => TokenKind::LBrace,
            ('}', _) => TokenKind::RBrace,
            ('[', _) => TokenKind::LBracket,
            (']', _) => TokenKind::RBracket,

            ('=', Some(b'=')) => {
                self.next_byte();
                TokenKind::EqEq
            }
            ('!', Some(b'=')) => {
                self.next_byte();
                TokenKind::BangEq
            }

            ('|', Some(b'|')) => {
                self.next_byte();
                TokenKind::PipePipe
            }

            ('+', Some(b'=')) => {
                self.next_byte();
                TokenKind::PlusEq
            }
            ('=', _) => TokenKind::Eq,

            (',', _) => TokenKind::Comma,
            (';', _) => TokenKind::Semicolon,
            ('.', _) => TokenKind::Dot,
            (':', _) => TokenKind::Colon,
            ('|', _) => TokenKind::Pipe,

            ('+', _) => TokenKind::Plus,
            ('-', _) => TokenKind::Minus,
            ('*', _) => TokenKind::Star,
            ('/', _) => TokenKind::Slash,

            (c, _) => return Err(self.make_error(ErrorKind::UnknownChar(c))),
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

    fn handle_string_lit(&mut self) -> Result<Token, Error> {
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
        Ok(self.make_token(TokenKind::StringLit(self.global.intern_str(string))))
    }

    fn handle_digits(&mut self, radix: u32) -> bool {
        #[inline]
        fn current_is_digit<F: File>(lexer: &mut Lexer<'_, F>, radix: u32) -> bool {
            matches!(lexer.current_byte(), Some(c) if (c as char).is_digit(radix))
        }
        #[inline]
        fn current_is_underscore<F: File>(lexer: &mut Lexer<'_, F>) -> bool {
            lexer.current_byte() == Some(b'_')
        }

        if current_is_digit(self, radix) || current_is_underscore(self) {
            self.next_byte();
        } else {
            return false;
        }

        while current_is_digit(self, radix) || current_is_underscore(self) {
            self.next_byte();
        }
        true
    }

    fn clean_digits(lexeme: &str) -> String {
        lexeme.chars().filter(|&c| c != '_').collect()
    }

    #[allow(clippy::unnecessary_wraps)]
    fn handle_int_lit_before_prefix(&mut self, prefix: IntegerBase) -> Result<Token, Error> {
        // Don't, `next_token` does this.
        //self.next_byte(); // skip '0'

        self.next_byte(); // skip prefix char
        if !self.handle_digits(prefix as u32) {
            return Err(self.make_error(ErrorKind::MissingIntegerPrefixDigits));
        }

        let digits = Self::clean_digits(&self.current_lexeme()[2..]);
        Ok(self.make_token(TokenKind::IntLit(self.global.intern_str(digits), prefix)))
    }

    fn handle_num_lit(&mut self, c: u8) -> Result<Token, Error> {
        // Prefixed integer literals (0b, 0o, 0x)
        if let (b'0', Some(prefix)) = (c, self.current_byte()) {
            let prefix = match prefix {
                b'b' => Some(IntegerBase::Binary),
                b'o' => Some(IntegerBase::Octal),
                b'x' => Some(IntegerBase::Hex),
                _ => None,
            };
            if let Some(p) = prefix {
                return self.handle_int_lit_before_prefix(p);
            }
        }

        let start = self.span.start;
        // No check needed, the only way this can trigger is if the current lexeme already has at least one digit.
        // If this returns false, there's still a digit in the lexeme, since that's the only way for `handle_num_lit` to be called.
        self.handle_digits(10);

        match self.current_byte() {
            Some(b'.') => {
                if let Some(c) = self.peek_byte()
                    && (matches!(c, b'_' | b'.') || c.is_ascii_alphabetic())
                {
                    return Ok(self.make_token(TokenKind::IntLit(
                        self.global
                            .intern_str(Self::clean_digits(self.current_lexeme())),
                        IntegerBase::Decimal,
                    )));
                }

                // Skip `.`
                self.next_byte();
                self.handle_digits(10);

                // No exponent -> simple float
                if !matches!(self.current_byte(), Some(b'e' | b'E')) {
                    self.span.start = start;
                    return Ok(self.make_token(TokenKind::FloatLit(
                        self.global
                            .intern_str(Self::clean_digits(self.current_lexeme())),
                    )));
                }

                // Exponent part
                self.next_byte();
                self.handle_float_with_exponent()
            }
            Some(b'e' | b'E') => {
                self.next_byte();
                self.handle_float_with_exponent()
            }
            _ => Ok(self.make_token(TokenKind::IntLit(
                self.global
                    .intern_str(Self::clean_digits(self.current_lexeme())),
                IntegerBase::Decimal,
            ))),
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn handle_float_with_exponent(&mut self) -> Result<Token, Error> {
        if let Some(b'-' | b'+') = self.current_byte() {
            self.next_byte();
        }

        if !self.handle_digits(10) {
            return Err(self.make_error(ErrorKind::MissingExponentDigits));
        }

        Ok(self.make_token(TokenKind::FloatLit(
            self.global
                .intern_str(Self::clean_digits(self.current_lexeme())),
        )))
    }

    #[allow(clippy::unnecessary_wraps)]
    fn handle_ident_or_keyword(&mut self) -> Result<Token, Error> {
        while let Some(c) = self.current_byte() {
            match c {
                b'_' => {}
                c if c.is_ascii_alphanumeric() => {}
                _ => break,
            }
            self.next_byte();
        }

        let ty = match self.current_lexeme() {
            "func" => TokenKind::Func,
            "let" => TokenKind::Let,
            "const" => TokenKind::Const,
            ident => TokenKind::Ident(self.global.intern_str(ident)),
        };

        Ok(self.make_token(ty))
    }

    #[allow(clippy::missing_panics_doc)]
    /// Gets the next token in the lexer. Nearly equivilant to [`next()`](`Self::next()`), but returns an [`TokenKind::Eof`] token instead of `None`.
    ///
    /// # Errors
    /// See [`ErrorKind`].
    pub fn next_token(&mut self) -> Result<Token, Error> {
        self.skip_whitespace();
        if self.at_eof() {
            return Ok(self.make_token(TokenKind::Eof));
        }

        match self.next_byte().unwrap() {
            b'"' => self.handle_string_lit(),
            c if c.is_ascii_digit() => self.handle_num_lit(c),
            c if c.is_ascii_alphabetic() => self.handle_ident_or_keyword(),
            b'_' => self.handle_ident_or_keyword(),
            c if c & 0x80 != 0 => {
                let char = self.handle_utf8_extras(c);

                self.handle_double_character_or_unknown(char)
            }
            c => self.handle_double_character_or_unknown(c as char),
        }
    }
}

impl<F: File> Iterator for Lexer<'_, F> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        match tok {
            Ok(Token {
                kind: TokenKind::Eof,
                ..
            }) => None,
            _ => Some(tok),
        }
    }
}

impl<F: File> Clone for Lexer<'_, F> {
    fn clone(&self) -> Self {
        Lexer {
            src: self.src,
            span: self.span,
            global: self.global,
        }
    }
}

proptest! {
    #[test]
    fn lexer(s in ".{1,4096}") {
        use tackc_file::BorrowedFile;

        let s = std::hint::black_box(s);
        let file = BorrowedFile::new(&s, "proptest.tck");
        let global = Global::create_heap();
        let lexer = Lexer::new(&file, &global);
        for i in lexer {
            drop(i);
        }
    }
}
