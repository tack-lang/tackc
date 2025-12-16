//! The lexer for tackc.

use std::fmt::Display;

use proptest::prelude::*;
use serde::{Deserialize, Serialize};
use tackc_span::Span;
use thiserror::Error;

use tackc_file::File;
use tackc_global::{Global, Interned};

/// The struct representing tokens in tackc.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct Token {
    /// The space in the file taken up by this token.
    pub span: Span,
    /// The kind of token this token is.
    pub kind: TokenKind,
}

impl Token {
    /// Create a new token.
    pub const fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }

    /// Display this token, using `global`.
    pub fn display(&self, global: &Global) -> String {
        self.kind.display(global)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum TokenKind {
    /// Identifiers
    Ident(Interned<str>),

    // Literals
    /// String literals
    StringLit(Interned<str>),
    /// Integer literals
    IntLit(Interned<str>, IntegerBase),
    /// Float literals
    FloatLit(Interned<str>),

    /// End of file
    Eof,

    // Keywords
    /// `func`
    Func,
    /// `let`
    Let,
    /// `const`
    Const,
    /// `mod`
    Mod,
    /// `u8`
    U8,
    /// `u16`
    U16,
    /// `u32`
    U32,
    /// `u64`
    U64,
    /// `i8`
    I8,
    /// `i16`
    I16,
    /// `i32`
    I32,
    /// `i64`
    I64,
    /// `exp`
    Exp,
    /// `imp`
    Imp,

    // Delimeters
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `[`
    LBracket,
    /// `]`
    RBracket,

    // Assignment operators
    /// `=`
    Eq,
    /// `+=`
    PlusEq,
    /// `-=`
    MinusEq,
    /// `*=`
    StarEq,
    /// `/=`
    SlashEq,

    // Symbols
    /// `,`
    Comma,
    /// `;`
    Semicolon,
    /// `.`
    Dot,
    /// `:`
    Colon,
    /// `|`
    Pipe,

    // Arithmatic operators
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,

    // Comparison operators
    /// `==`
    EqEq,
    /// `!=`
    BangEq,

    /// `>`
    Gt,
    /// `<`
    Lt,
    /// `>=`
    GtEq,
    /// `<=`
    LtEq,

    // Bitwise operators
    /// `||`
    PipePipe,
}

impl TokenKind {
    /// Gets the string representation of this token kind, using the given global.
    pub fn display(&self, global: &Global) -> String {
        match self {
            Self::Ident(ident) => ident.display(global).to_string(),

            Self::StringLit(string) => string.display(global).to_string(),
            Self::IntLit(int, base) => format!("{base}{}", int.display(global)),
            Self::FloatLit(float) => float.display(global).to_string(),

            ty => format!("{ty}"),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "<Ident: {ident:?}>"),

            Self::StringLit(string) => write!(f, "<StringLit: {string:?}>"),
            Self::IntLit(int, base) => write!(f, "<IntLit: {int:?}, base: {}>", *base as u32),
            Self::FloatLit(float) => write!(f, "<FloatLit: {float:?}>"),

            Self::Eof => write!(f, "<EOF>"),

            Self::Func => write!(f, "func"),
            Self::Let => write!(f, "let"),
            Self::Const => write!(f, "const"),
            Self::Mod => write!(f, "mod"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::Exp => write!(f, "exp"),
            Self::Imp => write!(f, "imp"),

            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),

            Self::Eq => write!(f, "="),
            Self::PlusEq => write!(f, "+="),
            Self::MinusEq => write!(f, "-="),
            Self::StarEq => write!(f, "*="),
            Self::SlashEq => write!(f, "/="),

            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::Dot => write!(f, "."),
            Self::Colon => write!(f, ":"),
            Self::Pipe => write!(f, "|"),

            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),

            Self::EqEq => write!(f, "=="),
            Self::BangEq => write!(f, "!="),

            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::GtEq => write!(f, ">="),
            Self::LtEq => write!(f, "<="),

            Self::PipePipe => write!(f, "||"),
        }
    }
}

/// An error in the lexer.
#[derive(Debug, Error, PartialEq, Eq, Serialize, Deserialize)]
pub struct LexError {
    /// The span of the file that caused the error
    pub span: Span,
    /// This kind of error this error is.
    pub kind: ErrorKind,
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[allow(missing_docs)]
#[derive(Debug, Error, PartialEq, Eq, Serialize, Deserialize)]
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

/// Different integer bases representable by this lexer
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize, Deserialize)]
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

/// Tack's lexer. `Lexer` implements iterator, but [`next_token`](Lexer::next_token) can also be called.
///
/// `Lexer`'s `Iterator` implementation returns `None` when `next_token` returns `Ok` with a token kind of [`TokenKind::Eof`].
/// `Lexer` should be easily cloneable.
pub struct Lexer<'src, F: File> {
    src: &'src F,
    span: Span,
    global: &'src Global,
}

impl<'src, F: File> Lexer<'src, F> {
    /// Create a new lexer
    pub fn new(src: &'src F, global: &'src Global) -> Self {
        Lexer {
            src,
            span: Span::new(),
            global,
        }
    }

    /// Returns true if the lexer is at the end of the given file.
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
        while let Some(c) = self.current_byte()
            && c.is_ascii_whitespace()
        {
            self.next_byte();
        }
        self.span.reset();
    }

    const fn make_token(&mut self, ty: TokenKind) -> Token {
        let span = self.span;
        self.span.reset();
        Token { span, kind: ty }
    }

    fn current_lexeme(&self) -> &'src str {
        self.span.apply_bytes(self.src)
    }

    const fn make_error(&mut self, ty: ErrorKind) -> LexError {
        let span = self.span;
        self.span.reset();

        LexError { span, kind: ty }
    }

    fn handle_double_character_or_unknown(&mut self, c1: char) -> Result<Token, LexError> {
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

            ('>', Some(b'=')) => {
                self.next_byte();
                TokenKind::GtEq
            }
            ('<', Some(b'=')) => {
                self.next_byte();
                TokenKind::LtEq
            }
            ('>', _) => TokenKind::Gt,
            ('<', _) => TokenKind::Lt,

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
        lexeme[lexeme.len() - char_len..]
            .chars()
            .next()
            .expect("This is a bug. Please submit a bug report.")
    }

    fn handle_string_lit(&mut self) -> Result<Token, LexError> {
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
        fn current_is_digit<F: File>(lexer: &Lexer<'_, F>, radix: u32) -> bool {
            matches!(lexer.current_byte(), Some(c) if (c as char).is_digit(radix))
        }
        #[inline]
        fn current_is_underscore<F: File>(lexer: &Lexer<'_, F>) -> bool {
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

    fn handle_int_lit_before_prefix(&mut self, prefix: IntegerBase) -> Result<Token, LexError> {
        // Don't, `next_token` does this.
        //self.next_byte(); // skip '0'

        self.next_byte(); // skip prefix char
        if !self.handle_digits(prefix as u32) {
            return Err(self.make_error(ErrorKind::MissingIntegerPrefixDigits));
        }

        let digits = Self::clean_digits(&self.current_lexeme()[2..]);
        Ok(self.make_token(TokenKind::IntLit(self.global.intern_str(digits), prefix)))
    }

    fn handle_num_lit(&mut self, c: u8) -> Result<Token, LexError> {
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

    fn handle_float_with_exponent(&mut self) -> Result<Token, LexError> {
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

    fn handle_ident_or_keyword(&mut self) -> Token {
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
            "mod" => TokenKind::Mod,

            "u8" => TokenKind::U8,
            "u16" => TokenKind::U16,
            "u32" => TokenKind::U32,
            "u64" => TokenKind::U64,
            "i8" => TokenKind::I8,
            "i16" => TokenKind::I16,
            "i32" => TokenKind::I32,
            "i64" => TokenKind::I64,

            "exp" => TokenKind::Exp,
            "imp" => TokenKind::Imp,

            ident => TokenKind::Ident(self.global.intern_str(ident)),
        };

        self.make_token(ty)
    }

    /// Gets the next token in the lexer. Nearly equivilant to [`next()`](`Self::next()`), but returns an [`TokenKind::Eof`] token instead of `None`.
    ///
    /// # Errors
    /// See [`ErrorKind`].
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();
        let Some(c) = self.next_byte() else {
            return Ok(self.make_token(TokenKind::Eof));
        };

        match c {
            b'"' => self.handle_string_lit(),
            c if c.is_ascii_digit() => self.handle_num_lit(c),
            c if c.is_ascii_alphabetic() => Ok(self.handle_ident_or_keyword()),
            b'_' => Ok(self.handle_ident_or_keyword()),
            c if c & 0x80 != 0 => {
                let char = self.handle_utf8_extras(c);

                self.handle_double_character_or_unknown(char)
            }
            c => self.handle_double_character_or_unknown(c as char),
        }
    }
}

impl<F: File> Iterator for Lexer<'_, F> {
    type Item = Result<Token, LexError>;

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
        use tackc_file::BasicFile;

        let file = BasicFile::new(&s, Path::new("proptest.tck"));
        let global = Global::create_heap();
        let lexer = Lexer::new(&file, &global);
        for i in lexer {
            drop(i);
        }
    }
}

#[test]
fn lexer_test_glob() {
    insta::glob!("lexer/*.tck", run_lexer_test);
}

#[cfg(test)]
use std::path::Path;

#[cfg(test)]
fn run_lexer_test(path: &Path) {
    use tackc_file::BasicFile;

    let file =
        BasicFile::try_from(path).unwrap_or_else(|_| panic!("Could not file {}!", path.display()));
    let global = Global::create_heap();
    let lexer = Lexer::new(&file, &global);
    let tokens = lexer.collect::<Vec<_>>();
    insta::assert_ron_snapshot!(tokens);
}
