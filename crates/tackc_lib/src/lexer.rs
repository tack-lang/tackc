//! The lexer for tackc.

use std::fmt::Display;

use crate::error::Diag;
use crate::span::Span;
use proptest::prelude::*;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::file::File;
use crate::global::{Global, Interned};

/// The struct representing tokens in tackc.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct Token {
    /// The space in the file taken up by this token.
    pub span: Span,
    /// The kind of token this token is.
    pub kind: TokenKind,
    /// Lexeme of this token. For string literals, this is the value of the literal.
    pub lexeme: Interned<str>,
}

impl From<Token> for Span {
    fn from(value: Token) -> Self {
        value.span
    }
}

impl Token {
    /// Create a new token.
    pub const fn new(span: Span, kind: TokenKind, lexeme: Interned<str>) -> Self {
        Self { span, kind, lexeme }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
#[repr(u8)]
pub enum TokenKind {
    /// Identifiers
    Ident,

    // Literals
    /// String literals
    StringLit,
    /// Integer literals
    IntLit,
    /// Float literals
    FloatLit,

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
    /// `!`
    Bang,

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

impl Token {
    /// Display this token, using `global`.
    pub fn display(&self, global: &Global) -> String {
        match self.kind {
            TokenKind::Ident | TokenKind::StringLit | TokenKind::IntLit | TokenKind::FloatLit => {
                self.lexeme.display(global).to_string()
            }

            ty => format!("{ty}"),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident => write!(f, "<Ident>"),

            Self::StringLit => write!(f, "<StringLit>"),
            Self::IntLit => write!(f, "<IntLit>"),
            Self::FloatLit => write!(f, "<FloatLit>"),

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
            Self::Bang => write!(f, "!"),

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
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LexError {
    /// The span of the file that caused the error
    pub span: Span,
    /// This kind of error this error is.
    pub kind: ErrorKind,
}

impl LexError {
    pub fn display<F: File>(&self, file: &F) -> String {
        Diag::with_span(self.kind.to_string(), self.span).display(file)
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

    fn skip_whitespace_and_comments(&mut self) {
        while let Some(c) = self.current_byte() {
            match c {
                b'/' if self.peek_byte() == Some(b'/') => {
                    self.next_byte();
                    self.next_byte();
                    while let Some(c) = self.peek_byte()
                        && c != b'\n'
                    {
                        self.next_byte();
                    }
                    self.next_byte();
                    self.next_byte();
                }
                c if c.is_ascii_whitespace() => {
                    self.next_byte();
                }
                _ => break,
            }
        }
        self.span.reset();
    }

    fn make_token(&mut self, kind: TokenKind) -> Token {
        self.make_token_with_lexeme(kind, self.current_lexeme())
    }

    fn make_token_with_lexeme(&mut self, kind: TokenKind, lexeme: &str) -> Token {
        let span = self.span;
        self.span.reset();
        Token::new(span, kind, self.global.intern_str(lexeme))
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
            ('!', _) => TokenKind::Bang,

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
        Ok(self.make_token_with_lexeme(TokenKind::StringLit, &string))
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

    fn handle_int_lit_before_prefix(&mut self, prefix: IntegerBase) -> Result<Token, LexError> {
        // Don't, `next_token` does this.
        //self.next_byte(); // skip '0'

        self.next_byte(); // skip prefix char
        if !self.handle_digits(prefix as u32) {
            return Err(self.make_error(ErrorKind::MissingIntegerPrefixDigits));
        }

        Ok(self.make_token(TokenKind::IntLit))
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
                    return Ok(self.make_token(TokenKind::IntLit));
                }

                // Skip `.`
                self.next_byte();
                self.handle_digits(10);

                // No exponent -> simple float
                if !matches!(self.current_byte(), Some(b'e' | b'E')) {
                    self.span.start = start;
                    return Ok(self.make_token(TokenKind::FloatLit));
                }

                // Exponent part
                self.next_byte();
                self.handle_float_with_exponent()
            }
            Some(b'e' | b'E') => {
                self.next_byte();
                self.handle_float_with_exponent()
            }
            _ => Ok(self.make_token(TokenKind::IntLit)),
        }
    }

    fn handle_float_with_exponent(&mut self) -> Result<Token, LexError> {
        if let Some(b'-' | b'+') = self.current_byte() {
            self.next_byte();
        }

        if !self.handle_digits(10) {
            return Err(self.make_error(ErrorKind::MissingExponentDigits));
        }

        Ok(self.make_token(TokenKind::FloatLit))
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

            _ => TokenKind::Ident,
        };

        self.make_token(ty)
    }

    /// Gets the next token in the lexer. Nearly equivilant to [`next()`](`Self::next()`), but returns an [`TokenKind::Eof`] token instead of `None`.
    ///
    /// # Errors
    /// See [`ErrorKind`].
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace_and_comments();
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
        use crate::file::BasicFile;

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
    setup_insta_test!();

    insta::glob!("lexer-tests/*.tck", run_lexer_test);
}

#[cfg(test)]
use std::path::Path;

#[cfg(test)]
fn run_lexer_test(path: &Path) {
    use crate::file::BasicFile;

    let src = std::fs::read_to_string(path).unwrap();
    let path = Path::new(path.file_name().unwrap());
    let file = BasicFile::new(src, path);

    let global = Global::create_heap();
    let lexer = Lexer::new(&file, &global);
    let tokens = lexer.collect::<Vec<_>>();
    insta::assert_ron_snapshot!(tokens);
}
