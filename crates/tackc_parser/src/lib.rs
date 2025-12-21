use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::Token;
use tackc_utils::{iter::Peekable, prelude::*};

pub struct Parser<'src, F, I: Iterator> {
    file: &'src F,
    global: &'src Global,
    lexer: Peekable<I, 2>,
}

impl<'src, F: File, I> Parser<'src, F, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(lexer: I, file: &'src F, global: &'src Global) -> Self {
        Parser {
            file,
            global,
            lexer: lexer.peekable_tackc(),
        }
    }
}
