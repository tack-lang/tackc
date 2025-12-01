use std::path::Path;
use tackc_parser::ast::Expression;
use tackc_parser::error::DiagResult;

// Bring in the compiler pieces we want to fuzz
use tackc_error::iter::IteratorExt;
use tackc_file::BorrowedFile;
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_parser::Parser;
use tackc_parser::ast::AstNode;

pub fn run(data: &[u8]) {
    let Ok(src_owned) = String::from_utf8(data.to_vec()) else {
        return;
    };

    // Create a borrowed file (no on-disk IO, avoids growing global intern tables).
    let file = BorrowedFile::new(&src_owned, Path::new("fuzz_input.tck"));

    // Use a heap-allocated Global for each iteration to avoid the single-call
    // restriction of `Global::new()` in debug builds.
    let global = Global::create_heap();

    let mut errors = Vec::new();
    let lexer = Lexer::new(&file, &global).consume_reporter(|e| {
        errors.push(e);
    });
    let tokens = lexer.collect::<Vec<_>>();

    if !errors.is_empty() {
        for e in errors {
            e.to_string();
        }
        return;
    }

    // Parser expects an iterator of `Token` that implements `Clone`.
    let mut parser = Parser::new(tokens.iter().copied());

    // Try to parse an expression; we don't care about the result here — panics
    // and crashes are what the fuzzer should find.
    let res = Expression::parse(&mut parser, 0).expected("expression");
    match res {
        Ok(s) => {
            s.display(&global);
        }
        Err(e) => {
            e.display(&file, &global);
        }
    };
}