use std::path::Path;

use tackc_analyze::resolution::resolve;
// Bring in the compiler pieces we want to fuzz
use tackc_error::iter::IteratorExt;
use tackc_file::BasicFile;
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_parser::ast::{AstNode, Program, ProgramExt};

pub fn run(data: &[u8]) {
    let Ok(src_owned) = String::from_utf8(data.to_vec()) else {
        return;
    };

    // Create a borrowed file (no on-disk IO, avoids growing global intern tables).
    let file = BasicFile::new(&src_owned, Path::new("fuzz_input.tck"));

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

    // Try to parse an expression; we don't care about the result here — panics
    // and crashes are what the fuzzer should find.
    let res = Program::parse_file(tokens.iter().copied(), &global, &file);
    let prog = match res {
        Ok(s) => {
            s.display(&global);
            s
        }
        Err(e) => {
            e.display(&file, &global);
            return;
        }
    };

    let (errors, _) = resolve(&mut [prog], &global);
    for _e in errors {
        //e.display(&file);
    }
}
