//! Shared code for fuzzing.

#![allow(missing_docs)]

use std::path::Path;

use tackc_lib::prelude::*;

use tackc_lib::file::File;
use tackc_lib::frontend::lexer::Lexer;
use tackc_lib::frontend::parser::Parser;
use tackc_lib::global::Global;

/// Run the main fuzzing loop.
pub fn run(data: &[u8]) {
    let Ok(src_owned) = String::from_utf8(data.to_vec()) else {
        return;
    };

    // Create a borrowed file (no on-disk IO, avoids growing global intern tables).
    let file = File::new(src_owned, Path::new("fuzz_input.tck"));

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
            e.display(&global);
        }
        return;
    }

    // Try to parse an expression; we don't care about the result here — panics
    // and crashes are what the fuzzer should find.
    let (prog, errors, _failed) = Parser::parse(&tokens, &file, &global);
    for e in errors {
        e.display(&global);
    }
    prog.display(&global);

    // If the parser utterly fails, we probably shouldn't continue.
    /*if failed {
        return;
    }*/
}
