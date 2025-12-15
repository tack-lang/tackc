use std::{hint::black_box, path::Path};

use criterion::{Criterion, criterion_group, criterion_main};
use tackc_ast::Program;
use tackc_file::BasicFile;
use tackc_global::Global;
use tackc_lexer::{Lexer, Token};
use tackc_parser::ast::ProgramExt;

fn bench(c: &mut Criterion) {
    c.bench_function("bench", |b| {
        let global = Global::create_heap();
        let file = BasicFile::new(include_str!("main_bench.tck"), Path::new("main_bench.tck"));
        let tokens: Vec<Token> = Lexer::new(&file, &global).map(Result::unwrap).collect();
        let iter = tokens.iter().copied();
        b.iter(|| {
            let file = black_box(&file);
            let global = Global::create_heap();
            let program = Program::parse_file(iter.clone(), &global, file);
            _ = black_box(program);
        });
    });
}

criterion_group!(parse, bench);
criterion_main!(parse);
