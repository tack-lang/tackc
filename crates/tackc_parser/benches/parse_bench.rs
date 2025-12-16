use std::path::Path;

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use tackc_ast::Program;
use tackc_file::BasicFile;
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_parser::ast::ProgramExt;

fn bench(c: &mut Criterion) {
    c.bench_function("bench", |b| {
        let file = BasicFile::new(include_str!("main_bench.tck"), Path::new("main_bench.tck"));

        b.iter_batched(
            || {
                let global = Global::create_heap();
                let tokens = Lexer::new(&file, &global)
                    .map(Result::unwrap)
                    .collect::<Vec<_>>()
                    .into_iter();
                (global, tokens)
            },
            |(global, iter)| Program::parse_file(iter, &global, &file),
            BatchSize::SmallInput,
        );
    });
}

criterion_group!(parse, bench);
criterion_main!(parse);
