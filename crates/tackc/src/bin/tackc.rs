use std::path::PathBuf;

use clap::{Parser, ValueEnum};

use tackc_analyze::resolution::resolve;
use tackc_error::prelude::*;
use tackc_file::OwnedFile;
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_parser::ast::{AstNode, Program, ProgramExt};

use tackc_lexer::Token;

#[derive(Parser)]
struct Args {
    #[cfg_attr(debug_assertions, clap(short, long))]
    #[cfg_attr(not(debug_assertions), clap(skip))]
    debug: Vec<Stage>,
    
    #[cfg_attr(debug_assertions, clap(short, long))]
    #[cfg_attr(not(debug_assertions), clap(skip))]
    show: Vec<Stage>,
}

#[derive(Clone, ValueEnum, PartialEq, Eq, Copy)]
enum Stage {
    Lexer,
    Parser,
    //BindingResolution,
}

fn main() {
    let args = Args::parse();

    let global = Global::new();

    let file = OwnedFile::try_from(PathBuf::from("../test.tck")).unwrap();

    let Some(tokens) = run_lexer(&file, global, &args) else {
        return;
    };

    let Some(mut prog) = run_parser(tokens, &file, global, &args) else {
        return;
    };

    if !run_resolver(&mut prog, &file, global, &args) {}
}

fn run_lexer(file: &OwnedFile, global: &Global, args: &Args) -> Option<Vec<Token>> {
    #[cfg(not(debug_assertions))]
    {_ = args;}

    let lexer = Lexer::new(file, global);
    let tokens = lexer.collect::<Vec<_>>();

    if args.debug.contains(&Stage::Lexer) {
        for token in &tokens {
            eprintln!("{token:?}");
        }
    }
    if args.show.contains(&Stage::Lexer) {
        for token in &tokens {
            match token {
                Ok(token) => eprintln!("{}", token.display(global)),
                Err(e) => eprintln!("{e}"),
            }
        }
    }

    let mut errors = Vec::new();
    let tokens = tokens
        .into_iter()
        .consume_reporter(|e| {
            errors.push(e);
        })
        .collect();

    let out = errors.is_empty().then_some(tokens).ok_or(errors);
    match out {
        Ok(tokens) => Some(tokens),
        Err(errors) => {
            for e in errors {
                eprintln!("{e}");
            }
            None
        }
    }
}

fn run_parser(
    tokens: Vec<Token>,
    file: &OwnedFile,
    global: &Global,
    args: &Args,
) -> Option<Program> {
    let res = Program::parse_file(tokens.iter().copied(), global, file);

    match res {
        Ok(prog) => {
            if args.debug.contains(&Stage::Parser) {
                eprintln!("{prog:#?}");
            }
            if args.show.contains(&Stage::Parser) {
                eprintln!("{}", prog.display(global));
            }
            Some(prog)
        }
        Err(errs) => {
            if args.debug.contains(&Stage::Parser) {
                eprintln!("{errs:#?}");
            }
            eprintln!("{}", errs.display(file, global));

            None
        }
    }
}

fn run_resolver(program: &mut Program, file: &OwnedFile, global: &Global, _: &Args) -> bool {
    let errors = resolve(program, global);
    if !errors.is_empty() {
        for e in errors {
            eprintln!("{}", e.display(file));
        }
        false
    } else {
        true
    }
}
