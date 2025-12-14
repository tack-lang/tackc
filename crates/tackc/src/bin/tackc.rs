use std::path::PathBuf;

use clap::{Parser, ValueEnum};

use tackc_analyze::resolution::resolve;
use tackc_error::prelude::*;
use tackc_file::BasicFile;
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

    #[clap(required = true)]
    files: Vec<PathBuf>,
}

struct DebugModes {
    debug: Vec<Stage>,
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
    let debug_modes = DebugModes {
        debug: args.debug,
        show: args.show,
    };
    let global = Global::new();
    let mut error = false;
    let files = args
        .files
        .iter()
        .map(PathBuf::as_ref)
        .map(|path| {
            BasicFile::try_from(path)
                .map_err(|x| (x, path))
                .map(|file| (file, path))
        })
        .consume_reporter(|(e, path)| {
            error = true;
            println!("failed to open file {}: {e:?}", path.display());
        })
        .collect::<Vec<_>>();
    if error {
        return;
    }

    let asts = files
        .into_iter()
        .map(|(file, path)| (run_lexer(&file, global, &debug_modes), file, path))
        .map(|(tokens, file, path)| (run_parser(tokens, &file, global, &debug_modes), path))
        .collect::<Vec<_>>();

    drop(asts);
}

fn run_lexer(file: &BasicFile, global: &Global, debug_modes: &DebugModes) -> Vec<Token> {
    #[cfg(not(debug_assertions))]
    {
        _ = debug_modes;
    }

    let lexer = Lexer::new(file, global);
    let tokens = lexer.collect::<Vec<_>>();

    if debug_modes.debug.contains(&Stage::Lexer) {
        for token in &tokens {
            eprintln!("{token:?}");
        }
    }
    if debug_modes.show.contains(&Stage::Lexer) {
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
        .skip_reporter(|e| {
            errors.push(e);
        })
        .collect();

    for e in errors {
        eprintln!("{e}");
    }

    tokens
}

fn run_parser(
    tokens: Vec<Token>,
    file: &BasicFile,
    global: &Global,
    debug_modes: &DebugModes,
) -> Option<Program> {
    let res = Program::parse_file(tokens.iter().copied(), global, file);

    match res {
        Ok(prog) => {
            if debug_modes.debug.contains(&Stage::Parser) {
                eprintln!("{prog:#?}");
            }
            if debug_modes.show.contains(&Stage::Parser) {
                eprintln!("{}", prog.display(global));
            }
            Some(prog)
        }
        Err(errs) => {
            if debug_modes.debug.contains(&Stage::Parser) {
                eprintln!("{errs:#?}");
            }
            eprintln!("{}", errs.display(file, global));

            None
        }
    }
}

fn _run_resolver(program: &mut Program, file: &BasicFile, global: &Global, _: &DebugModes) -> bool {
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
