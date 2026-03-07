//! Binary for tackc.

use std::path::PathBuf;

use clap::{Parser as ClapParser, ValueEnum};

use colored::Colorize;

use tackc_lib::prelude::*;

use tackc_ast::AstModule;
use tackc_file::{File, FileList};
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_lexer::Token;
use tackc_parser::Parser;
use tackc_sema::{resolve_globals, resolve_mods};

#[derive(ClapParser)]
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
}

fn main() {
    let args = Args::parse();
    let debug_modes = DebugModes {
        debug: args.debug,
        show: args.show,
    };
    let global = Global::new();
    let mut failed = false;
    let files: Vec<File> = args
        .files
        .into_iter()
        .map(PathBuf::leak)
        .map(|path| &*path)
        .map(|path| File::try_from(path).map_err(|x| (x, path)))
        .consume_reporter(|(e, path)| {
            failed = true;
            println!("failed to open file {}: {e:?}", path.display());
        })
        .collect::<Vec<_>>();
    if failed {
        return;
    }

    let file_list = FileList::from(files);

    global.set_file_list(file_list);

    let mods = global
        .file_list()
        .values()
        .map(|file| (run_lexer(file, global, &debug_modes), file))
        .map(|(tokens, file)| run_parser(&tokens, file, global, &debug_modes))
        .map(|(module, error)| {
            if error {
                failed = true;
            }
            module
        })
        .collect::<Vec<_>>();

    if failed {
        return;
    }

    let (mods, errors) = resolve_mods(mods, global);
    for e in errors {
        eprintln!("{} {}", "error:".bright_red(), e.display(global));
    }
    println!("{}", mods.display(global));
    let globals = resolve_globals(&mods, global);
    println!("{}", globals.display(global));
}

fn run_lexer(file: &File, global: &Global, debug_modes: &DebugModes) -> Vec<Token> {
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
                Err(e) => eprintln!("{}", e.display(global)),
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
        eprintln!("{} {}\n", "error:".bright_red(), e.display(global));
    }

    tokens
}

fn run_parser<'src>(
    tokens: &[Token],
    file: &'src File,
    global: &'src Global,
    debug_modes: &DebugModes,
) -> (AstModule<'src>, bool) {
    let (module, errs, failed) = Parser::parse(tokens, file, global);

    if debug_modes.debug.contains(&Stage::Parser) {
        eprintln!("{module:#?}");
    }
    if debug_modes.show.contains(&Stage::Parser) {
        eprintln!("{}", module.display(global));
    }

    for err in errs {
        eprintln!("{} {}\n", "error:".bright_red(), err.display(global));
    }

    (module, failed)
}
