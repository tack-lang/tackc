//! Binary for tackc.

use std::num::NonZeroU32;
use std::path::PathBuf;

use clap::{Parser as ClapParser, ValueEnum};

use rustc_hash::FxHashMap;

use tackc_ast::AstModule;
use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_lexer::Token;
use tackc_lib::prelude::*;
use tackc_parser::Parser;
use tackc_sema::module_resolution::resolve_mods;

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
    BindingResolution,
}

fn main() {
    let args = Args::parse();
    let debug_modes = DebugModes {
        debug: args.debug,
        show: args.show,
    };
    let global = Global::new();
    let mut error = false;
    let files: Vec<File> = args
        .files
        .iter()
        .map(PathBuf::as_ref)
        .map(|path| File::try_from(path).map_err(|x| (x, path)))
        .consume_reporter(|(e, path)| {
            error = true;
            println!("failed to open file {}: {e:?}", path.display());
        })
        .collect::<Vec<_>>();
    if error {
        return;
    }
    let files_map = files
        .into_iter()
        .map(|file| (file.id(), file))
        .collect::<FxHashMap<NonZeroU32, File>>();

    let mut failed = false;

    let mods = files_map
        .values()
        .map(|file| (run_lexer(file, global, &debug_modes), file))
        .map(|(tokens, file)| run_parser(&tokens, file, global, &debug_modes))
        .map(|(module, failed_parse)| {
            if failed_parse {
                failed = true;
            }
            module
        })
        .collect::<Vec<_>>();

    if failed {
        return;
    }

    let _mod_list = resolve_mods(mods, global);
    //println!("{}", mods.display(global));
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
                Err(e) => eprintln!("{}", e.display(file)),
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
        eprintln!("{}\n", e.display(file));
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
        eprintln!("{}\n", err.display(file, global));
    }

    (module, failed)
}
