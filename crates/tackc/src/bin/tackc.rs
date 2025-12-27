use std::num::NonZeroU32;
use std::path::PathBuf;

use clap::{Parser as ClapParser, ValueEnum};

use rustc_hash::FxHashMap;
//use tackc_analyze::resolution::resolve;
use tackc_error::prelude::*;
use tackc_file::{BasicFile, File};
use tackc_global::Global;
use tackc_lexer::Lexer;
//use tackc_parser::ast::{AstNode, Program, ProgramExt};

use tackc_lexer::Token;
use tackc_parser::Parser;
use tackc_parser::ast::Program;

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
    let files: Vec<BasicFile> = args
        .files
        .iter()
        .map(PathBuf::as_ref)
        .map(|path| BasicFile::try_from(path).map_err(|x| (x, path)))
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
        .collect::<FxHashMap<NonZeroU32, BasicFile>>();

    let _progs = files_map
        .values()
        .map(|file| (run_lexer(file, global, &debug_modes), file))
        .map(|(tokens, file)| (run_parser(&tokens, file, global, &debug_modes), file))
        .collect::<Vec<_>>();

    //run_resolver(&mut asts, &files_map, global, &debug_modes);
}

fn run_lexer(file: &BasicFile, global: &Global, debug_modes: &DebugModes) -> Vec<Token> {
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
    tokens: &[Token],
    file: &BasicFile,
    global: &Global,
    debug_modes: &DebugModes,
) -> Program {
    let (prog, errs) = Parser::parse(tokens, file, global);

    if debug_modes.debug.contains(&Stage::Parser) {
        eprintln!("{prog:#?}");
    }
    if debug_modes.show.contains(&Stage::Parser) {
        eprintln!("{}", prog.display(global));
    }

    let str = errs
        .into_iter()
        .map(|err| err.display(file, global))
        .collect::<Vec<_>>()
        .join("\n\n");

    print!("{str}");
    if !str.is_empty() {
        println!();
    }

    prog
}

/*fn run_resolver(
    programs: &mut [Program],
    files: &FxHashMap<u64, BasicFile>,
    global: &Global,
    debug_modes: &DebugModes,
) -> bool {
    let (errors, global_scope) = resolve(programs, global);
    if debug_modes.debug.contains(&Stage::BindingResolution) {
        for prog in programs.iter_mut() {
            eprintln!("{prog:#?}");
        }
        eprintln!("{global_scope:#?}");
    }
    if debug_modes.show.contains(&Stage::BindingResolution) {
        for prog in programs.iter_mut() {
            eprintln!("{}", prog.display(global));
        }
    }
    if !errors.is_empty() {
        for e in errors {
            eprintln!("{}", e.display(files.get(&e.node.file).unwrap(), global));
        }
        false
    } else {
        true
    }
}*/
