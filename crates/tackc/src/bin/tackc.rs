use std::path::PathBuf;

use anyhow::Result;

use tackc_error::prelude::*;
use tackc_file::OwnedFile;
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_parser::ast::Program;

fn main() -> Result<()> {
    let global = Global::new();

    /*let file: OwnedFile = PathBuf::from("../test.tck")
    .try_into()
    .context("Failed to open test.tck!")?;*/
    let file = OwnedFile::try_from(PathBuf::from("../test.tck")).unwrap();
    let file_interned = global.intern(file);
    let file_ref = global.get_interned(file_interned);

    let mut errors = Vec::new();
    let lexer = Lexer::new(file_ref, global).consume_reporter(|e| {
        errors.push(e);
    });
    let tokens = lexer.collect::<Vec<_>>();

    if !errors.is_empty() {
        for e in errors {
            println!("{e}");
        }
        return Ok(());
    }

    let res = Program::parse(tokens.iter().copied());
    match res {
        Ok(prog) => {
            println!("{}", prog.display(global));
        }
        Err(diags) => println!("{}", diags.display(file_ref, global)),
    }

    Ok(())
}
