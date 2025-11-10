use std::path::PathBuf;

use anyhow::{Context, Result};

use tackc_file::OwnedFile;
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_error::prelude::*;

fn main() -> Result<()> {
    let global = Global::new();

    let file: OwnedFile = PathBuf::from("../test.tck")
        .try_into()
        .context("Failed to open test.tck!")?;
    let file_interned = global.intern(file);

    let lexer = Lexer::new(global.get_interned(file_interned)).consume_reporter(|e| {
        println!("{e}");
    });

    for i in lexer {
        println!("{}", i.data);
    }

    Ok(())
}
