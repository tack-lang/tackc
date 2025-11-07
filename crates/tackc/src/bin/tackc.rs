use std::path::PathBuf;

use anyhow::{Context, Result};

use tackc_blob::{prelude::*, file::File, global::Global, lexer::Lexer};

fn main() -> Result<()> {
    let global = Global::new();

    let file: File = PathBuf::from("../test.tck")
        .try_into()
        .context("Failed to open test.tck!")?;
    let file_interned = global.intern(file);

    let lexer = Lexer::new(global.get_interned(file_interned)).consume_reporter(|e| {
        println!("{e}");
    });

    for i in lexer {
        println!("{}", i.ty);
    }

    Ok(())
}
