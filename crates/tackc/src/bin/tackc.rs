use std::path::PathBuf;

use anyhow::Result;

use tackc_error::prelude::*;
use tackc_file::OwnedFile;
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_parser::{
    Parser,
    ast::{AstNode, Expr},
};

fn main() -> Result<()> {
    let global = Global::new();

    /*let file: OwnedFile = PathBuf::from("../test.tck")
    .try_into()
    .context("Failed to open test.tck!")?;*/
    let file = OwnedFile::try_from(PathBuf::from("../test.tck")).unwrap();
    let file_interned = global.intern(file);
    let file_ref = global.get_interned(file_interned);

    let lexer = Lexer::new(file_ref, global).consume_reporter(|e| {
        println!("{e}");
    });
    let mut parser = Parser::new(lexer);

    let res = Expr::parse(&mut parser, 0);
    match res {
        Ok(expr) => {
            println!("{}", expr.display(global));
        }
        Err(diags) => println!("{}", diags.display(file_ref)),
    }

    Ok(())
}
