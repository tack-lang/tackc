//! The main library for tackc's code.

/// The compiler hash of tackc, as reported by `tackc_meta::hash`.
pub const COMPILER_HASH: &str = env!("TACKC_COMPILER_HASH");
/// The compiler version of tackc, as specified in `Cargo.toml`.
pub const COMPILER_VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(test)]
macro_rules! setup_insta_test {
    () => {
        use insta::Settings;

        let mut settings = Settings::clone_current();
        settings.set_sort_maps(true);
        settings.set_omit_expression(true);
        settings.set_snapshot_path(concat!(env!("CARGO_MANIFEST_DIR"), "/snapshots"));
        let _guard = settings.bind_to_scope();
    };
}

macro_rules! insta_test {
    ($name:ident, $glob:expr, $func:expr) => {
        #[test]
        fn $name() {
            setup_insta_test!();

            insta::glob!($glob, |path| {
                // This is a test, so this would be a panic-able error.
                let input = std::fs::read_to_string(path).unwrap(); // CHECKED(Chloe)
                $func(input);
            });
        }
    };
}

pub mod error;
pub mod file;
pub mod frontend;
pub mod global;
pub mod span;
pub mod utils;

/// Combines the preludes of all the other modules.
pub mod prelude {
    pub use crate::error::prelude::*;
    pub use crate::utils::prelude::*;
}

insta_test!(ui_test, "ui-tests/*.tck", run_ui_test);

#[cfg(test)]
fn run_ui_test(src: String) {
    use std::fmt::Write;
    use std::path::Path;

    use insta::assert_snapshot;

    use crate::{
        error::iter::IteratorExt,
        file::{File, FileList},
        frontend::{lexer::Lexer, parser::Parser},
        global::Global,
    };

    let file = File::new(src, Path::new("testing.tck"));
    let id = file.id();
    let mut file_list = FileList::new();
    file_list.insert(file);

    let mut global = Global::create_heap();
    global.set_file_list(file_list);

    // We added this file.
    let file = global.file_list().get(id).unwrap(); // CHECKED(Chloe)

    let lexer = Lexer::new(file, &global);
    let mut lex_errors = Vec::new();
    let tokens = lexer
        .consume_reporter(|e| lex_errors.push(e))
        .collect::<Vec<_>>();

    let mut stdout = String::new();

    for e in lex_errors {
        _ = writeln!(stdout, "{}", e.display(&global));
    }

    let (_, parse_errors, _) = Parser::parse(&tokens, file, &global);

    for e in parse_errors {
        _ = writeln!(stdout, "{}", e.display(&global));
    }

    assert_snapshot!(stdout);
}
