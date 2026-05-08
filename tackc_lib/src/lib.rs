//! The main library for tackc's code.

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
