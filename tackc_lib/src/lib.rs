//! The main library for tackc's code.

#[cfg(test)]
macro_rules! setup_insta_test {
    () => {
        use insta::Settings;

        let mut settings = Settings::clone_current();
        settings.set_sort_maps(true);
        settings.set_omit_expression(true);
        let _guard = settings.bind_to_scope();
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
