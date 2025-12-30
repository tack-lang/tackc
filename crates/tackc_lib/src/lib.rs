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

pub mod ast;
pub mod error;
pub mod file;
pub mod global;
pub mod hash;
pub mod lexer;
pub mod parser;
pub mod sema;
pub mod span;
pub mod utils;

pub mod prelude {
    pub use super::error::prelude::*;
    pub use super::utils::prelude::*;

    pub use super::ast as tackc_ast;
    pub use super::error as tackc_error;
    pub use super::file as tackc_file;
    pub use super::global as tackc_global;
    pub use super::hash as tackc_hash;
    pub use super::lexer as tackc_lexer;
    pub use super::parser as tackc_parser;
    pub use super::sema as tackc_sema;
    pub use super::span as tackc_span;
    pub use super::utils as tackc_utils;
}
