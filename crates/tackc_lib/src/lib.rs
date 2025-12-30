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

    pub use super::error as tackc_error;
    pub use super::ast as tackc_ast;
    pub use super::file as tackc_file;
    pub use super::global as tackc_global;
    pub use super::hash as tackc_hash;
    pub use super::lexer as tackc_lexer;
    pub use super::parser as tackc_parser;
    pub use super::sema as tackc_sema;
    pub use super::span as tackc_span;
    pub use super::utils as tackc_utils;
}
