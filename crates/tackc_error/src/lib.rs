//! A crate for error constructs in tackc.

#[allow(missing_docs)]
pub mod prelude {
    pub use crate::iter::IteratorExt as _;
}

/// Iterator adapters for errors
pub mod iter;

/// Diagnostic error messages
pub mod diag;
pub use diag::Diag;
