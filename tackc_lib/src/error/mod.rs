//! A crate for error constructs in tackc.

/// The prelude for this module.
pub mod prelude {
    pub use crate::error::iter::IteratorExt as _;
}

/// Iterator adapters for errors.
pub mod iter;

/// Diagnostic error messages.
pub mod diag;
pub use diag::Diag;
