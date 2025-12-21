//! Various no-dependency utilities for tackc.

pub mod prelude {
    pub use crate::iter::IteratorExt as _;
}

/// Hashing utilities.
pub mod hash;

pub mod iter;
