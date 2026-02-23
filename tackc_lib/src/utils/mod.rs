//! Various utilities for tackc.

/// The prelude for this module.
pub mod prelude {
    pub use super::UnwrapExt as _;
    pub use super::iter::IteratorExt as _;
    pub use super::tree::TreeItemExt as _;
}

pub mod hash;
pub mod iter;
pub mod tree;

/// The extension trait for things that can be unwrapped. ([`Option`], [`Result`], etc.)
pub trait UnwrapExt<T> {
    /// Returns the contained value, panicking if there is none.
    ///
    /// Can be used to avoid triggering [`clippy::missing_panics_doc`](https://rust-lang.github.io/rust-clippy/master/index.html#missing_panics_doc).
    ///
    /// # Safety
    /// While this function is 100% safe (and should stay that way), this function should not be called unless you can be 100% sure that the value will have a value.
    fn expect_unreachable(self) -> T
    where
        Self: Sized,
        T: Sized;
}

impl<T> UnwrapExt<T> for Option<T> {
    #[inline]
    fn expect_unreachable(self) -> T
    where
        Self: Sized,
        T: Sized,
    {
        // This is completely intended.
        self.unwrap_or_else(|| unreachable!()) // CHECKED(Chloe)
    }
}

impl<T, E> UnwrapExt<T> for Result<T, E> {
    #[inline]
    fn expect_unreachable(self) -> T
    where
        Self: Sized,
        T: Sized,
    {
        // This is completely intended.
        self.unwrap_or_else(|_| unreachable!()) // CHECKED(Chloe)
    }
}
