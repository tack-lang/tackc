//! Various no-dependency utilities for tackc.

pub mod prelude {
    pub use super::UnwrapExt as _;
    pub use super::iter::IteratorExt as _;
    pub use super::tree::TreeItemExt as _;
}

pub mod iter;

pub mod tree;

pub trait UnwrapExt<T> {
    fn expect_unreachable(self) -> T
    where
        Self: Sized,
        T: Sized;
}

impl<T> UnwrapExt<T> for Option<T> {
    /// Returns the contained [`Some`] value, panicking if the option is a [`None`].
    /// Can be used to avoid triggering [`clippy::missing_panics_doc`](https://rust-lang.github.io/rust-clippy/master/index.html#missing_panics_doc).
    ///
    /// # Safety
    /// While this function is 100% safe (and should stay that way), this function should not be called unless you can be 100% sure that the value will not be [`None`].
    #[inline]
    fn expect_unreachable(self) -> T
    where
        Self: Sized,
        T: Sized,
    {
        self.unwrap_or_else(|| unreachable!())
    }
}

impl<T, E> UnwrapExt<T> for Result<T, E> {
    /// Returns the contained [`Ok`] value, panicking if the option is a [`Err`].
    /// Can be used to avoid triggering [`clippy::missing_panics_doc`](https://rust-lang.github.io/rust-clippy/master/index.html#missing_panics_doc).
    ///
    /// # Safety
    /// While this function is 100% safe (and should stay that way), this function should not be called unless you can be 100% sure that the value will not be [`Err`].
    #[inline]
    fn expect_unreachable(self) -> T
    where
        Self: Sized,
        T: Sized,
    {
        self.unwrap_or_else(|_| unreachable!())
    }
}
