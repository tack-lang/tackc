use std::{
    collections::HashMap,
    hash::{BuildHasher, BuildHasherDefault, Hash, Hasher},
    num::{NonZeroU32, NonZeroU64},
};

use dashmap::DashMap;
use rustc_hash::FxHasher;

use crate::utils::UnwrapExt;

pub type NonZeroFxHasher = NonZeroHasher<FxHasher>;

pub struct NonZeroHasher<H> {
    inner: H,
}

impl<H: Hasher> NonZeroHasher<H> {
    pub fn finish_non_zero(&self) -> NonZeroU64 {
        // self.finish() is guaranteed to return a non-zero value.
        NonZeroU64::new(self.finish()).expect_unreachable() // CHECKED(Chloe)
    }

    pub fn finish_non_zero_truncated(&self) -> NonZeroU32 {
        let value = self.finish();
        // We are trying to truncate this.
        #[expect(clippy::cast_possible_truncation)] // CHECKED(Chloe)
        let low = value as u32;
        // `low` could be zero at this point, so ` | u32::from(low == 0)` ensures that it won't be zero.
        NonZeroU32::new(low | u32::from(low == 0)).expect_unreachable() // CHECKED(Chloe)
    }

    pub const fn new(hasher: H) -> Self {
        Self { inner: hasher }
    }
}

impl<H: Hasher + Default> Default for NonZeroHasher<H> {
    fn default() -> Self {
        Self::new(H::default())
    }
}

impl<H: Hasher> Hasher for NonZeroHasher<H> {
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        self.inner.write(bytes);
    }

    /// Return the current value of the hasher.
    ///
    /// This function will ALWAYS return a non-zero number.
    /// To get the value as a [`NonZeroU64`], see [`NonZeroHasher::finish_non_zero`].
    fn finish(&self) -> u64 {
        let hash = self.inner.finish();
        // Map a result of 0 to 1
        hash | u64::from(hash == 0)
    }
}

pub struct NonZeroFxHasherBuilder;

impl NonZeroFxHasherBuilder {
    /// Return the hash of `T`, as a non-zero value.
    pub fn hash_one_non_zero<T: Hash>(&self, val: T) -> NonZeroU64 {
        let mut hasher = self.build_hasher();
        val.hash(&mut hasher);
        hasher.finish_non_zero()
    }

    /// Return the hash of `T`, truncated to a `u32`, as a non-zero value.
    pub fn hash_one_non_zero_truncated<T: Hash>(&self, val: T) -> NonZeroU32 {
        let mut hasher = self.build_hasher();
        val.hash(&mut hasher);
        hasher.finish_non_zero_truncated()
    }
}

impl BuildHasher for NonZeroFxHasherBuilder {
    type Hasher = NonZeroFxHasher;

    fn build_hasher(&self) -> Self::Hasher {
        NonZeroFxHasher::default()
    }
}

/// A hasher that will do nothing with the value. If given a value greater than 8 bytes, the hasher will panic.
#[derive(Default)]
pub struct IdentityHasher {
    hash: u64,
}

impl Hasher for IdentityHasher {
    fn write(&mut self, bytes: &[u8]) {
        // Expect exactly 8 bytes for u64 keys
        assert_eq!(
            bytes.len(),
            8,
            "IdentityHasher can only be used for u64-sized types!"
        );

        // try_into() will always return `Ok`, since `bytes.len() == 8`.
        self.hash = u64::from_ne_bytes(bytes.try_into().expect_unreachable()); // CHECKED(Chloe)
    }

    fn write_u64(&mut self, i: u64) {
        self.hash = i;
    }

    fn finish(&self) -> u64 {
        self.hash
    }
}

pub type IdentityHashMap<K, V> = HashMap<K, V, BuildHasherDefault<IdentityHasher>>;
pub type IdentityDashMap<K, V> = DashMap<K, V, BuildHasherDefault<IdentityHasher>>;
