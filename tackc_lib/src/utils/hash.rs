//! The module for hashing utilities.

// We're using HashMap not as hashing, but as the template for putting in a new hasher.
#[expect(clippy::disallowed_types)] // CHECKED(Chloe)
use std::collections::HashMap;

use std::{
    hash::{BuildHasher, BuildHasherDefault, Hash, Hasher},
    marker::PhantomData,
    num::{NonZeroU32, NonZeroU64},
};

use dashmap::DashMap;
use rustc_hash::{FxHashMap, FxHasher};

use crate::utils::UnwrapExt;

/// A non-zero returning version of [`FxHasher`].
pub type NonZeroFxHasher = NonZeroHasher<FxHasher>;

impl NonZeroFxHasher {
    /// Creates a non-zero returning version of [`FxHasher`], in a `const` context.
    pub const fn default() -> Self {
        Self::new(FxHasher::default())
    }
}

/// A wrapper around a hasher that returns a non-zero value. This is to take advantage of the [null-pointer optimization][npo].
///
/// # Implementation
/// The way this adapter is implemented is by returning one if the inner hasher returns zero.
///
/// [npo]: https://doc.rust-lang.org/std/option/index.html#representation
pub struct NonZeroHasher<H> {
    inner: H,
}

impl<H: Hasher> NonZeroHasher<H> {
    /// Returns a [`NonZeroU64`] equivilent to the hashed value.
    pub fn finish_non_zero(&self) -> NonZeroU64 {
        // self.finish() is guaranteed to return a non-zero value.
        NonZeroU64::new(self.finish()).expect_unreachable() // CHECKED(Chloe)
    }

    /// Returns a [`NonZeroU32`] equivilent to the hashed value.
    pub fn finish_non_zero_truncated(&self) -> NonZeroU32 {
        let value = self.finish();
        // We are trying to truncate this.
        #[expect(clippy::cast_possible_truncation)] // CHECKED(Chloe)
        let low = value as u32;
        // `low` could be zero at this point, so ` | u32::from(low == 0)` ensures that it won't be zero.
        NonZeroU32::new(low | u32::from(low == 0)).expect_unreachable() // CHECKED(Chloe)
    }

    /// Creates a new [`NonZeroHasher`], using an existing hasher as the inner hasher.
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

/// This is an implementation of [`BuildHasher`] that can build [`NonZeroFxHasher`].
/// There are also extra functions specific to the non-zero hasher.
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

/// A hasher that will do nothing with the value to hash. If given a value greater than 8 bytes, the hasher will panic.
#[derive(Default)]
pub struct IdentityHasher {
    hash: u64,
}

impl Hasher for IdentityHasher {
    fn write(&mut self, bytes: &[u8]) {
        // Expect exactly 8 bytes
        assert_eq!(
            bytes.len(),
            8,
            "IdentityHasher can only be used for 8-byte values!"
        );

        // try_into() will always return `Ok`, since `bytes.len() == 8`.
        let bytes = <&[u8] as TryInto<[u8; 8]>>::try_into(bytes).expect_unreachable(); // CHECKED(Chloe)
        self.hash = u64::from_ne_bytes(bytes);
    }

    fn write_u64(&mut self, i: u64) {
        self.hash = i;
    }

    fn finish(&self) -> u64 {
        self.hash
    }
}

/// An alias for a [`HashMap`] that uses [`IdentityHasher`].
// We're using HashMap not as hashing, but as the template for putting in a new hasher.
#[expect(clippy::disallowed_types)] // CHECKED(Chloe)
pub type IdentityHashMap<K, V> = HashMap<K, V, BuildHasherDefault<IdentityHasher>>;
/// An alias for a [`DashMap`] that uses [`IdentityHasher`].
pub type IdentityDashMap<K, V> = DashMap<K, V, BuildHasherDefault<IdentityHasher>>;

/// An index into a [`IndexedSet`].
pub struct HashIndex<V>(u64, PhantomData<V>);

impl<V> Clone for HashIndex<V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<V> Copy for HashIndex<V> {}

impl<V> HashIndex<V> {
    /// Returns a new [`HashIndex`] using `value` as the inner value.
    pub const fn new(value: u64) -> Self {
        Self(value, PhantomData)
    }

    /// Gets the inner value of this [`HashIndex`].
    pub const fn get(&self) -> u64 {
        self.0
    }
}

/// A wrapper for a [`HashMap`] that can be indexed directly by an item's hash.
pub struct IndexedSet<V> {
    inner: FxHashMap<u64, V>,
}

impl<V: Hash> Default for IndexedSet<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V: Hash> IndexedSet<V> {
    /// Creates a new empty [`IndexedSet`].
    pub fn new() -> Self {
        Self {
            inner: FxHashMap::default(),
        }
    }

    /// Inserts a value into the set, and returns its index.
    pub fn insert(&mut self, value: V) -> HashIndex<V> {
        let hasher = self.inner.hasher();
        let hash = HashIndex::<V>::new(hasher.hash_one(&value));

        self.inner.insert(hash.get(), value);

        hash
    }

    /// Returns a value from the set, provided its index.
    pub fn get(&self, hash: HashIndex<V>) -> Option<&V> {
        self.inner.get(&hash.get())
    }
}
