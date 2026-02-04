use std::{
    collections::HashMap,
    hash::{BuildHasher, BuildHasherDefault, Hash, Hasher},
    num::NonZeroU64,
};

use dashmap::DashMap;
use rustc_hash::FxHasher;

pub struct NonZeroHasher {
    inner: FxHasher,
}

impl NonZeroHasher {
    #[allow(clippy::missing_panics_doc)]
    pub fn finish_non_zero(&self) -> NonZeroU64 {
        NonZeroU64::new(self.finish()).unwrap()
    }

    pub const fn default() -> Self {
        Self {
            inner: FxHasher::default(),
        }
    }
}

impl Default for NonZeroHasher {
    fn default() -> Self {
        Self::default()
    }
}

impl Hasher for NonZeroHasher {
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        self.inner.write(bytes);
    }

    fn finish(&self) -> u64 {
        let hash = self.inner.finish();
        // Map a result of 0 to 1
        hash | u64::from(hash == 0)
    }
}

pub struct NonZeroHasherBuilder;

impl NonZeroHasherBuilder {
    pub fn hash_one_non_zero<T: Hash>(&self, val: T) -> NonZeroU64 {
        let mut hasher = self.build_hasher();
        val.hash(&mut hasher);
        hasher.finish_non_zero()
    }
}

impl BuildHasher for NonZeroHasherBuilder {
    type Hasher = NonZeroHasher;

    fn build_hasher(&self) -> Self::Hasher {
        NonZeroHasher::default()
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
        self.hash = u64::from_ne_bytes(bytes.try_into().unwrap());
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
