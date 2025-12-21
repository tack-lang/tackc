use std::{hash::Hasher, num::NonZeroU64};

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
