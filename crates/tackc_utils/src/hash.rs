use std::hash::{BuildHasherDefault, Hasher};

/// A hasher that just uses the key as the hash.
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

pub type IdentityHasherBuilder = BuildHasherDefault<IdentityHasher>;
