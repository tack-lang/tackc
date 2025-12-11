use std::hash::{BuildHasherDefault, Hasher};

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

/// This hasher builder can be used as the third generic parameter in many hashmap types in order to use [`IdentityHasher`] as the hasher.
pub type IdentityHasherBuilder = BuildHasherDefault<IdentityHasher>;
