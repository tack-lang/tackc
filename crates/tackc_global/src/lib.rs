use std::{
    any::Any,
    collections::HashMap,
    hash::{BuildHasherDefault, Hash, Hasher},
    marker::PhantomData,
};

use ahash::AHasher;

pub trait Internable: Any {
    fn dyn_hash(&self, hasher: &mut dyn Hasher);
    fn dyn_eq(&self, other: &dyn Any) -> bool;
}

impl<T: Any + Hash + PartialEq> Internable for T {
    fn dyn_hash(&self, mut hasher: &mut dyn Hasher) {
        self.hash(&mut hasher);
    }

    fn dyn_eq(&self, other: &dyn Any) -> bool {
        let Some(other) = other.downcast_ref::<T>() else {
            return false;
        };

        self.eq(other)
    }
}

#[derive(Debug)]
pub struct Interned<T: Internable>(u64, PhantomData<fn() -> T>);

impl<T: Internable> Clone for Interned<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Internable> Copy for Interned<T> {}

/// A hasher that just uses the key as the hash.
#[derive(Default)]
struct IdentityHasher {
    hash: u64,
}

impl Hasher for IdentityHasher {
    fn write(&mut self, bytes: &[u8]) {
        // Expect exactly 8 bytes for u64 keys
        assert_eq!(bytes.len(), 8);
        self.hash = u64::from_ne_bytes(bytes.try_into().unwrap());
    }

    fn write_u64(&mut self, i: u64) {
        self.hash = i;
    }

    fn finish(&self) -> u64 {
        self.hash
    }
}

type IdentityHasherBuilder = BuildHasherDefault<IdentityHasher>;

pub struct Global {
    interned: HashMap<u64, Box<dyn Internable>, IdentityHasherBuilder>,
}

impl Global {
    /// Creates a new 'static `Global` by leaking it. Recomended for applications that compile entire files, and should hold one `Global` the entire time.
    /// Should only be called once during an entire program. For programs that need multiple `Global`s in a program, use [`Global::create_heap`].
    ///
    /// # Panics
    /// If `debug_assertions` is enabled, an extra check will be added.
    /// If this function is called more than once, that function will panic.
    pub fn new() -> &'static mut Self {
        #[cfg(debug_assertions)]
        {
            use std::sync::atomic::{AtomicBool, Ordering};

            static USED: AtomicBool = AtomicBool::new(false);
            assert!(
                !USED.swap(true, Ordering::AcqRel),
                "`Global::new` should only be called once!"
            );
        }

        Box::leak(Self::create_heap())
    }

    /// Creates a new `Global` on the heap. This `Global` is not 'static, in contrast to the `Global` created by [`Global::new`].
    /// If your program will only use one `Global`, and for the entire lifetime, use [`Global::new`].
    pub fn create_heap() -> Box<Self> {
        Box::new(Self {
            interned: HashMap::with_hasher(IdentityHasherBuilder::new()),
        })
    }

    /// Interns a value into the global map.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern<T: Internable>(&mut self, val: T) -> Interned<T> {
        let mut hasher = AHasher::default();
        val.type_id().hash(&mut hasher);
        val.dyn_hash(&mut hasher);
        let hash = hasher.finish();

        if let Some(interned) = self.interned.get(&hash) {
            assert!(interned.dyn_eq(&val), "Hash collision!");
        }
        self.interned.insert(hash, Box::new(val));
        Interned(hash, PhantomData)
    }

    /// Gets a reference to the interned value represented by `interned`.
    #[allow(clippy::missing_panics_doc)]
    pub fn get_interned<T: Internable>(&self, interned: Interned<T>) -> &T {
        <dyn Any>::downcast_ref::<T>(&**self.interned.get(&interned.0).unwrap()).unwrap()
    }
}
