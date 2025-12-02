use std::{
    any::Any,
    fmt::Debug,
    hash::{BuildHasherDefault, Hash, Hasher},
    marker::PhantomData,
};

use bumpalo::Bump;
use dashmap::DashMap;
use proptest_derive::Arbitrary;
use rustc_hash::FxHasher;

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

#[derive(PartialEq, Eq, Hash, Arbitrary)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Interned<T: ?Sized>(u64, PhantomData<fn() -> T>);

impl<T: Internable> Interned<T> {
    /// --- THIS FUNCTION SHOULD ONLY BE USED IN TESTING! --- <br>
    /// This function creates an interned version of `val`, without entering it into a `Global`.
    pub fn get_interned(val: &T) -> Self {
        let mut hasher = FxHasher::default();
        val.type_id().hash(&mut hasher);
        val.dyn_hash(&mut hasher);
        Interned(hasher.finish(), PhantomData)
    }
}

impl Interned<str> {
    /// --- THIS FUNCTION SHOULD ONLY BE USED IN TESTING! --- <br>
    /// This function creates an interned version of `val`, without entering it into a `Global`.
    pub fn get_interned(val: &str) -> Self {
        let mut hasher = FxHasher::default();
        val.hash(&mut hasher);
        Interned(hasher.finish(), PhantomData)
    }
}

impl<T: ?Sized> Debug for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Interned").field(&self.0).finish()
    }
}

impl<T: ?Sized> Clone for Interned<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for Interned<T> {}

impl Interned<str> {
    /// Returns a reference to the string representation of this interned string.
    ///
    /// # Panics
    /// This function will panic if the global given was not the global used to create this interned string.
    pub fn display<'a>(&self, global: &'a Global) -> &'a str {
        global.get_interned_str(*self)
    }
}

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
    arena: Bump,
    interned: DashMap<u64, &'static dyn Internable, IdentityHasherBuilder>,
    interned_strs: DashMap<u64, &'static str, IdentityHasherBuilder>,
}

impl Global {
    /// Creates a new 'static `Global` by leaking it. Recomended for applications that compile entire files, and should hold one `Global` the entire time.
    /// Should only be called once during an entire program. For programs that need multiple `Global`s in a program, use [`Global::create_heap`].
    ///
    /// # Panics
    /// If `debug_assertions` is enabled, an extra check will be added.
    /// If this function is called more than once, that function will panic.
    pub fn new() -> &'static Self {
        #[cfg(debug_assertions)]
        {
            use std::sync::atomic::{AtomicBool, Ordering};

            static USED: AtomicBool = AtomicBool::new(false);
            assert!(
                !USED.swap(true, Ordering::AcqRel),
                "`Global::new` should only be called once! If multiple `Global`s are needed, use `Global::create_heap()`"
            );
        }

        Box::leak(Self::create_heap())
    }

    /// Creates a new `Global` on the heap. This `Global` is not 'static, in contrast to the `Global` created by [`Global::new`].
    /// If your program will only use one `Global`, and for the entire lifetime, use [`Global::new`].
    pub fn create_heap() -> Box<Self> {
        Box::new(Self {
            arena: Bump::new(),
            interned: DashMap::with_hasher(IdentityHasherBuilder::new()),
            interned_strs: DashMap::with_hasher(IdentityHasherBuilder::new()),
        })
    }

    fn intern_value<T: ?Sized>(
        val: *const T,
        hash: u64,
        map: &DashMap<u64, &'static T, IdentityHasherBuilder>,
    ) {
        // SAFETY: The value is allocated in the arena and lives as long as `self`.
        #[allow(unsafe_code)]
        let static_ref: &'static T = unsafe { &*val };

        map.insert(hash, static_ref);
    }

    /// Interns a value into the global map.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern<T: Internable>(&self, val: T) -> Interned<T> {
        let mut hasher = FxHasher::default();
        val.type_id().hash(&mut hasher);
        val.dyn_hash(&mut hasher);
        let hash = hasher.finish();

        if let Some(interned) = self.interned.get(&hash) {
            assert!(interned.dyn_eq(&val), "Hash collision!");
        }

        let ptr: *mut dyn Internable = self.arena.alloc(val);

        Self::intern_value(ptr, hash, &self.interned);

        Interned(hash, PhantomData)
    }

    /// Gets a reference to the interned value represented by `interned`.
    ///
    /// # Panics
    /// This function will panic if the `interned` given is from a different `Global`, or in the event of a hash collision.
    #[allow(clippy::missing_panics_doc)]
    pub fn get_interned<T: 'static>(&self, interned: Interned<T>) -> &T {
        <dyn Any>::downcast_ref::<T>(
            &**self
                .interned
                .get(&interned.0)
                .unwrap_or_else(|| panic!("Wrong Global used!")),
        )
        .unwrap_or_else(|| panic!("Hash collision!"))
    }

    /// Interns a string value into the global map.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern_str<S: AsRef<str>>(&self, val: S) -> Interned<str> {
        let mut hasher = FxHasher::default();
        val.as_ref().hash(&mut hasher);
        let hash = hasher.finish();

        if let Some(interned) = self.interned_strs.get(&hash) {
            assert!(*interned == val.as_ref(), "Hash collision!");
        }

        let ptr: *mut str = self.arena.alloc_str(val.as_ref());

        Self::intern_value(ptr, hash, &self.interned_strs);

        Interned(hash, PhantomData)
    }

    /// Gets a reference to the interned string value represented by `interned`.
    ///
    /// # Panics
    /// This function will panic if the `interned` given is from a different `Global`.
    pub fn get_interned_str(&self, interned: Interned<str>) -> &str {
        *self
            .interned_strs
            .get(&interned.0)
            .unwrap_or_else(|| panic!("Wrong Global used!"))
    }

    pub fn alloc<T>(&self, val: T) -> &mut T {
        self.arena.alloc(val)
    }
}
