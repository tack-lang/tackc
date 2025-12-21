//! The crate containing [`Global`], tackc's global context.

use std::{
    any::Any,
    fmt::Debug,
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::NonZeroU64,
};

use bumpalo::Bump;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use tackc_hash::NonZeroHasher;
use tackc_utils::hash::IdentityHasherBuilder;

/// A trait representing values that are able to be interned by [`Global`].
pub trait Internable: Any + Debug {
    /// Hash the value using the given hasher.
    fn dyn_hash(&self, hasher: &mut dyn Hasher);
    /// Check if the value is equal to `other`.
    fn dyn_eq(&self, other: &dyn Any) -> bool;
}

impl<T: Any + Hash + PartialEq + Debug> Internable for T {
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

/// A token type used for
#[derive(Serialize, Deserialize)]
#[repr(transparent)]
pub struct Interned<T: ?Sized>(NonZeroU64, PhantomData<fn() -> T>);

impl<T: ?Sized> Interned<T> {
    pub const fn inner(self) -> NonZeroU64 {
        self.0
    }
}

impl<T: ?Sized> Hash for Interned<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.0.get());
    }
}

impl<T: ?Sized> PartialEq for Interned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: ?Sized> Eq for Interned<T> {}

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

impl<T: Internable> Interned<T> {
    /// Returns a reference to the interned value.
    ///
    /// # Panics
    /// This function will panic if the global given was not the global used to create this interned value.
    pub fn get(self, global: &Global) -> &T {
        global.get_interned(self)
    }
}

impl Interned<str> {
    /// Returns a reference to the interned string.
    ///
    /// # Panics
    /// This function will panic if the global given was not the global used to create this interned string.
    pub fn get(self, global: &Global) -> &str {
        global.get_interned_str(self)
    }

    /// Returns a reference to the string representation of this interned string.
    ///
    /// # Panics
    /// This function will panic if the global given was not the global used to create this interned string.
    pub fn display(self, global: &Global) -> &str {
        self.get(global)
    }
}

/// tackc's global context.
#[derive(Debug)]
pub struct Global {
    arena: Bump,
    interned: DashMap<NonZeroU64, &'static dyn Internable, IdentityHasherBuilder>,
    interned_strs: DashMap<NonZeroU64, &'static str, IdentityHasherBuilder>,
}

impl Global {
    pub const fn get_hasher() -> NonZeroHasher {
        NonZeroHasher::default()
    }

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
        hash: NonZeroU64,
        map: &DashMap<NonZeroU64, &'static T, IdentityHasherBuilder>,
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
        let mut hasher = Self::get_hasher();
        val.type_id().hash(&mut hasher);
        val.dyn_hash(&mut hasher);
        let hash = hasher.finish_non_zero();

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
    pub fn get_interned<T: 'static>(&self, interned: Interned<T>) -> &T {
        <dyn Any>::downcast_ref::<T>(&**self.interned.get(&interned.0).expect("Wrong Global used!"))
            .expect("Hash collision!")
    }

    /// Interns a string value into the global map.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern_str<S: AsRef<str>>(&self, val: S) -> Interned<str> {
        let mut hasher = Self::get_hasher();
        val.as_ref().hash(&mut hasher);
        let hash = hasher.finish_non_zero();

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
            .expect("Wrong Global used!")
    }

    /// Allocates a value within the arena of this [`Global`] without interning it.
    pub fn alloc<T>(&self, val: T) -> &mut T {
        self.arena.alloc(val)
    }
}
