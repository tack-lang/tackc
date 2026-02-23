//! The crate containing [`Global`], tackc's global context.

use std::{
    any::{Any, type_name},
    fmt::Debug,
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::NonZeroU64,
};

use crate::utils::{
    UnwrapExt,
    hash::{IdentityDashMap, NonZeroFxHasher},
};
use bumpalo::Bump;
use serde::{Deserialize, Serialize};

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

/// A type that represents interned values.
#[derive(Serialize, Deserialize)]
#[repr(transparent)]
pub struct Interned<T: ?Sized>(NonZeroU64, PhantomData<fn() -> T>);

impl<T: ?Sized> Interned<T> {
    /// Gets the inner representation of this interned value.
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
    /// The arena used in [`Global::alloc_arena`]. Public to allow resetting.
    pub current_arena: Bump,

    arena: Bump,
    interned: IdentityDashMap<NonZeroU64, &'static dyn Internable>,
    interned_strs: IdentityDashMap<NonZeroU64, &'static str>,
}

impl Global {
    /// Allocates a new value using the inner arena.
    pub fn alloc_arena<T>(&self, val: T) -> &mut T {
        self.current_arena.alloc(val)
    }

    /// Gets the hasher for this global context.
    ///
    /// This will always be a default [`NonZeroFxHasher`].
    pub const fn get_hasher() -> NonZeroFxHasher {
        NonZeroFxHasher::default()
    }

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
                "`Global::new` should only be called once! If multiple `Global`s are needed, use `Global::create_heap()`"
            );
        }

        Box::leak(Self::create_heap())
    }

    /// Creates a new `Global` on the heap. This `Global` is not 'static, in contrast to the `Global` created by [`Global::new`].
    /// If your program will only use one `Global`, and for the entire lifetime, use [`Global::new`].
    pub fn create_heap() -> Box<Self> {
        Box::new(Self {
            current_arena: Bump::new(),
            arena: Bump::new(),
            interned: IdentityDashMap::default(),
            interned_strs: IdentityDashMap::default(),
        })
    }

    #[inline]
    fn intern_value<T: ?Sized>(
        val: *const T,
        hash: NonZeroU64,
        map: &IdentityDashMap<NonZeroU64, &'static T>,
    ) {
        #[expect(unsafe_code)] // CHECKED(Chloe)
        // SAFETY:
        // The value is allocated in the arena and lives as long as `self`.
        // This is safe as long as the 'static reference is only returned to callers
        // if &self is 'static.
        let static_ref: &'static T = unsafe { &*val };

        map.insert(hash, static_ref);
    }

    /// Interns a value into the global map.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern<T: Internable>(&self, val: T) -> Interned<T> {
        let mut hasher = Self::get_hasher();
        type_name::<T>().hash(&mut hasher);
        val.dyn_hash(&mut hasher);
        let hash = hasher.finish_non_zero();

        if let Some(interned) = self.interned.get(&hash) {
            assert!(interned.dyn_eq(&val), "Hash collision!");
        }

        let ptr: *mut dyn Internable = self.alloc(val);

        Self::intern_value(ptr, hash, &self.interned);

        Interned(hash, PhantomData)
    }

    /// Gets a reference to the interned value represented by `interned`.
    ///
    /// # Panics
    /// This function will panic if the `interned` given is from a different `Global`, or in the event of a hash collision.
    pub fn get_interned<T: 'static>(&self, interned: Interned<T>) -> &T {
        assert!(self.interned.contains_key(&interned.0), "wrong Global!");

        // Assertion made ensures `get` returns `Some`.
        let val = self.interned.get(&interned.0).expect_unreachable(); // CHECKED(Chloe)
        let Some(res) = <dyn Any>::downcast_ref::<T>(&**val) else {
            Self::report_collision();
        };

        // Satisfy clippy
        drop(val);

        res
    }

    /// Interns a string value into the global map.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern_str<S: AsRef<str>>(&self, val: S) -> Interned<str> {
        #[inline(never)]
        fn inner(global: &Global, val: &str) -> Interned<str> {
            let mut hasher = Global::get_hasher();
            val.hash(&mut hasher);
            let hash = hasher.finish_non_zero();

            if let Some(interned) = global.interned_strs.get(&hash)
                && *interned != val
            {
                Global::report_collision();
            }

            let ptr: *mut str = global.alloc_str(val);

            Global::intern_value(ptr, hash, &global.interned_strs);

            Interned(hash, PhantomData)
        }
        inner(self, val.as_ref())
    }

    #[inline(never)]
    #[cold]
    fn report_collision() -> ! {
        // Hash collisions should be treated as impossible.
        panic!("Hash collision!"); // CHECKED(Chloe)
    }

    /// Gets a reference to the interned string value represented by `interned`.
    ///
    /// # Panics
    /// This function will panic if the `interned` given is from a different `Global`.
    pub fn get_interned_str(&self, interned: Interned<str>) -> &str {
        assert!(
            self.interned_strs.contains_key(&interned.0),
            "wrong Global!"
        );

        // We asserted that the map contains the key.
        *self.interned_strs.get(&interned.0).expect_unreachable() // CHECKED(Chloe)
    }

    /// Allocates a value within the arena of this [`Global`] without interning it.
    pub fn alloc<T>(&self, val: T) -> &mut T {
        self.arena.alloc(val)
    }

    /// Allocates a string within the arena of this [`Global`] without interning it.
    #[inline(never)] // `Bump::alloc_str` is huge, so don't inline.
    pub fn alloc_str(&self, val: &str) -> &mut str {
        self.arena.alloc_str(val)
    }
}
