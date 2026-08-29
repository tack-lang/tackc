//! A module for an interner.

use std::{
    any::{Any, type_name},
    fmt::Debug,
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::NonZeroU64,
    slice,
};

use bumpalo::Bump;
use serde::{Deserialize, Serialize};

use crate::utils::{
    UnwrapExt,
    hash::{IdentityDashMap, NonZeroFxHasher},
};

/// A trait representing values that are able to be interned by [`Interner`].
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
    /// This function will panic if the Interner given was not the Interner used to create this interned value.
    pub fn get(self, interner: &Interner) -> &T {
        interner.get_interned(self)
    }
}

impl Interned<str> {
    /// Returns a reference to the interned string.
    ///
    /// # Panics
    /// This function will panic if the Interner given was not the Interner used to create this interned string.
    pub fn get(self, interner: &Interner) -> &str {
        interner.get_interned_str(self)
    }
}

impl<T: Hash> Interned<[T]> {
    /// Returns a reference to the interned string.
    ///
    /// # Panics
    /// This function will panic if the Interner given was not the Interner used to create this interned string.
    pub fn get(self, interner: &Interner) -> &[T] {
        interner.get_interned_slice(self)
    }
}

/// A struct for interning and deduplicating values.
#[derive(Debug, Default)]
pub struct Interner {
    arena: Bump,
    interned: IdentityDashMap<NonZeroU64, &'static dyn Internable>,
    interned_strs: IdentityDashMap<NonZeroU64, &'static str>,
    interned_slices: IdentityDashMap<NonZeroU64, (usize, *const u8)>,
}

impl Interner {
    /// Creates a new interner.
    pub fn new() -> Self {
        Self::default()
    }

    /// Gets the hasher for this interner context.
    ///
    /// This will always be a default [`NonZeroFxHasher`].
    pub const fn get_hasher() -> NonZeroFxHasher {
        NonZeroFxHasher::default()
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

    /// Interns a value into the interner map.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern<T: Internable>(&self, val: T) -> Interned<T> {
        let mut hasher = Self::get_hasher();
        type_name::<T>().hash(&mut hasher);
        val.dyn_hash(&mut hasher);
        let hash = hasher.finish_non_zero();

        if let Some(interned) = self.interned.get(&hash) {
            if interned.dyn_eq(&val) {
                return Interned(hash, PhantomData);
            }
            Self::report_collision();
        }

        let ptr: *mut dyn Internable = self.alloc(val);

        Self::intern_value(ptr, hash, &self.interned);

        Interned(hash, PhantomData)
    }

    /// Gets a reference to the interned value represented by `interned`.
    ///
    /// # Panics
    /// This function will panic if the `interned` given is from a different `Interner`, or in the event of a hash collision.
    pub fn get_interned<T: 'static>(&self, interned: Interned<T>) -> &T {
        assert!(self.interned.contains_key(&interned.0), "wrong Interner!");

        let val = self
            .interned
            .get(&interned.0)
            // Assertion made ensures `get` returns `Some`.
            .expect_unreachable(); // CHECKED(Chloe)
        let Some(res) = <dyn Any>::downcast_ref::<T>(&**val) else {
            Self::report_collision();
        };

        // Satisfy clippy
        drop(val);

        res
    }

    /// Interns a string value into the interner map.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern_str<S: AsRef<str>>(&self, val: S) -> Interned<str> {
        #[inline(never)]
        fn inner(interner: &Interner, val: &str) -> Interned<str> {
            let mut hasher = Interner::get_hasher();
            val.hash(&mut hasher);
            let hash = hasher.finish_non_zero();

            if let Some(interned) = interner.interned_strs.get(&hash) {
                if *interned == val {
                    return Interned(hash, PhantomData);
                }
                Interner::report_collision();
            }

            let ptr: *mut str = interner.alloc_str(val);

            Interner::intern_value(ptr, hash, &interner.interned_strs);

            Interned(hash, PhantomData)
        }
        inner(self, val.as_ref())
    }

    /// Gets a reference to the interned string value represented by `interned`.
    ///
    /// # Panics
    /// This function will panic if the `interned` given is from a different `Interner`.
    pub fn get_interned_str(&self, interned: Interned<str>) -> &str {
        assert!(
            self.interned_strs.contains_key(&interned.0),
            "wrong Interner!"
        );

        *self
            .interned_strs
            .get(&interned.0)
            // We asserted that the map contains the key.
            .expect_unreachable() // CHECKED(Chloe)
    }

    /// Interns a string value into the interner map by copying its elements.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern_slice_copy<T: Copy + Hash + PartialEq>(&self, val: &[T]) -> Interned<[T]> {
        self.intern_slice(val, Self::alloc_slice_copy)
    }

    /// Interns a string value into the interner map by cloning its elements.
    ///
    /// # Panics
    /// This function will only panic in the event of a hash collision.
    pub fn intern_slice_clone<T: Clone + Hash + PartialEq>(&self, val: &[T]) -> Interned<[T]> {
        self.intern_slice(val, Self::alloc_slice_clone)
    }

    fn intern_slice<'a, T: Hash + PartialEq>(
        &'a self,
        val: &[T],
        func: fn(&'a Self, &[T]) -> &'a mut [T],
    ) -> Interned<[T]> {
        let mut hasher = Self::get_hasher();
        type_name::<[T]>().hash(&mut hasher);
        val.hash(&mut hasher);
        let hash = hasher.finish_non_zero();

        if let Some(interned) = self.interned_slices.get(&hash) {
            let (len, ptr) = *interned;
            let ptr = ptr.cast::<T>();

            #[expect(unsafe_code)] // CHECKED(Chloe)
            // SAFETY:
            // When inserting into the `interned_slices` map,
            // we ensure that the pointer/length comes from a valid slice.
            let slice = unsafe { slice::from_raw_parts(ptr, len) };

            if slice == val {
                return Interned(hash, PhantomData);
            }
            Self::report_collision();
        }

        let ptr = func(self, val);

        self.interned_slices
            .insert(hash, (ptr.len(), ptr.as_ptr().cast::<u8>()));

        Interned(hash, PhantomData)
    }

    /// Gets a reference to the interned slice value represented by `interned`.
    ///
    /// # Panics
    /// This function will panic if the `interned` given is from a different `Interner`.
    pub fn get_interned_slice<T: Hash>(&self, interned: Interned<[T]>) -> &[T] {
        assert!(
            self.interned_slices.contains_key(&interned.0),
            "wrong Interner!"
        );

        let (len, ptr) = *self
            .interned_slices
            .get(&interned.0)
            // This was just checked.
            .expect_unreachable(); // CHECKED(Chloe)
        let ptr = ptr.cast::<T>();

        #[expect(unsafe_code)] // CHECKED(Chloe)
        // SAFETY:
        // When inserting into the `interned_slices` map,
        // we ensure that the pointer/length comes from a valid slice.
        unsafe {
            slice::from_raw_parts(ptr, len)
        }
    }

    #[inline(never)]
    #[cold]
    fn report_collision() -> ! {
        // Hash collisions should be treated as impossible.
        panic!("Hash collision!"); // CHECKED(Chloe)
    }

    /// Allocates a value within the arena of this [`Interner`] without interning it.
    pub fn alloc<T>(&self, val: T) -> &mut T {
        self.arena.alloc(val)
    }

    /// Allocates a string within the arena of this [`Interner`] without interning it.
    #[inline(never)] // `Bump::alloc_str` is huge, so don't inline.
    pub fn alloc_str(&self, src: &str) -> &mut str {
        self.arena.alloc_str(src)
    }

    /// Allocates a slice where `T: Copy` within the arena of this [`Interner`] without interning it.
    pub fn alloc_slice_copy<T: Copy>(&self, src: &[T]) -> &mut [T] {
        self.arena.alloc_slice_copy(src)
    }

    /// Allocates a slice where `T: Clone` within the arena of this [`Interner`] without interning it.
    pub fn alloc_slice_clone<T: Clone>(&self, src: &[T]) -> &mut [T] {
        self.arena.alloc_slice_clone(src)
    }
}

#[test]
fn intern_test() {
    const FIBB: &[i32] = &[1, 1, 2, 3, 5, 8];
    let strings: &[String] = &["foo".to_string(), "bar".to_string(), "baz".to_string()];

    let interner = Interner::new();
    let five = interner.intern(5);
    let four = interner.intern(4);
    let two = interner.intern(2);
    let foo = interner.intern_str("foo");
    let fibb = interner.intern_slice_copy(&[1, 1, 2, 3, 5, 8]);
    let foo2 = interner.intern_str("foo");
    let strings_interned = interner.intern_slice_clone(strings);

    assert_eq!(*five.get(&interner), 5);
    assert_eq!(*two.get(&interner), 2);
    assert_eq!(*five.get(&interner), 5);
    assert_eq!(*four.get(&interner), 4);
    assert_eq!(foo.get(&interner), "foo");
    assert_eq!(fibb.get(&interner), FIBB);
    assert_eq!(foo, foo2);
    assert_ne!(five, four);
    assert_eq!(strings_interned.get(&interner), strings);
}
