use std::{
    any::Any,
    collections::HashMap,
    hash::{Hash, Hasher},
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

pub struct Global {
    interned: HashMap<u64, Box<dyn Internable>>,
}

impl Global {
    pub fn new() -> &'static mut Self {
        Box::leak(Box::new(Self {
            interned: HashMap::new(),
        }))
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
