use std::{
    array,
    cmp::Ordering,
    collections::HashMap,
    hash::{BuildHasher, Hash},
    path::PathBuf,
};

use rand::random_range;
pub use tackc_macros_impl::*;

pub trait Random {
    fn random() -> Self;
}

macro_rules! random_impl_rand_random {
    ($ty:ty) => {
        impl Random for $ty {
            fn random() -> Self {
                rand::random()
            }
        }
    };
}

random_impl_rand_random!(u8);
random_impl_rand_random!(u16);
random_impl_rand_random!(u32);
random_impl_rand_random!(u64);
random_impl_rand_random!(u128);
random_impl_rand_random!(i8);
random_impl_rand_random!(i16);
random_impl_rand_random!(i32);
random_impl_rand_random!(i64);
random_impl_rand_random!(i128);
random_impl_rand_random!(char);
random_impl_rand_random!(bool);

impl<T: Random, const N: usize> Random for [T; N] {
    fn random() -> Self {
        array::from_fn(|_| T::random())
    }
}

macro_rules! random_impl_tuple {
    ($($ident:ident)*) => {
        impl<$($ident: Random),*> Random for ($($ident),*,) {
            fn random() -> Self {
                ($(<$ident as Random>::random()),*,)
            }
        }
    };
}

macro_rules! random_impl_tuples {
    ($head:ident $($ident:ident)*) => {
        random_impl_tuple!($head $($ident)*);
        random_impl_tuples!($($ident)*);
    };
    () => {

    };
}

// 12-arity tuples, larger is excessive
random_impl_tuples!(A B C D E F G H I J K L);

impl<T: Random> Random for Vec<T> {
    fn random() -> Self {
        let mut vec = Vec::new();
        for _ in 0..u16::random() {
            vec.push(T::random());
        }
        vec
    }
}

impl<K: Random + Eq + Hash, V: Random, S: Default + BuildHasher> Random for HashMap<K, V, S> {
    fn random() -> Self {
        let mut map = HashMap::with_hasher(S::default());
        for _ in 0..u16::random() {
            map.insert(K::random(), V::random());
        }
        map
    }
}

impl Random for String {
    fn random() -> Self {
        let mut str = String::new();
        for _ in 0..u16::random() {
            str.push(char::random());
        }
        str
    }
}

impl Random for PathBuf {
    fn random() -> Self {
        Self::from(String::random())
    }
}

impl Random for Ordering {
    fn random() -> Self {
        match random_range::<u8, _>(0..3) {
            0 => Self::Less,
            1 => Self::Greater,
            2 => Self::Equal,
            _ => unreachable!(),
        }
    }
}
