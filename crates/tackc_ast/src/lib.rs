//! Tack's AST representation

mod expr;
use std::{collections::HashMap, fmt::Write, hash::Hash};

use parking_lot::RwLock;
use serde::{Deserialize, Serialize};

pub use expr::*;

mod stmt;
pub use stmt::*;

mod block;
pub use block::*;

mod item;
pub use item::*;

mod prog;
pub use prog::*;

mod prim;
pub use prim::*;

mod util;
use tackc_global::{Global, Interned};
use tackc_span::Span;
use tackc_utils::hash::IdentityHasherBuilder;
pub use util::*;

/// Errors in parsing
pub mod error;

/// An index types for Node IDs.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct NodeId {
    /// An ID for the node itself
    pub node: u64,
    /// An ID for the file the node was found in
    pub file: u64,
}

/// Representation of a symbol in the file. This contains a [`Span`], and an [`Interned<str>`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Symbol {
    /// Span of the symbol
    pub span: Span,
    /// Value of the symbol, interned
    pub inner: Interned<str>,
}

impl Symbol {
    /// Create a new symbol
    pub const fn new(span: Span, ident: Interned<str>) -> Self {
        Self { span, inner: ident }
    }

    /// Display the symbol
    pub fn display<'a>(&self, global: &'a Global) -> &'a str {
        self.inner.display(global)
    }
}

/// Tack's representation of a binding, e.g. variables, modules, functions
#[derive(Debug, Serialize, Deserialize)]
pub struct Binding {
    /// The symbol of the identifier defining this binding
    pub symbol: Symbol,
    /// The AST node of the expression representing the type of this binding
    pub ty_annotation: Option<NodeId>,
    /// The mutable part of this binding.
    pub mutable: RwLock<MutBinding>,
}

impl Binding {
    pub fn new(symbol: Symbol) -> Self {
        Self::with_ty(symbol, None)
    }

    pub fn with_ty(symbol: Symbol, ty_annotation: Option<NodeId>) -> Self {
        Self {
            symbol,
            ty_annotation,
            mutable: RwLock::default(),
        }
    }

    pub fn display(&self, global: &Global) -> String {
        let ident = self.symbol.display(global);
        let fields = {
            let mut f = String::from("{");
            for (binding, _) in self.mutable.read().fields.values() {
                _ = write!(
                    f,
                    "\n    {}",
                    binding.get(global).display(global).replace('\n', "\n    "),
                );
            }
            _ = write!(f, "\n}}");
            f
        };
        format!("{ident} {fields}")
    }

    pub fn add_field(
        &self,
        name: Interned<str>,
        binding: Interned<Self>,
    ) -> Option<(Interned<Self>, bool)> {
        self.mutable.write().fields.insert(name, (binding, true))
    }
}

impl PartialEq for Binding {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}

impl Eq for Binding {}

impl Hash for Binding {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
    }
}

/// The mutable part of tack's representation of a binding. See [`Binding`] for more.
#[derive(Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct MutBinding {
    pub fields: HashMap<Interned<str>, (Interned<Binding>, bool), IdentityHasherBuilder>,
}

/// Equivilent to [`Option<T>`], but adds an [`Err`] variant. Used to represent something that may or may not be there, but also could be an error.
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MaybeError<T> {
    /// A value of type T
    Some(T),
    /// No value
    None,
    /// An error
    Err,
}

impl<T> MaybeError<T> {
    /// Unwraps the inner `Some` value, panicing on `None` or `Err`.
    ///
    /// # Panics
    /// This function will panic if `self` is `None` or `Err`.
    pub fn unwrap(self) -> T {
        match self {
            Self::Some(val) => val,
            Self::None => panic!("Maybe::unwrap called on a `None` value!"),
            Self::Err => panic!("Maybe::unwrap called on a `Err` value!"),
        }
    }

    /// Unwraps the inner `Some` value, returning [`T::default`](Default::default) on `None` or `Err`.
    pub fn unwrap_or_default(self) -> T
    where
        T: Default,
    {
        match self {
            Self::Some(val) => val,
            _ => T::default(),
        }
    }

    /// Returns true if `self` is [`Some`](MaybeError::Some)
    pub const fn is_some(&self) -> bool {
        matches!(*self, Self::Some(_))
    }

    /// Returns true if `self` is [`None`](MaybeError::None)
    pub const fn is_none(&self) -> bool {
        matches!(*self, Self::None)
    }

    /// Returns true if `self` is [`Err`](MaybeError::Err)
    pub const fn is_err(&self) -> bool {
        matches!(*self, Self::Err)
    }

    /// Converts `self` to an [`Option<T>`] by turning [`MaybeError::Some`] into [`Option::Some`], and turning [`MaybeError::None`] and [`MaybeError::Err`] into [`Option::None`].
    pub fn to_option(self) -> Option<T> {
        match self {
            Self::Some(val) => Some(val),
            _ => None,
        }
    }

    /// Maps `MaybeError<T>` to `MaybeError<U>` using `op` on the value of [`MaybeError::Some`].
    pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> MaybeError<U> {
        match self {
            Self::Some(val) => MaybeError::Some(op(val)),
            Self::None => MaybeError::None,
            Self::Err => MaybeError::Err,
        }
    }

    /// Converts [`&MaybeError<T>`](MaybeError<T>) to [`MaybeError<&T>`].
    pub const fn as_ref(&self) -> MaybeError<&T> {
        match self {
            Self::Some(val) => MaybeError::Some(val),
            Self::None => MaybeError::None,
            Self::Err => MaybeError::Err,
        }
    }

    /// Returns an iterator over the contained value, or an empty iterator if there is no contained value.
    pub fn iter(&self) -> std::option::IntoIter<&T> {
        match self {
            Self::Some(val) => Some(val).into_iter(),
            _ => None.into_iter(),
        }
    }

    /// Returns an iterator over the contained value, or an empty iterator if there is no contained value.
    pub fn iter_mut(&mut self) -> std::option::IntoIter<&mut T> {
        match self {
            Self::Some(val) => Some(val).into_iter(),
            _ => None.into_iter(),
        }
    }
}

impl<T> IntoIterator for MaybeError<T> {
    type IntoIter = std::option::IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        self.to_option().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a MaybeError<T> {
    type IntoIter = std::option::IntoIter<&'a T>;
    type Item = &'a T;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut MaybeError<T> {
    type IntoIter = std::option::IntoIter<&'a mut T>;
    type Item = &'a mut T;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}
