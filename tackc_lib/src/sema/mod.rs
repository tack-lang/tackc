//! Semantic analysis for tackc. See `docs/architecture.md` for more.

use std::{iter::Copied, num::NonZeroU64, ops::Deref, slice::Iter};

use derive_more::derive::From;
use thin_vec::ThinVec;

use crate::{
    global::Global,
    utils::{UnwrapExt, intern::Interned},
};

pub mod module_analyzer;
pub mod namespace_analyzer;

/// A component of a path.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy, From)]
pub enum PathComponent {
    /// An identifier.
    Identifier(Interned<str>),
    /// An index. Used in special cases where the compiler needs a path to something, but can't use an identifier.
    Idx(NonZeroU64),
}

impl PathComponent {
    /// Returns the identifier representation of this component, if applicable.
    pub const fn identifier(self) -> Option<Interned<str>> {
        match self {
            Self::Identifier(ident) => Some(ident),
            Self::Idx(_) => None,
        }
    }

    /// Returns the index representation of this component, if applicable.
    pub const fn idx(self) -> Option<NonZeroU64> {
        match self {
            Self::Identifier(_) => None,
            Self::Idx(idx) => Some(idx),
        }
    }

    /// Displays this component, either as an identifier, or as an index starting with `_`.
    pub fn display(self, global: &Global) -> String {
        match self {
            Self::Identifier(ident) => ident.get(&global.interner).to_owned(),
            Self::Idx(idx) => format!("_{idx}"),
        }
    }
}

/// A path without error components or AST/span information.
/// Unlike [`AstPath`](crate::frontend::ast::AstPath), [`LogicalPath`] doesn't require that paths be non-empty!
#[derive(Debug, Clone, Hash, Default, PartialEq, Eq)]
pub struct LogicalPath {
    /// The components of this path.
    pub components: ThinVec<PathComponent>,
}

impl LogicalPath {
    /// An empty logical path, usable in a const context.
    pub const EMPTY: Self = Self::new();

    /// Creates a new [`LogicalPath`].
    pub const fn new() -> Self {
        Self {
            components: ThinVec::new(),
        }
    }

    /// Pushes a new component to this path.
    pub fn push<T: Into<PathComponent>>(&mut self, component: T) {
        self.components.push(component.into());
    }

    /// Returns the first component of this path, if there.
    pub fn first(&self) -> Option<PathComponent> {
        self.components.first().copied()
    }

    /// Returns the last component of this path, if there.
    pub fn last(&self) -> Option<PathComponent> {
        self.components.last().copied()
    }

    /// Returns whether the path is empty or not.
    pub fn is_empty(&self) -> bool {
        self.components.is_empty()
    }

    /// Displays the path. If empty, returns an empty string.
    pub fn display(&self, global: &Global) -> String {
        if self.components.is_empty() {
            return String::new();
        }

        self.components.iter().copied().skip(1).fold(
            self.components
                .first()
                // If self.components was empty, the above if branch would have been taken.
                .expect_unreachable() // CHECKED(Chloe)
                .display(global),
            |val, elem| val + "." + &elem.display(global),
        )
    }

    /// Creates a new path with the component added to the end of it.
    #[must_use]
    pub fn join<T: Into<PathComponent>>(&self, component: T) -> Self {
        let mut path = self.clone();
        path.push(component.into());
        path
    }

    /// Creates a new non-empty path with the component added to the end of it.
    #[must_use]
    pub fn join_non_empty<T: Into<PathComponent>>(&self, component: T) -> NonEmptyLogicalPath {
        self.join(component)
            .into_non_empty()
            // The path was just joined to.
            .expect_unreachable() // CHECKED(Chloe)
    }

    /// Calls [`NonEmptyLogicalPath::new`] on this path.
    pub fn into_non_empty(&self) -> Option<NonEmptyLogicalPath> {
        NonEmptyLogicalPath::new(self.clone())
    }
}

impl<'a> IntoIterator for &'a LogicalPath {
    type IntoIter = Copied<Iter<'a, Self::Item>>;
    type Item = PathComponent;

    fn into_iter(self) -> Self::IntoIter {
        self.components.iter().copied()
    }
}

impl<I: Into<PathComponent>> FromIterator<I> for LogicalPath {
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        Self::from(iter.into_iter().map(Into::into).collect::<ThinVec<_>>())
    }
}

impl Deref for LogicalPath {
    type Target = [PathComponent];

    fn deref(&self) -> &Self::Target {
        &self.components
    }
}

impl<T: Into<PathComponent>> From<ThinVec<T>> for LogicalPath {
    fn from(value: ThinVec<T>) -> Self {
        value.into_iter().collect()
    }
}

/// A [`LogicalPath`] with the added condition that it must be non-empty.
#[derive(Debug, Clone)]
pub struct NonEmptyLogicalPath(LogicalPath);

impl NonEmptyLogicalPath {
    /// Creates a new [`NonEmptyLogicalPath`].
    pub fn new(path: LogicalPath) -> Option<Self> {
        if path.is_empty() {
            None
        } else {
            Some(Self(path))
        }
    }

    /// Returns the first component of this path.
    pub fn first(&self) -> PathComponent {
        // This is an invariant.
        (**self).first().expect_unreachable() // CHECKED(Chloe)
    }

    /// Returns the last component of this path.
    pub fn last(&self) -> PathComponent {
        // This is an invariant.
        (**self).last().expect_unreachable() // CHECKED(Chloe)
    }

    /// Returns the inner path.
    pub fn into_inner(self) -> LogicalPath {
        self.0
    }

    /// Joins a path component onto this path.
    #[must_use]
    pub fn join<T: Into<PathComponent>>(&self, component: T) -> Self {
        self.join_non_empty(component)
    }
}

impl Deref for NonEmptyLogicalPath {
    type Target = LogicalPath;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
