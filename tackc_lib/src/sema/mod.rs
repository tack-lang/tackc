//! Semantic analysis for tackc. See `docs/architecture.md` for more.

use std::{iter::Copied, ops::Deref, slice::Iter};

use thin_vec::ThinVec;

use crate::{
    global::Global,
    utils::{UnwrapExt, intern::Interned},
};

pub mod module_analyzer;
pub mod namespace_analyzer;

/// A path without error components or AST/span information.
/// Unlike [`AstPath`](crate::frontend::ast::AstPath), [`LogicalPath`] doesn't require that paths be non-empty!
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct LogicalPath {
    components: ThinVec<Interned<str>>,
}

impl LogicalPath {
    const EMPTY: Self = Self::new(ThinVec::new());

    /// Creates a new [`LogicalPath`].
    pub const fn new(components: ThinVec<Interned<str>>) -> Self {
        Self { components }
    }

    /// Pushes a new component to this path.
    pub fn push(&mut self, component: Interned<str>) {
        self.components.push(component);
    }

    /// Returns the first component of this path, if there.
    pub fn first(&self) -> Option<Interned<str>> {
        self.components.first().copied()
    }

    /// Returns the last component of this path, if there.
    pub fn last(&self) -> Option<Interned<str>> {
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
            String::from(
                self.components
                    .first()
                    // If self.components was empty, the above if branch would have been taken.
                    .expect_unreachable() // CHECKED(Chloe)
                    .get(&global.interner),
            ),
            |val, elem| val + "." + elem.get(&global.interner),
        )
    }

    /// Creates a new path with the component added to the end of it.
    #[must_use]
    pub fn join(&self, component: Interned<str>) -> Self {
        self.join_non_empty(component).into_inner()
    }

    /// Creates a new non-empty path with the component added to the end of it.
    #[must_use]
    pub fn join_non_empty(&self, component: Interned<str>) -> NonEmptyLogicalPath {
        let mut new = self.clone();
        new.push(component);
        NonEmptyLogicalPath::new(new)
            // This path was just pushed to.
            .expect_unreachable() // CHECKED(Chloe)
    }
}

impl<'a> IntoIterator for &'a LogicalPath {
    type IntoIter = Copied<Iter<'a, Self::Item>>;
    type Item = Interned<str>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.iter().copied()
    }
}

impl FromIterator<Interned<str>> for LogicalPath {
    fn from_iter<T: IntoIterator<Item = Interned<str>>>(iter: T) -> Self {
        Self::new(iter.into_iter().collect())
    }
}

impl Deref for LogicalPath {
    type Target = [Interned<str>];

    fn deref(&self) -> &Self::Target {
        &self.components
    }
}

/// A [`LogicalPath`] with the added condition that it must be non-empty.
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
    pub fn first(&self) -> Interned<str> {
        // This is an invariant.
        (**self).first().expect_unreachable() // CHECKED(Chloe)
    }

    /// Returns the last component of this path.
    pub fn last(&self) -> Interned<str> {
        // This is an invariant.
        (**self).last().expect_unreachable() // CHECKED(Chloe)
    }

    /// Returns the inner path.
    pub fn into_inner(self) -> LogicalPath {
        self.0
    }
}

impl Deref for NonEmptyLogicalPath {
    type Target = LogicalPath;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
