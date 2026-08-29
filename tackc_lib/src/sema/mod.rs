//! Semantic analysis for tackc. See `docs/architecture.md` for more.

use std::{iter::Copied, ops::Deref, slice::Iter};

use thin_vec::ThinVec;

use crate::utils::intern::Interned;
use crate::{global::Global, utils::UnwrapExt};

pub mod module_analyzer;

/// A path without error components or AST/span information.
/// Unlike [`AstPath`](crate::frontend::ast::AstPath), [`LogicalPath`] doesn't require that paths be non-empty!
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct LogicalPath {
    components: ThinVec<Interned<str>>,
}

impl LogicalPath {
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
}

impl<'a> IntoIterator for &'a LogicalPath {
    type IntoIter = Copied<Iter<'a, Self::Item>>;
    type Item = Interned<str>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.iter().copied()
    }
}

impl Deref for LogicalPath {
    type Target = [Interned<str>];

    fn deref(&self) -> &Self::Target {
        &self.components
    }
}
