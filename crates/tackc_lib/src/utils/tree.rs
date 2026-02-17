//! A system for displaying trees as text.

use std::borrow::Cow;
use std::fmt::Write;

use crate::global::Global;

/// The trait for an item of a tree.
pub trait TreeItem {
    /// Returns the name of this tree item.
    fn name<'a>(&'a self, global: &'a Global) -> Cow<'a, str>;
    /// Returns the children of this tree item.
    fn children(&self) -> Vec<&'_ dyn TreeItem>;
}

/// An extension trait for displaying tree items.
pub trait TreeItemExt: TreeItem {
    /// Displays a tree item.
    fn display(&self, global: &Global) -> String {
        render(self, global)
    }
}

impl<T: TreeItem> TreeItemExt for T {}

/// Displays a tree item.
pub fn render<T: TreeItem + ?Sized>(tree: &T, global: &Global) -> String {
    let name = tree.name(global);

    let children = tree.children();
    let mut i = 0;
    let len = children.len();

    let mut content = String::new();

    for child in children {
        i += 1;

        let module_display = render(child, global);

        if i == len {
            _ = write!(content, "\n+-- {}", module_display.replace('\n', "\n    "));
        } else {
            _ = write!(content, "\n+-- {}", module_display.replace('\n', "\n|   "));
        }
    }

    format!("{name}{content}")
}
