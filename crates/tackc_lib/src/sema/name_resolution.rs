//! Name resolution in tackc.

use crate::sema::LogicalPath;

/// A binding to a variable in tackc.
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Binding {
    /// The path to this binding.
    pub path: LogicalPath,
}
