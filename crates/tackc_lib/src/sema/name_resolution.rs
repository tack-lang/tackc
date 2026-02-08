use crate::sema::LogicalPath;

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Binding {
    pub path: LogicalPath,
}
