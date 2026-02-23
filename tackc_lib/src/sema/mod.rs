//! Module for semantic analysis.

use std::{cmp::Ordering, fmt::Write, ops::Deref};

use rustc_hash::FxHashMap;
use thin_vec::ThinVec;

use crate::{
    ast::{AstPath, Item, NodeId},
    global::{Global, Interned},
    span::Span,
};

pub mod module_resolution;
pub use module_resolution::resolve_mods;
pub mod global_resolution;
pub use global_resolution::resolve_globals;

/// A struct for modules, represented in a logical form, instead of a raw AST form.
#[derive(Debug)]
pub struct LogicalModule<'src> {
    /// The items in this module.
    pub items: ThinVec<Option<&'src Item<'src>>>,
    /// The spans for each node in this module.
    pub spans: Box<FxHashMap<NodeId, Span>>,
    /// Whether or not this module is exported.
    pub exported: bool,
    /// The path of this module.
    pub path: LogicalPath,
}

impl LogicalModule<'_> {
    fn new(path: LogicalPath) -> Self {
        Self {
            items: ThinVec::new(),
            spans: Box::new(FxHashMap::default()),
            exported: true,
            path,
        }
    }
}

/// A struct representing a path in Tack.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct LogicalPath {
    comps: ThinVec<Interned<str>>,
}

impl LogicalPath {
    /// Pushes a symbol to the path.
    pub fn push(&mut self, symbol: Interned<str>) {
        self.comps.push(symbol);
    }

    /// Takes the last symbol off of the path.
    pub fn pop(&mut self) -> Option<Interned<str>> {
        self.comps.pop()
    }

    /// Returns the length of the path.
    pub fn len(&self) -> usize {
        self.comps.len()
    }

    /// Returns whether the path is empty or not.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Displays the path.
    pub fn display(&self, global: &Global) -> String {
        self.comps
            .iter()
            .map(|comp| comp.display(global))
            .collect::<Vec<_>>()
            .join(".")
    }
}

impl PartialOrd for LogicalPath {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LogicalPath {
    fn cmp(&self, other: &Self) -> Ordering {
        let mut iter = self.comps.iter();
        let mut other_iter = other.comps.iter();

        loop {
            match (iter.next(), other_iter.next()) {
                (Some(id), Some(other_id)) => {
                    if id == other_id {
                        continue;
                    }

                    break id.inner().cmp(&other_id.inner());
                }
                (None, Some(_)) => break Ordering::Less,
                (Some(_), None) => break Ordering::Greater,
                (None, None) => break Ordering::Equal,
            }
        }
    }
}

impl<const N: usize> From<[Interned<str>; N]> for LogicalPath {
    fn from(value: [Interned<str>; N]) -> Self {
        Self {
            comps: ThinVec::from(value),
        }
    }
}

impl LogicalPath {
    /// Falliably converts an [`AstPath`] to a [`LogicalPath`].
    pub fn try_from(value: &AstPath, global: &Global) -> Option<Self> {
        let mut path = Self::default();
        for comp in &value.components {
            path.push(comp.as_ref()?.get(global).0);
        }

        Some(path)
    }
}

/// A list of modules, indexed by path.
#[derive(Debug)]
pub struct ModuleList<'src> {
    /// The inner module list.
    pub mods: FxHashMap<LogicalPath, LogicalModule<'src>>,
}

impl ModuleList<'_> {
    /// Displays the module list.
    pub fn display(&self, global: &Global) -> String {
        let mut str = String::new();

        let mut vec = self.mods.iter().collect::<Vec<_>>();
        vec.sort_by_key(|tuple| tuple.0);
        for (path, module) in vec {
            _ = writeln!(
                str,
                "{}mod {}:\n{}\n",
                if module.exported { "exp " } else { "" },
                path.display(global),
                module
                    .items
                    .iter()
                    .filter_map(Option::as_ref)
                    .map(|comp| comp.display(global))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }

        str.truncate(str.len().saturating_sub(2));

        str
    }
}

impl<'src> Deref for ModuleList<'src> {
    type Target = FxHashMap<LogicalPath, LogicalModule<'src>>;

    fn deref(&self) -> &Self::Target {
        &self.mods
    }
}

/// A binding to a variable in tackc.
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Binding {
    /// The kind of binding this is.
    pub kind: BindingKind,
    /// The name of this binding.
    pub name: Interned<str>,
    /// Whether this binding is exported or not.
    pub exported: bool,
}

/// The kind of binding this is.
#[derive(Debug, Hash, PartialEq, Eq)]
pub enum BindingKind {
    /// An `imp` binding.
    Import(LogicalPath),
    /// A `const` binding.
    Const,
    /// A `func` binding.
    Func,
}

impl Binding {
    /// Displays this binding.
    pub fn display(&self, global: &Global) -> String {
        (if self.exported { "exp " } else { "" }).to_string()
            + &match &self.kind {
                BindingKind::Const => "const".to_string(),
                BindingKind::Func => "func".to_string(),
                BindingKind::Import(path) => format!("imp {}", path.display(global)),
            }
    }
}

/// A list of globals, indexed by their paths.
#[derive(Default)]
pub struct BindingList {
    /// The list of globals.
    pub map: FxHashMap<LogicalPath, Interned<Binding>>,
}

impl BindingList {
    /// Displays this binding list.
    pub fn display(&self, global: &Global) -> String {
        let mut str = String::new();
        for (path, binding) in &self.map {
            _ = writeln!(
                str,
                "{}: {}",
                path.display(global),
                binding.get(global).display(global)
            );
        }
        str.truncate(str.len().saturating_sub(1));
        str
    }
}
