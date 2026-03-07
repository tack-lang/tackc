//! Module for semantic analysis.

use std::{
    cmp::Ordering,
    fmt::{self, Write},
    hash::Hash,
    ops::Deref,
};

use parking_lot::Mutex;
use rustc_hash::FxHashMap;
use thin_vec::ThinVec;

use crate::{
    ast::{AstPath, Item, NodeId},
    file::FileId,
    global::{Global, Interned},
    span::Span,
};

pub mod module_resolution;
pub use module_resolution::resolve_mods;
pub mod global_resolution;
pub use global_resolution::resolve_globals;
pub mod import_resolution;
pub use import_resolution::resolve_imports;

/// A struct for modules, represented in a logical form, instead of a raw AST form.
#[derive(Debug)]
pub struct LogicalModule<'src> {
    /// The items in this module.
    pub items: ThinVec<Option<&'src Item<'src>>>,
    /// The spans for each node in this module.
    pub spans: Box<FxHashMap<NodeId, Span>>,
    /// Whether or not this module is exported.
    pub exported: bool,
    /// Whether or not a module duplication error occured.
    pub duplicated: bool,
    /// The file that holds this module. In the case of a module duplication error, this is not 100% accurate.
    pub file: Option<FileId>,
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
            file: None,
            duplicated: false,
        }
    }

    fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let path = self.path.display(global);
        let mut items = String::new();

        for item in &self.items {
            let Some(item) = item else { continue };
            _ = writeln!(items, "{}", item.display(global));
        }
        items.truncate(items.len().saturating_sub(1));
        let newline = if items.is_empty() { "" } else { "\n" };

        format!("{exp}mod {path}:{newline}{items}")
    }
}

/// A struct representing an immutable path in Tack.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[must_use]
pub struct LogicalPath {
    comps: Interned<[Interned<str>]>,
}

impl LogicalPath {
    /// Creates a new logical path using a list of components, and a [`Global`].
    pub fn new(comps: &[Interned<str>], global: &Global) -> Self {
        Self {
            comps: global.intern_slice_copy(comps),
        }
    }

    /// Displays the path.
    pub fn display(&self, global: &Global) -> String {
        let mut str = String::new();

        for comp in self.comps.get(global) {
            _ = write!(str, "{}.", comp.display(global));
        }

        str.truncate(str.len().saturating_sub(1)); // Remove final `.`
        str
    }

    /// Creates a version of this path that contains a [`Global`]. More traits are implemented for this struct.
    pub const fn with_global<'a>(&self, global: &'a Global) -> LogicalPathGlobal<'a> {
        LogicalPathGlobal(*self, global)
    }

    /// Falliably converts an [`AstPath`] to a [`LogicalPath`] using a [`Global`].
    pub fn try_from(ast_path: &AstPath, global: &Global) -> Option<Self> {
        let mut comps = Vec::new();
        for comp in &ast_path.components {
            let Some(comp) = comp else { return None };
            comps.push(comp.get(global).0);
        }
        Some(Self::new(&comps, global))
    }
}

/// A version of [`LogicalPath`] that contains a [`Global`].
#[derive(Copy, Clone)]
#[must_use]
pub struct LogicalPathGlobal<'a>(LogicalPath, &'a Global);

impl LogicalPathGlobal<'_> {
    /// Gets the [`LogicalPath`] on the inside of this [`LogicalPathGlobal`].
    pub const fn inner(self) -> LogicalPath {
        self.0
    }

    /// Pushes a symbol to the path.
    pub fn push(self, symbol: Interned<str>) -> Self {
        let mut vec = self.0.comps.get(self.1).to_vec();
        vec.push(symbol);
        LogicalPath::new(&vec, self.1).with_global(self.1)
    }

    /// Takes the last symbol off of the path.
    pub fn pop(self) -> (Option<Interned<str>>, Self) {
        let mut vec = self.0.comps.get(self.1).to_vec();
        let sym = vec.pop();
        (sym, LogicalPath::new(&vec, self.1).with_global(self.1))
    }

    /// Returns the length of the path.
    pub fn len(&self) -> usize {
        self.0.comps.get(self.1).len()
    }

    /// Returns whether the path is empty or not.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl fmt::Display for LogicalPathGlobal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0.display(self.1))
    }
}

impl PartialEq for LogicalPathGlobal<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for LogicalPathGlobal<'_> {}

impl Hash for LogicalPathGlobal<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl PartialOrd for LogicalPathGlobal<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LogicalPathGlobal<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        let mut iter = self.0.comps.get(self.1).iter();
        let mut other_iter = other.0.comps.get(other.1).iter();

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

impl Deref for LogicalPathGlobal<'_> {
    type Target = [Interned<str>];

    fn deref(&self) -> &Self::Target {
        self.0.comps.get(self.1)
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
        vec.sort_by_key(|(path, _)| path.with_global(global));
        for (_, module) in vec {
            _ = writeln!(str, "{}\n", module.display(global));
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
#[derive(Debug)]
pub struct Binding {
    /// The kind of binding this is.
    pub kind: Mutex<BindingKind>,
    /// The name of this binding.
    pub name: Interned<str>,
    /// Whether this binding is exported or not.
    pub exported: bool,
    /// The index of this binding.
    pub idx: u64,
}

impl PartialEq for Binding {
    #[rustfmt::skip] // rustfmt forces this to be one line
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
            && self.name == other.name
            && self.exported == other.exported
    }
}

impl Eq for Binding {}

impl Hash for Binding {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.exported.hash(state);
        self.idx.hash(state);
    }
}

/// The kind of binding.
#[derive(Debug, Hash, PartialEq, Eq)]
pub enum BindingKind {
    /// An `imp` binding.
    Import(ImportBinding),
    /// A `const` binding.
    Const,
    /// A `let` binding.
    Let,
    /// A `func` binding.
    Func,
}

/// The kind of import binding this is.
#[derive(Debug, Hash, PartialEq, Eq)]
pub enum ImportBinding {
    /// Points to a path. Used before import resolution.
    Path(LogicalPath, Span),
    /// Signifies that there was an error during import resolution.
    Error,
    /// Points to a binding. Used after import resolution.
    Binding(Interned<Binding>),
}

impl ImportBinding {
    /// Displays this import binding kind, either as a path or a binding.
    pub fn display(&self, global: &Global) -> String {
        match self {
            Self::Path(path, _) => path.display(global),
            Self::Error => "<ERROR>".to_string(),
            Self::Binding(binding) => binding.get(global).display(global),
        }
    }
}

impl Binding {
    /// Displays this binding.
    pub fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };

        let binding = match &*self.kind.lock() {
            BindingKind::Const => "const".to_string(),
            BindingKind::Func => "func".to_string(),
            BindingKind::Let => "let".to_string(),
            BindingKind::Import(path) => format!("imp {}", path.display(global)),
        };

        format!("{exp}{binding}")
    }
}

/// A list of globals, indexed by their paths.
#[derive(Default)]
pub struct BindingList {
    /// The list of globals.
    pub map: FxHashMap<LogicalPath, Interned<Binding>>,
    /// The binding index.
    pub idx: u64,
}

impl BindingList {
    /// Displays this binding list.
    pub fn display(&self, global: &Global) -> String {
        let mut str = String::new();
        for (path, binding) in &self.map {
            let path = path.display(global);
            let binding = binding.get(global).display(global);
            _ = writeln!(str, "{path}: {binding}");
        }

        str.truncate(str.len().saturating_sub(1));
        str
    }
}
