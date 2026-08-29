//! Turns a flat list of modules into a tree of modules.

use std::borrow::Cow;

use crate::{
    error::Diag,
    file::FileId,
    frontend::ast::{AstModule, Item},
    global::{Global, Interned},
    sema::LogicalPath,
    utils::{
        UnwrapExt,
        hash::IdentityHashMap,
        tree::{TreeItem, TreeItemExt},
    },
};

use rustc_hash::FxHashSet;
use thin_vec::thin_vec;

/// A tree of modules, using [`ModuleNode`].
pub struct ModuleTree {
    /// The inner nodes.
    pub nodes: IdentityHashMap<Interned<str>, ModuleNode>,
}

impl ModuleTree {
    /// Displays a representation of this module tree.
    pub fn display(&self, global: &Global) -> String {
        if self.nodes.is_empty() {
            return String::new();
        }

        let mut nodes = self.nodes.values().collect::<Vec<_>>();
        nodes.sort_by_key(|val| {
            val.path
                .first()
                // This is an invariant.
                .expect_unreachable() // CHECKED(Chloe)
                .display(global)
        });

        nodes.into_iter().fold(String::new(), |val, elem| {
            val + "\n" + &elem.display(global)
        })
    }

    /// Gets a module from the tree, ignoring visibillity.
    pub fn get(&self, path: &LogicalPath) -> Option<&ModuleNode> {
        if path.is_empty() {
            return None;
        }

        let (first, rest) = path.split_at(1);
        let first = first
            .first()
            // `first` represents `&path[0..1]`, which will always have a length of at least one.
            .expect_unreachable(); // CHECKED(Chloe)

        let mut node = self.nodes.get(first)?;
        let mut nodes = &node.nodes;

        for next in rest {
            node = nodes.get(next)?;
            nodes = &node.nodes;
        }

        Some(node)
    }
}

/// A node in a tree of modules.
pub struct ModuleNode {
    /// The path of this module. Will always be non-empty.
    pub path: LogicalPath,
    /// The files that make up this module.
    pub files: Vec<FileId>,
    /// The items of this module.
    pub items: IdentityHashMap<Interned<str>, Item>,
    /// The children of this module.
    pub nodes: IdentityHashMap<Interned<str>, Self>,
    /// Whether this module is exported or not.
    pub exported: bool,
    auto: bool,
}

impl TreeItem for ModuleNode {
    fn name<'a>(&'a self, global: &'a Global) -> Cow<'a, str> {
        debug_assert!(
            !self.path.is_empty(),
            "ModuleNode paths shouldn't be empty!"
        );

        // This is an invariant.
        let path = self
            .path
            .last()
            .expect_unreachable() // CHECKED(Chloe)
            .display(global);
        let exp = if self.exported { "exp " } else { "" };

        format!("{exp}{path}").into()
    }

    fn children(&self) -> Cow<'_, [&dyn TreeItem]> {
        self.nodes
            .values()
            .map(|s| s as &dyn TreeItem)
            .collect::<Vec<_>>()
            .into()
    }
}

/// Errors for module analysis.
pub enum ModuleAnalyzerError {
    /// A conflict in visibillities for duplicate modules.
    ModuleVisibillityConflict(LogicalPath, Vec<FileId>),
    /// An item name is repeated in a module.
    DuplicateItem(LogicalPath),
}

impl ModuleAnalyzerError {
    /// Displays the error.
    ///
    /// # Panics
    /// This function panics if the file used to produce this error is not in `global`'s file list.
    pub fn display(&self, global: &Global) -> String {
        match self {
            Self::ModuleVisibillityConflict(path, files) => {
                let mut string = format!(
                    "Duplicated module {} with different visibillities! Files: ",
                    path.display(global)
                );

                for file in files {
                    assert!(
                        global.file_list().contains(*file),
                        "Global doesn't contain file!"
                    );

                    let file = global
                        .file_list()
                        .get(*file)
                        // This was asserted above.
                        .expect_unreachable(); // CHECKED(Chloe)

                    string += &file.path().display().to_string();
                    string += ", ";
                }

                // Remove last comma
                string.truncate(string.len() - 2);

                Diag::without_span(string).display(global)
            }
            Self::DuplicateItem(path) => {
                Diag::without_span(format!("duplicate item {}!", path.display(global)))
                    .display(global)
            }
        }
    }
}

/// Run the module analyzer on a list of modules.
pub fn analyze(modules: Vec<AstModule>, global: &Global) -> (ModuleTree, Vec<ModuleAnalyzerError>) {
    let mut root = IdentityHashMap::default();
    let mut error_set = FxHashSet::default();
    let mut errors = Vec::new();

    for module in modules {
        let Some(mod_stmt) = module.mod_stmt else {
            continue;
        };
        let Some(path) = mod_stmt.path else {
            continue;
        };
        let exported = mod_stmt.exported;

        // Hacky fix to ensure node is initialized,
        // this value is never actually used.
        let mut default = ModuleNode {
            path: LogicalPath::new(thin_vec![global.intern_str("<ERROR>")]),
            files: vec![],
            items: IdentityHashMap::default(),
            nodes: IdentityHashMap::default(),
            exported: false,
            auto: true,
        };

        let mut nodes = &mut root;
        let mut node = &mut default;
        let mut logical_path: LogicalPath = LogicalPath::new(thin_vec![]);

        for component in &path {
            let Some(component) = component else {
                break;
            };

            let str = component.get(global).0;

            logical_path.push(str);

            let new_node = nodes.entry(str).or_insert(ModuleNode {
                path: logical_path.clone(),
                files: vec![],
                items: IdentityHashMap::default(),
                nodes: IdentityHashMap::default(),
                exported: true,
                auto: true,
            });
            node = new_node;
            nodes = &mut node.nodes;

            debug_assert!(
                !node.path.is_empty(),
                "ModuleNode paths shouldn't be empty!"
            );
        }

        if !node.auto {
            for item in module.items.into_iter().flatten() {
                let Some(name) = item.get_name(global) else {
                    continue;
                };
                node.items.insert(name, item);
            }

            if node.exported != exported {
                error_set.insert(logical_path);
                continue;
            }

            continue;
        }

        for item in module.items.into_iter().flatten() {
            let Some(name) = item.get_name(global) else {
                continue;
            };

            if node.items.contains_key(&name) {
                let mut path = logical_path.clone();
                path.push(name);
                errors.push(ModuleAnalyzerError::DuplicateItem(path));
            }

            node.items.insert(name, item);
        }
        node.files.push(module.file);
        node.exported = exported;
        node.auto = false;
    }

    let tree = ModuleTree { nodes: root };

    let errors = error_set
        .into_iter()
        .map(|path| {
            let node = tree
                .get(&path)
                // Paths added to the error_set always exist in the tree.
                .expect_unreachable(); // CHECKED(Chloe)

            ModuleAnalyzerError::ModuleVisibillityConflict(path, node.files.clone())
        })
        .chain(errors)
        .collect();

    (tree, errors)
}
