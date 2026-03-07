//! Module resolution, or turning a list of [`AstModule`]s into a map of [`LogicalPath`]s to [`LogicalModule`].

use rustc_hash::FxHashMap;

use std::fmt::Write;

use crate::{
    ast::AstModule,
    file::File,
    global::Global,
    sema::{LogicalModule, LogicalPath, ModuleList},
    utils::UnwrapExt,
};

/// Errors during module resolution.
pub enum ModuleResolveError<'src> {
    /// A duplicated module.
    DuplicateModule(LogicalPath, Vec<&'src File>),
}

impl ModuleResolveError<'_> {
    /// Displays this module resolution error.
    pub fn display(&self, global: &Global) -> String {
        match self {
            Self::DuplicateModule(path, files) => {
                let file_paths = files.iter().map(|file| file.path());
                let mut paths = String::new();

                for file_path in file_paths {
                    _ = write!(paths, "{}, ", file_path.display());
                }

                paths.truncate(paths.len().saturating_sub(2));

                format!(
                    "Module {} is duplicated in files {}",
                    path.display(global),
                    paths
                )
            }
        }
    }
}

struct Resolver<'src> {
    logical_mods: FxHashMap<LogicalPath, LogicalModule<'src>>,
    errors: FxHashMap<LogicalPath, Vec<&'src File>>,
    global: &'src Global,
}

impl<'src> Resolver<'src> {
    fn analyze_module(&mut self, mut module: AstModule<'src>) {
        let Some(module_file) = self.global.file_list().get(&module.file) else {
            #[cfg(debug_assertions)]
            {
                println!("failed to find file in global file list! is the file list set?");
            }
            return;
        };

        let Some(mod_stmt) = module.mod_stmt else {
            return;
        };
        let Some(ref path) = mod_stmt.path else {
            return;
        };

        let mut comps = path.components.iter();
        let Some(Some(first)) = comps.next() else {
            return;
        };

        let logical_path =
            LogicalPath::new(&[first.get(self.global).0], self.global).with_global(self.global);
        let mut current = self
            .logical_mods
            .entry(logical_path.inner())
            .or_insert_with(|| LogicalModule::new(logical_path.inner()));

        for next in comps {
            let Some(next) = next else {
                return;
            };

            let logical_path = logical_path.push(next.get(self.global).0);

            current = self
                .logical_mods
                .entry(logical_path.inner())
                .or_insert_with(|| LogicalModule::new(logical_path.inner()));
        }

        if let Some(file) = current.file {
            self.errors
                .entry(logical_path.inner())
                .and_modify(|vec| vec.push(module_file))
                .or_insert_with(|| {
                    vec![
                        self.global.file_list().get(&file).expect_unreachable(), // CHECKED(Chloe)
                        module_file,
                    ]
                });

            current.items.append(&mut module.items);
            current.spans.extend(*module.spans);
            current.exported |= mod_stmt.exported;
            current.duplicated = true;

            return;
        }

        current.items = module.items;
        current.spans = module.spans;
        current.exported = mod_stmt.exported;
        current.file = Some(module.file);
    }
}

/// Transforms a list of [`AstModule`]s into a [`ModuleList`].
pub fn resolve_mods<'src>(
    mods: Vec<AstModule<'src>>,
    global: &'src Global,
) -> (ModuleList<'src>, Vec<ModuleResolveError<'src>>) {
    let mut r = Resolver {
        logical_mods: FxHashMap::default(),
        errors: FxHashMap::default(),
        global,
    };

    for module in mods {
        r.analyze_module(module);
    }

    let errors = r
        .errors
        .into_iter()
        .map(|(path, files)| ModuleResolveError::DuplicateModule(path, files))
        .collect();

    (
        ModuleList {
            mods: r.logical_mods,
        },
        errors,
    )
}
