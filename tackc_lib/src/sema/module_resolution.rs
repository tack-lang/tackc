//! Module resolution, or turning a list of [`AstModule`]s into a map of [`LogicalPath`]s to [`LogicalModule`].

use rustc_hash::FxHashMap;

use crate::{
    ast::AstModule,
    global::Global,
    sema::{LogicalModule, LogicalPath, ModuleList},
};

struct Resolver<'src> {
    logical_mods: FxHashMap<LogicalPath, LogicalModule<'src>>,
    global: &'src Global,
}

impl<'src> Resolver<'src> {
    fn analyze_module(&mut self, module: AstModule<'src>) {
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

        let mut logical_path = LogicalPath::from([first.get(self.global).0]);
        let mut current = self
            .logical_mods
            .entry(logical_path.clone())
            .or_insert_with(|| LogicalModule::new(logical_path.clone()));

        for next in comps {
            let Some(next) = next else {
                return;
            };

            logical_path.push(next.get(self.global).0);

            current = self
                .logical_mods
                .entry(logical_path.clone())
                .or_insert_with(|| LogicalModule::new(logical_path.clone()));
        }

        current.items = module.items;
        current.spans = module.spans;
        current.exported = mod_stmt.exported;
    }
}

/// Transforms a list of [`AstModule`]s into a [`ModuleList`].
pub fn resolve_mods<'src>(mods: Vec<AstModule<'src>>, global: &'src Global) -> ModuleList<'src> {
    let mut analyzer = Resolver {
        logical_mods: FxHashMap::default(),
        global,
    };

    for module in mods {
        analyzer.analyze_module(module);
    }

    ModuleList {
        mods: analyzer.logical_mods,
    }
}
