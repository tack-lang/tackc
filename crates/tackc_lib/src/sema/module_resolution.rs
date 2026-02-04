use crate::{ast::AstModule, global::Interned, hash::IdentityHashMap, sema::LogicalModule};

#[derive(Default)]
struct Analyzer {
    logical_mods: IdentityHashMap<Interned<str>, LogicalModule>,
}

impl Analyzer {
    pub fn analyze_module(&mut self, module: AstModule) {
        let Some(mod_stmt) = module.mod_stmt else {
            return;
        };
        let Some(path) = mod_stmt.path else {
            return;
        };

        let mut comps = path.components.iter();
        let Some(first) = comps.next().unwrap() else {
            return;
        };

        let mut current = self.logical_mods.entry(first.0).or_default();

        for next in comps {
            let Some(next) = next else {
                return;
            };

            current = current.submodules.entry(next.0).or_default();
        }

        current.items = module.items;
        current.spans = module.spans;
        current.exported = mod_stmt.exported;
    }
}

pub fn resolve_mods(mods: Vec<AstModule>) -> IdentityHashMap<Interned<str>, LogicalModule> {
    let mut analyzer = Analyzer::default();

    for module in mods {
        analyzer.analyze_module(module);
    }

    analyzer.logical_mods
}
