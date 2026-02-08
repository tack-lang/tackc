use std::ops::Deref;

use crate::{
    ast::AstModule,
    global::{Global, Interned},
    hash::IdentityHashMap,
    sema::{LogicalModule, LogicalPath},
};

#[derive(Debug)]
pub struct ModuleTree {
    pub mods: IdentityHashMap<Interned<str>, LogicalModule>,
}

impl ModuleTree {
    pub fn display(&self, global: &Global) -> String {
        self.mods
            .values()
            .map(|module| module.display(global))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Deref for ModuleTree {
    type Target = IdentityHashMap<Interned<str>, LogicalModule>;

    fn deref(&self) -> &Self::Target {
        &self.mods
    }
}

#[derive(Default)]
struct Resolver {
    logical_mods: IdentityHashMap<Interned<str>, LogicalModule>,
}

impl Resolver {
    fn analyze_module(&mut self, module: AstModule) {
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

        let mut path = LogicalPath::from([first.0]);
        let mut current = self
            .logical_mods
            .entry(first.0)
            .or_insert_with(|| LogicalModule::new(path.clone()));

        for next in comps {
            let Some(next) = next else {
                return;
            };

            path.push(next.0);

            current = current
                .submodules
                .entry(next.0)
                .or_insert_with(|| LogicalModule::new(path.clone()));
        }

        current.items = module.items;
        current.spans = module.spans;
        current.exported = mod_stmt.exported;
    }
}

pub fn resolve_mods(mods: Vec<AstModule>) -> ModuleTree {
    let mut analyzer = Resolver::default();

    for module in mods {
        analyzer.analyze_module(module);
    }

    ModuleTree {
        mods: analyzer.logical_mods,
    }
}
