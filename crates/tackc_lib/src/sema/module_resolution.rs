use std::{collections::HashMap, fmt::Write, ops::Deref};

use crate::{
    ast::AstModule,
    global::Global,
    sema::{LogicalModule, LogicalPath},
};

#[derive(Debug)]
pub struct ModuleList {
    pub mods: HashMap<LogicalPath, LogicalModule>,
}

impl ModuleList {
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

impl Deref for ModuleList {
    type Target = HashMap<LogicalPath, LogicalModule>;

    fn deref(&self) -> &Self::Target {
        &self.mods
    }
}

#[derive(Default)]
struct Resolver {
    logical_mods: HashMap<LogicalPath, LogicalModule>,
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
        let Some(Some(first)) = comps.next() else {
            return;
        };

        let mut logical_path = LogicalPath::from([first.0]);
        let mut current = self
            .logical_mods
            .entry(logical_path.clone())
            .or_insert_with(|| LogicalModule::new(logical_path.clone()));

        for next in comps {
            let Some(next) = next else {
                return;
            };

            logical_path.push(next.0);

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

pub fn resolve_mods(mods: Vec<AstModule>) -> ModuleList {
    let mut analyzer = Resolver::default();

    for module in mods {
        analyzer.analyze_module(module);
    }

    ModuleList {
        mods: analyzer.logical_mods,
    }
}
