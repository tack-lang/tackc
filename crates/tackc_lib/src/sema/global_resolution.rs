//! Global resolution, or taking a [`ModuleList`], and collecting its globals into a [`GlobalList`].

use crate::{
    ast::{AstVisitor, ConstItem, FuncItem, ImpItem},
    global::Global,
    sema::{Binding, BindingKind, BindingList, LogicalModule, LogicalPath, ModuleList},
};

struct Resolver<'src> {
    global: &'src Global,
    path: Option<&'src LogicalPath>,
    list: BindingList,
}

impl<'src> AstVisitor<'src> for Resolver<'src> {
    fn visit_logical_module(&mut self, module: &'src LogicalModule<'src>) {
        self.path = Some(&module.path);

        for item in module.items.iter().flatten() {
            self.visit_item(item);
        }

        self.path = None;
    }

    fn visit_const_item(&mut self, item: &'src ConstItem<'src>) {
        let Some(path) = self.path else { return };
        let mut path = path.clone();
        let Some(ident) = item.ident else { return };
        let ident = ident.get(self.global).0;
        path.push(ident);

        if self.list.map.contains_key(&path) {
            return;
        }

        let binding = Binding {
            kind: BindingKind::Const,
            name: ident,
            exported: item.exported,
        };
        self.list.map.insert(path, self.global.intern(binding));
    }

    fn visit_func_item(&mut self, item: &'src FuncItem<'src>) {
        let Some(path) = self.path else { return };
        let mut path = path.clone();
        let Some(ident) = item.ident else { return };
        let ident = ident.get(self.global).0;
        path.push(ident);

        if self.list.map.contains_key(&path) {
            return;
        }

        let binding = Binding {
            kind: BindingKind::Func,
            name: ident,
            exported: item.exported,
        };
        self.list.map.insert(path, self.global.intern(binding));
    }

    fn visit_imp_item(&mut self, item: &'src ImpItem<'src>) {
        let Some(ast_path) = item.path else { return };
        let Some(imp_path) = LogicalPath::try_from(ast_path, self.global) else {
            return;
        };
        let name = *imp_path.comps.last().unwrap();
        let Some(path) = self.path else { return };
        let mut path = path.clone();
        path.push(name);

        let binding = Binding {
            kind: BindingKind::Import(imp_path),
            name,
            exported: item.exported,
        };
        self.list.map.insert(path, self.global.intern(binding));
    }
}

/// Collects a [`GlobalList`] from a [`ModuleList`].
pub fn resolve_globals(mods: &ModuleList, global: &Global) -> BindingList {
    let mut r = Resolver {
        global,
        path: None,
        list: BindingList::default(),
    };
    r.visit_module_list(mods);

    r.list
}
