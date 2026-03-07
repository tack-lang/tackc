//! Global resolution, or taking a [`ModuleList`], and collecting its globals into a [`BindingList`].

use parking_lot::Mutex;

use crate::{
    ast::{AstVisitor, ConstItem, FuncItem, ImpItem},
    global::Global,
    sema::{
        Binding, BindingKind, BindingList, ImportBinding, LogicalModule, LogicalPath, ModuleList,
    },
};

struct Resolver<'src> {
    global: &'src Global,
    path: Option<&'src LogicalPath>,
    module: Option<&'src LogicalModule<'src>>,
    list: BindingList,
}

impl Resolver<'_> {
    const fn new_idx(&mut self) -> u64 {
        let temp = self.list.idx;
        self.list.idx += 1;
        temp
    }
}

impl<'src> AstVisitor<'src> for Resolver<'src> {
    fn visit_logical_module(&mut self, module: &'src LogicalModule<'src>) {
        self.path = Some(&module.path);
        self.module = Some(module);

        for item in module.items.iter().flatten() {
            self.visit_item(item);
        }

        self.path = None;
        self.module = None;
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
            kind: Mutex::new(BindingKind::Const),
            name: ident,
            exported: item.exported,
            idx: self.new_idx(),
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
            kind: Mutex::new(BindingKind::Func),
            name: ident,
            exported: item.exported,
            idx: self.new_idx(),
        };
        self.list.map.insert(path, self.global.intern(binding));
    }

    fn visit_imp_item(&mut self, item: &'src ImpItem<'src>) {
        let Some(path) = self.path else { return };
        let mut path = path.clone();
        let Some(ast_path) = item.path else { return };
        let Some(imp_path) = LogicalPath::try_from(ast_path, self.global) else {
            return;
        };
        let Some(name) = imp_path.comps.last().copied() else {
            return;
        };
        path.push(name);

        if self.list.map.contains_key(&path) {
            return;
        }

        let Some(module) = self.module else { return };
        let Some(&span) = module.spans.get(&ast_path.id) else {
            return;
        };

        let binding = Binding {
            kind: Mutex::new(BindingKind::Import(ImportBinding::Path(imp_path, span))),
            name,
            exported: item.exported,
            idx: self.new_idx(),
        };
        self.list.map.insert(path, self.global.intern(binding));
    }
}

/// Collects a [`BindingList`] from a [`ModuleList`].
pub fn resolve_globals(mods: &ModuleList, global: &Global) -> BindingList {
    let mut r = Resolver {
        global,
        path: None,
        module: None,
        list: BindingList::default(),
    };
    r.visit_module_list(mods);

    r.list
}
