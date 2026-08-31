//! A step to do namespace analysis - turning a tree of [`LogicalModule`]s into an [`AnonymousNamespace`]s.

use std::{borrow::Cow, fmt::Write};

use thin_vec::ThinVec;

use crate::{
    frontend::ast::{Block, Expression, Item, ItemKind},
    global::Global,
    sema::{
        LogicalPath, NonEmptyLogicalPath,
        module_analyzer::{ModuleNode, ModuleTree},
    },
    utils::{
        UnwrapExt,
        hash::IdentityHashMap,
        intern::Interned,
        tree::{TreeItem, TreeItemExt},
    },
};

/// A namespace, holds items, and other namespaces.
pub struct Namespace {
    /// The children of this namespace.
    pub children: IdentityHashMap<Interned<str>, NamespaceChild>,
}

impl Namespace {
    /// Displays this [`Namespace`].
    pub fn display(&self, global: &Global) -> String {
        let mut out = String::new();

        for module in self.children.values() {
            _ = writeln!(out, "{}", module.display(global));
        }

        out.truncate(out.len() - 1);

        out
    }
}

/// A child of a namespace.
pub struct NamespaceChild {
    /// The path to this child, ignoring any re-exports.
    pub path: NonEmptyLogicalPath,
    /// The name this child can be accessed by. Usually the last component of `path`.
    pub name: Interned<str>,
    /// The value of this child.
    pub value: NamespaceExpression,
    /// Whether or not this child is exported.
    pub exported: bool,
}

/// An expression belonging to a namespace.
pub enum NamespaceExpression {
    /// An expression.
    Expression(Expression),
    /// A function.
    Function(NamespaceFunction),
    /// A path, used for `imp` statements.
    Path(LogicalPath),
    /// A namespace.
    Namespace(Namespace),
}

/// A function represented in a namespace.
pub struct NamespaceFunction {
    /// The parameters for this function.
    pub params: ThinVec<(Interned<str>, Expression)>,
    /// The return type of this function.
    pub ret_type: Expression,
    /// The block for this function.
    pub block: Block,
}

impl TreeItem for NamespaceChild {
    fn name<'a>(&'a self, global: &'a Global) -> std::borrow::Cow<'a, str> {
        match self.value {
            NamespaceExpression::Namespace(_) => {
                let name = self.name.get(&global.interner);
                let exp = if self.exported { "exp " } else { "" };
                format!("{exp}namespace {name}").into()
            }
            _ => self.name.get(&global.interner).into(),
        }
    }

    fn children(&self) -> Cow<'_, [&dyn TreeItem]> {
        match self.value {
            NamespaceExpression::Namespace(ref namespace) => namespace
                .children
                .values()
                .map(|child| child as &dyn TreeItem)
                .collect::<Vec<_>>()
                .into(),
            _ => (&[]).into(),
        }
    }
}

struct State<'a> {
    global: &'a Global,
}

/// Analyzes the [`ModuleTree`], turning it into a [`Namespace`].
pub fn analyze(modules: ModuleTree, global: &Global) -> Namespace {
    let mut children = IdentityHashMap::default();

    let state = State { global };

    for (_, module) in modules.nodes {
        state.analyze_module(&mut children, module, &LogicalPath::EMPTY);
    }

    Namespace { children }
}

impl State<'_> {
    fn analyze_module(
        &self,
        namespace: &mut IdentityHashMap<Interned<str>, NamespaceChild>,
        module: ModuleNode,
        path: &LogicalPath,
    ) {
        let name = module.get_name();
        let path = path.join_non_empty(name);

        let mut children = IdentityHashMap::default();

        for (_, module) in module.nodes {
            self.analyze_module(&mut children, module, &path);
        }

        for (_, item) in module.items {
            self.analyze_item(&mut children, item, &path);
        }

        namespace.insert(
            name,
            NamespaceChild {
                path,
                name,
                value: NamespaceExpression::Namespace(Namespace { children }),
                exported: module.exported,
            },
        );
    }

    fn analyze_item(
        &self,
        namespace: &mut IdentityHashMap<Interned<str>, NamespaceChild>,
        item: Item,
        path: &LogicalPath,
    ) -> Option<()> {
        match item.kind {
            ItemKind::ConstItem(item) => {
                let name = item.ident?.get(&self.global.interner).0;
                let path = path.join_non_empty(name);
                namespace.insert(
                    name,
                    NamespaceChild {
                        name,
                        value: NamespaceExpression::Expression(item.expr?),
                        exported: item.exported,
                        path,
                    },
                );
            }
            ItemKind::FuncItem(func_item) => {
                let name = func_item.get_name(self.global)?;
                let path = path.join_non_empty(name);

                let params = if func_item
                    .params
                    .iter()
                    .any(|&(val, ref expr)| val.is_none() || expr.is_none())
                {
                    return None;
                } else {
                    func_item
                        .params
                        .into_iter()
                        .filter_map(|(sym, expr)| Some((sym?.get(&self.global.interner).0, expr?)))
                        .collect()
                };

                let func = NamespaceFunction {
                    block: func_item.block?,
                    params,
                    ret_type: func_item.ret_type.some()?,
                };

                namespace.insert(
                    name,
                    NamespaceChild {
                        name,
                        path,
                        value: NamespaceExpression::Function(func),
                        exported: func_item.exported,
                    },
                );
            }
            ItemKind::ImpItem(imp_item) => {
                let ast_path = imp_item.path?;

                if ast_path.components().contains(&None) {
                    return None;
                }

                let pointed_path = ast_path
                    .into_iter()
                    .flatten()
                    .map(|sym| sym.get(&self.global.interner).0)
                    .collect::<LogicalPath>();
                let name = pointed_path
                    .last()
                    // `path` was created from an AstPath with no `None` components, meaning it's guaranteed to be non-empty.
                    .expect_unreachable(); // CHECKED(Chloe)
                let imp_path = path.join_non_empty(name);

                namespace.insert(
                    name,
                    NamespaceChild {
                        name,
                        path: imp_path,
                        value: NamespaceExpression::Path(pointed_path),
                        exported: imp_item.exported,
                    },
                );
            }
        }

        Some(())
    }
}
