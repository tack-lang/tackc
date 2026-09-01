//! A step to do namespace analysis - turning a tree of [`LogicalModule`]s into an [`AnonymousNamespace`]s.

use std::{borrow::Cow, fmt::Write, mem};

use thin_vec::ThinVec;

use crate::{
    frontend::ast::{
        AstVisitor, Block, Expression, FuncItem, Item, ItemKind, LetStatement, TriState,
    },
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
#[derive(Debug)]
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
#[derive(Debug)]
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
            NamespaceExpression::Function(ref func) => func.metadata.children(),
            _ => (&[]).into(),
        }
    }
}

/// An expression belonging to a namespace.
#[derive(Debug)]
pub enum NamespaceExpression {
    /// An expression.
    Expression(Expression),
    /// A function.
    Function(NamespaceFunction),
    /// A path, used for `imp` statements.
    Path(NonEmptyLogicalPath),
    /// A namespace.
    Namespace(Namespace),
    /// A let binding, used in a function.
    LetBinding,
}

/// A function represented in a namespace.
#[derive(Debug)]
pub struct NamespaceFunction {
    /// The parameters for this function.
    pub params: ThinVec<(Interned<str>, Expression)>,
    /// The return type of this function.
    pub ret_type: TriState<Expression>,
    /// The block for this function.
    pub block: Block,
    /// The 'metadata' about this function. Contains information allowing functions to be namespaces.
    pub metadata: NamespaceBlock,
}

/// Metadata turning a block into a namespace.
#[derive(Debug)]
pub struct NamespaceBlock {
    /// The path to this block.
    pub path: NonEmptyLogicalPath,
    /// The children of this block. Referenced using [`Idx`](crate::sema::PathComponent::Idx) paths.
    pub children: ThinVec<Self>,
    /// Let bindings in this block.
    pub bindings: ThinVec<Interned<str>>,
}

impl TreeItem for NamespaceBlock {
    fn name<'a>(&'a self, global: &'a Global) -> Cow<'a, str> {
        self.path.last().display(global).into()
    }

    fn children(&self) -> Cow<'_, [&dyn TreeItem]> {
        self.children
            .iter()
            .map(|block| block as &dyn TreeItem)
            .chain(self.bindings.iter().map(|str| str as &dyn TreeItem))
            .collect()
    }
}

/// The state of the analyzer.
struct State<'a> {
    /// The global context given to the analyzer.
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
    /// Analyzes a module, and adds it to the namespace given.
    fn analyze_module(
        &self,
        namespace: &mut IdentityHashMap<Interned<str>, NamespaceChild>,
        module: ModuleNode,
        namespace_path: &LogicalPath,
    ) {
        let name = module.get_name();
        let path = namespace_path.join_non_empty(name);

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

    /// Analyzes an item, and adds it to the given namespace.
    fn analyze_item(
        &self,
        namespace: &mut IdentityHashMap<Interned<str>, NamespaceChild>,
        item: Item,
        namespace_path: &LogicalPath,
    ) -> Option<()> {
        match item.kind {
            ItemKind::ConstItem(item) => {
                let name = item.ident?.get(&self.global.interner).0;
                let path = namespace_path.join_non_empty(name);
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
                self.analyze_function(namespace, func_item, namespace_path);
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
                    .collect::<LogicalPath>()
                    .into_non_empty()
                    // `ast_path` is an AstPath without any `None` components, which are required to be non-empty.
                    .expect_unreachable(); // CHECKED(Chloe)
                let name = pointed_path
                    .last()
                    .identifier()
                    // `imp_path` was created from an AstPath, which doesn't contain `Idx` components.
                    .expect_unreachable(); // CHECKED(Chloe)
                let imp_path = namespace_path.join_non_empty(name);

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

    /// Analyzes a function, and adds it to the given namespace.
    fn analyze_function(
        &self,
        namespace: &mut IdentityHashMap<Interned<str>, NamespaceChild>,
        func_item: FuncItem,
        namespace_path: &LogicalPath,
    ) -> Option<()> {
        struct MetadataVisitor<'a> {
            current: NamespaceBlock,
            global: &'a Global,
        }

        impl MetadataVisitor<'_> {
            fn visit_root(mut self, block: &Block) -> NamespaceBlock {
                for stmt in block.stmts.iter().flatten() {
                    self.visit_statement(stmt);
                }

                if let Some(expr) = block.expr.as_ref().some() {
                    self.visit_expression(expr);
                }

                self.current
            }
        }

        impl AstVisitor<'_> for MetadataVisitor<'_> {
            fn visit_block(&mut self, block: &Block) {
                let new_path = self.current.path.join(
                    self.global
                        .interner
                        .intern_str(format!("_{}", self.current.children.len())),
                );
                let new = NamespaceBlock {
                    path: new_path,
                    children: ThinVec::new(),
                    bindings: ThinVec::new(),
                };
                let old = mem::replace(&mut self.current, new);

                for stmt in block.stmts.iter().flatten() {
                    self.visit_statement(stmt);
                }

                if let Some(expr) = block.expr.as_ref().some() {
                    self.visit_expression(expr);
                }

                let new = mem::replace(&mut self.current, old);

                self.current.children.push(new);
            }

            fn visit_let_statement(&mut self, stmt: &'_ LetStatement) {
                if let Some(ident) = stmt.ident {
                    self.current
                        .bindings
                        .push(ident.get(&self.global.interner).0);
                }

                if let TriState::Some(ref expr) = stmt.expr {
                    self.visit_expression(expr);
                }

                if let TriState::Some(ref ty) = stmt.ty {
                    self.visit_expression(ty);
                }
            }
        }

        let name = func_item.get_name(self.global)?;
        let path = namespace_path.join_non_empty(name);

        let visitor = MetadataVisitor {
            current: NamespaceBlock {
                path: path.clone(),
                children: ThinVec::new(),
                bindings: ThinVec::new(),
            },
            global: self.global,
        };

        let root = visitor.visit_root(func_item.block.as_ref()?);

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
            ret_type: func_item.ret_type,
            metadata: root,
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

        Some(())
    }
}
