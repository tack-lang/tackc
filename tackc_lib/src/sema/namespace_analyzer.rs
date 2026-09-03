//! A step to do namespace analysis - turning a [`ModuleTree`] into a tree of [`Namespace`]s.

use std::{borrow::Cow, fmt::Write, mem};

use derive_more::{IsVariant, TryUnwrap, Unwrap};
use rustc_hash::FxHashMap;

use crate::{
    frontend::ast::{
        AstVisitor, Block, Expression, FuncItem, Item, ItemKind, LetStatement, TriState,
    },
    global::Global,
    sema::{
        LogicalPath, NonEmptyLogicalPath, PathComponent,
        module_analyzer::{ModuleNode, ModuleTree},
    },
    utils::{
        UnwrapExt,
        tree::{TreeItem, TreeItemExt},
    },
};

/// A namespace, holds items, and other namespaces.
#[derive(Debug, Default)]
pub struct Namespace<'a> {
    /// The children of this namespace.
    pub children: FxHashMap<PathComponent, NamespaceChild<'a>>,
}

impl Namespace<'_> {
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
pub struct NamespaceChild<'a> {
    /// The path to this child, ignoring any re-exports.
    pub path: NonEmptyLogicalPath,
    /// The name this child can be accessed by. Usually the last component of `path`.
    pub name: PathComponent,
    /// The value of this child.
    pub value: NamespaceExpression<'a>,
    /// Whether or not this child is exported.
    pub exported: bool,
    /// Whether or not this child is only attatched to instances of this namespace.
    pub instance: bool,
}

impl TreeItem for NamespaceChild<'_> {
    fn name<'a>(&'a self, global: &'a Global) -> std::borrow::Cow<'a, str> {
        match self.value {
            NamespaceExpression::Namespace(_) => {
                let name = self.name.display(global);
                let exp = if self.exported { "exp " } else { "" };
                format!("{exp}namespace {name}").into()
            }
            _ => self.name.display(global).into(),
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

/// An expression belonging to a namespace.
#[derive(Debug, Unwrap, TryUnwrap, IsVariant)]
pub enum NamespaceExpression<'a> {
    /// An expression.
    Expression(&'a Expression),
    /// A function.
    Function(NamespaceFunction<'a>),
    /// A path, used for `imp` statements.
    Path(NonEmptyLogicalPath),
    /// A namespace.
    Namespace(Namespace<'a>),
    /// A let binding, used in function namespaces.
    LetBinding(Option<TypeHint<'a>>),
    /// A parameter, used in function namespaces.
    Parameter(TypeHint<'a>),
}

/// A function, represented in a namespace.
#[derive(Debug, Clone, Copy)]
pub struct NamespaceFunction<'a> {
    /// The return type of this function.
    pub ret_type: Option<&'a Expression>,
    /// The block for this function.
    pub block: &'a Block,
}

/// A type hint. Self-documenting newtype struct for [`Expression`].
#[derive(Debug, Clone, Copy)]
pub struct TypeHint<'a>(pub &'a Expression);

/// The state of the analyzer.
struct State<'a> {
    /// The global context given to the analyzer.
    global: &'a Global,
}

/// Analyzes the [`ModuleTree`], turning it into a [`Namespace`].
pub fn analyze<'a>(modules: &'a ModuleTree, global: &Global) -> Namespace<'a> {
    let mut children = FxHashMap::default();

    let state = State { global };

    for module in modules.nodes.values() {
        state.analyze_module(&mut children, module, &LogicalPath::EMPTY);
    }

    Namespace { children }
}

impl State<'_> {
    /// Analyzes a module, and adds it to the namespace given.
    fn analyze_module<'a>(
        &self,
        namespace: &mut FxHashMap<PathComponent, NamespaceChild<'a>>,
        module: &'a ModuleNode,
        namespace_path: &LogicalPath,
    ) {
        let name = module.get_name();
        let path = namespace_path.join_non_empty(name);

        let mut children = FxHashMap::default();

        for module in module.nodes.values() {
            self.analyze_module(&mut children, module, &path);
        }

        for item in module.items.values() {
            self.analyze_item(&mut children, item, &path);
        }

        namespace.insert(
            name.into(),
            NamespaceChild {
                path,
                name: name.into(),
                value: NamespaceExpression::Namespace(Namespace { children }),
                exported: module.exported,
                instance: false,
            },
        );
    }

    /// Analyzes an item, and adds it to the given namespace.
    fn analyze_item<'a>(
        &self,
        namespace: &mut FxHashMap<PathComponent, NamespaceChild<'a>>,
        item: &'a Item,
        namespace_path: &LogicalPath,
    ) -> Option<()> {
        match item.kind {
            ItemKind::ConstItem(ref item) => {
                let name = item.ident?.get(&self.global.interner).0;
                let path = namespace_path.join_non_empty(name);
                namespace.insert(
                    name.into(),
                    NamespaceChild {
                        name: name.into(),
                        value: NamespaceExpression::Expression(item.expr.as_ref()?),
                        exported: item.exported,
                        path,
                        instance: false,
                    },
                );
            }
            ItemKind::FuncItem(ref func_item) => {
                self.analyze_function(namespace, func_item, namespace_path);
            }
            ItemKind::ImpItem(ref imp_item) => {
                let ast_path = imp_item.path.as_ref()?;

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
                    .try_unwrap_identifier()
                    // `imp_path` was created from an AstPath, which doesn't contain `Idx` components.
                    .expect_unreachable(); // CHECKED(Chloe)
                let imp_path = namespace_path.join_non_empty(name);

                namespace.insert(
                    name.into(),
                    NamespaceChild {
                        name: name.into(),
                        path: imp_path,
                        value: NamespaceExpression::Path(pointed_path),
                        exported: imp_item.exported,
                        instance: false,
                    },
                );
            }
        }

        Some(())
    }

    /// Analyzes a function, and adds it to the given namespace.
    fn analyze_function<'a>(
        &self,
        namespace: &mut FxHashMap<PathComponent, NamespaceChild<'a>>,
        func_item: &'a FuncItem,
        namespace_path: &LogicalPath,
    ) -> Option<()> {
        let name = PathComponent::Identifier(func_item.ident?.get(&self.global.interner).0);
        let path = namespace_path.join_non_empty(name);
        let exported = func_item.exported;
        let mut children = FxHashMap::default();

        self.populate_function_namespace(&mut children, func_item, &path)?;

        namespace.insert(
            name,
            NamespaceChild {
                path,
                name,
                value: NamespaceExpression::Namespace(Namespace { children }),
                exported,
                instance: false,
            },
        );

        Some(())
    }

    /// Populates a function namespace using a [`FuncItem`].
    fn populate_function_namespace<'a>(
        &self,
        namespace: &mut FxHashMap<PathComponent, NamespaceChild<'a>>,
        func_item: &'a FuncItem,
        namespace_path: &LogicalPath,
    ) -> Option<()> {
        for &(param, ref type_hint) in &func_item.params {
            let name = PathComponent::Identifier(param?.get(&self.global.interner).0);
            let path = namespace_path.join_non_empty(name);
            namespace.insert(
                name,
                NamespaceChild {
                    path,
                    name,
                    value: NamespaceExpression::Parameter(TypeHint(type_hint.as_ref()?)),
                    exported: true,
                    instance: false,
                },
            );
        }

        let children = BlockNamespaceVisitor {
            current: FxHashMap::default(),
            current_path: namespace_path.into_non_empty()?,
            global: self.global,
        }
        .visit_root(func_item.block.as_ref()?);

        for (comp, child) in children {
            namespace.insert(comp, child);
        }

        let inner_path = namespace_path.join_non_empty(PathComponent::Function);
        namespace.insert(
            PathComponent::Function,
            NamespaceChild {
                path: inner_path,
                name: PathComponent::Function,
                value: NamespaceExpression::Function(NamespaceFunction {
                    block: func_item.block.as_ref()?,
                    ret_type: match func_item.ret_type {
                        TriState::Some(ref val) => Some(val),
                        TriState::None => None,
                        TriState::Error => return None,
                    },
                }),
                exported: true,
                instance: false,
            },
        );

        Some(())
    }
}

/// An [`AstVisitor`] that visits blocks to add them to the namespace.
struct BlockNamespaceVisitor<'g, 'a> {
    /// The current namespace.
    current: FxHashMap<PathComponent, NamespaceChild<'a>>,
    /// The path to the current namespace.
    current_path: NonEmptyLogicalPath,
    /// The global context.
    global: &'g Global,
}

impl<'a> BlockNamespaceVisitor<'_, 'a> {
    /// Visits the root of a function block.
    fn visit_root(mut self, block: &'a Block) -> FxHashMap<PathComponent, NamespaceChild<'a>> {
        for stmt in block.stmts.iter().flatten() {
            self.visit_statement(stmt);
        }

        if let TriState::Some(ref expr) = block.expr {
            self.visit_expression(expr);
        }

        self.current
    }
}

impl<'a> AstVisitor<'a> for BlockNamespaceVisitor<'_, 'a> {
    fn visit_block(&mut self, block: &'a Block) {
        let component = PathComponent::NodeId(block.id);
        let mut old = mem::take(&mut self.current);
        let path = self.current_path.join(component);
        self.current_path = path.clone();

        for stmt in block.stmts.iter().flatten() {
            self.visit_statement(stmt);
        }

        if let TriState::Some(ref expr) = block.expr {
            self.visit_expression(expr);
        }

        let new = mem::take(&mut self.current);
        old.insert(
            PathComponent::NodeId(block.id),
            NamespaceChild {
                path,
                name: component,
                value: NamespaceExpression::Namespace(Namespace { children: new }),
                exported: true,
                instance: false,
            },
        );
        self.current = old;
    }

    fn visit_let_statement(&mut self, stmt: &'a LetStatement) {
        if let TriState::Some(ref ty) = stmt.ty {
            self.visit_expression(ty);
        }
        if let TriState::Some(ref expr) = stmt.expr {
            self.visit_expression(expr);
        }

        let Some(sym) = stmt.ident else { return };
        let name = PathComponent::Identifier(sym.get(&self.global.interner).0);
        let path = self.current_path.join(name);

        self.current.insert(
            name,
            NamespaceChild {
                path,
                name,
                value: NamespaceExpression::LetBinding(stmt.expr.as_ref().some().map(TypeHint)),
                exported: true,
                instance: false,
            },
        );
    }
}
