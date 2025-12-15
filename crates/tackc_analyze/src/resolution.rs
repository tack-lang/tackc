use std::{collections::HashMap, mem};

use tackc_ast::{Binding, Block, ConstItem, FuncItem, Item, MaybeError};
use tackc_error::Diag;
use tackc_global::{Global, Interned};
use tackc_parser::ast::{AstNode, LetStatement, Primary, PrimaryKind, Program, Symbol, VisitorMut};
use tackc_utils::hash::IdentityHasherBuilder;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum ErrorType {
    Missing,
    Duplicated,
}

type Scope = HashMap<Interned<str>, (Interned<Binding>, bool), IdentityHasherBuilder>;

struct SymbolResolver<'a> {
    global_scope: Scope,
    scopes: Vec<Scope>,
    errors: Vec<(Symbol, ErrorType)>,
    global: &'a Global,
}

impl<'a> SymbolResolver<'a> {
    pub fn new(global: &'a Global) -> Self {
        SymbolResolver {
            global_scope: HashMap::default(),
            scopes: vec![],
            errors: Vec::new(),
            global,
        }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::default());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn iter(&self) -> impl Iterator<Item = &Scope> {
        self.scopes
            .iter()
            .rev()
            .chain(std::iter::once(&self.global_scope))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.scopes
            .iter_mut()
            .rev()
            .chain(std::iter::once(&mut self.global_scope))
    }

    fn get_entry_mut(&mut self, str: Interned<str>) -> Option<&mut (Interned<Binding>, bool)> {
        for scope in self.iter_mut() {
            if let Some(entry) = scope.get_mut(&str) {
                return Some(entry);
            }
        }
        None
    }

    pub fn exists(&self, str: Interned<str>) -> bool {
        for scope in self.iter() {
            if scope.get(&str).is_some() {
                return true;
            }
        }
        false
    }

    pub fn resolve(&self, str: Interned<str>) -> Option<Interned<Binding>> {
        for scope in self.iter() {
            if let Some((binding, enabled)) = scope.get(&str) {
                if !*enabled {
                    return None;
                }
                return Some(*binding);
            }
        }
        None
    }

    pub fn disable(&mut self, str: Interned<str>) {
        let str_display = str.display(self.global);
        let (_, enabled) = self
            .get_entry_mut(str)
            .unwrap_or_else(|| panic!("cannot disable non-existant binding {str_display}!"));
        *enabled = false;
    }

    pub fn enable(&mut self, str: Interned<str>) {
        let str_display = str.display(self.global);
        let (_, enabled) = self
            .get_entry_mut(str)
            .unwrap_or_else(|| panic!("cannot enable non-existant binding {str_display}!"));
        *enabled = true;
    }

    pub fn define(&mut self, str: Interned<str>, binding: Binding) -> Interned<Binding> {
        let interned = self.global.intern(binding);
        self.current_scope_mut().insert(str, (interned, true));
        self.current_scope().get(&str).unwrap().0
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().unwrap_or(&self.global_scope)
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap_or(&mut self.global_scope)
    }
}

impl SymbolResolver<'_> {
    fn visit_const_item_shallow(&mut self, item: &mut ConstItem) {
        if self.exists(item.ident.inner) {
            self.errors.push((item.ident, ErrorType::Duplicated));
            return;
        }

        item.binding = Some(self.define(
            item.ident.inner,
            Binding {
                symbol: item.ident,
                ty_annotation: item.ty.as_ref().map(|expr| expr.id),
            },
        ));
    }

    fn visit_func_item_shallow(&mut self, item: &mut FuncItem) {
        if self.exists(item.ident.inner) {
            self.errors.push((item.ident, ErrorType::Duplicated));
            return;
        }

        item.binding = Some(self.define(
            item.ident.inner,
            Binding {
                symbol: item.ident,
                ty_annotation: None,
            },
        ));
    }
}

impl VisitorMut for SymbolResolver<'_> {
    fn visit_func_item_mut(&mut self, item: &mut FuncItem) {
        let env = mem::take(&mut self.scopes);
        self.disable(item.ident.inner);

        for ty in item.params.iter_mut().filter_map(|(_, ty, _)| ty.as_mut()) {
            ty.accept_mut(self);
        }
        if let MaybeError::Some(ty) = &mut item.ret_ty {
            ty.accept_mut(self);
        }
        for (ident, ty, binding) in &mut item.params {
            *binding = Some(self.define(
                ident.inner,
                Binding {
                    symbol: *ident,
                    ty_annotation: ty.as_ref().map(|ty| ty.id),
                },
            ));
        }
        item.block.accept_mut(self);

        self.enable(item.ident.inner);
        self.scopes = env;
    }

    fn visit_block_mut(&mut self, block: &mut Block) {
        self.push();
        for stmt in &mut block.stmts {
            stmt.accept_mut(self);
        }
        if let Some(expr) = &mut block.expr {
            expr.accept_mut(self);
        }
        self.pop();
    }

    fn visit_program_mut(&mut self, program: &mut Program) {
        for item in &mut program.items {
            match item {
                Item::ConstItem(item) => self.visit_const_item_shallow(item),
                Item::FuncItem(item) => self.visit_func_item_shallow(item),
            }
        }

        for item in &mut program.items {
            item.accept_mut(self);
        }
    }

    fn visit_primary_mut(&mut self, primary: &mut Primary) {
        if let PrimaryKind::Binding(ident, binding) = &mut primary.kind {
            let Some(new) = self.resolve(ident.inner) else {
                self.errors.push((*ident, ErrorType::Missing));
                return;
            };

            *binding = Some(new);
        }
    }

    fn visit_let_statement_mut(&mut self, stmt: &mut LetStatement) {
        if let Some(ty) = &mut stmt.ty {
            ty.accept_mut(self);
        }
        if let Some(expr) = &mut stmt.expr {
            expr.accept_mut(self);
        }

        self.define(
            stmt.ident.inner,
            Binding {
                symbol: stmt.ident,
                ty_annotation: stmt.ty.as_ref().map(|expr| expr.id),
            },
        );
    }
}

/// Resolve a program's bindings
pub fn resolve(prog: &mut Program, global: &Global) -> Vec<Diag> {
    let mut v = SymbolResolver::new(global);
    prog.accept_mut(&mut v);

    v.errors
        .into_iter()
        .map(|(symbol, error)| match error {
            ErrorType::Missing => Diag::with_span(
                format!(
                    "cannot find value `{}` in this scope",
                    symbol.display(global)
                ),
                symbol.span,
            ),
            ErrorType::Duplicated => Diag::with_span(
                format!(
                    "name `{}` defined multiple times in scope!",
                    symbol.display(global)
                ),
                symbol.span,
            ),
        })
        .collect::<Vec<_>>()
}
