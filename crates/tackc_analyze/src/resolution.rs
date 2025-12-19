use std::{collections::HashMap, mem};

use tackc_ast::{
    Binding, Block, Expression, ExpressionKind, FuncItem, Item, LetStatement, MaybeError,
    ModStatement, NodeId, Primary, PrimaryKind,
};
use tackc_error::Diag;
use tackc_file::File;
use tackc_global::{Global, Interned};
use tackc_parser::ast::{AstNode, Program, Symbol, VisitorMut};
use tackc_utils::hash::IdentityHasherBuilder;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ResolutionError {
    pub symbol: Symbol,
    pub kind: ErrorKind,
    pub node: NodeId,
}

impl ResolutionError {
    pub fn display<F: File>(&self, file: &F, global: &Global) -> String {
        let sym = self.symbol.display(global);
        let msg = match self.kind {
            ErrorKind::Missing => format!("symbol `{sym}` not found in current scope"),
            ErrorKind::Duplicated => {
                format!("symbol `{sym}` defined multiple times in current scope")
            }
        };
        let diag: Diag = Diag::with_span(msg, self.symbol.span);
        diag.display(file)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ErrorKind {
    /// The error returned when the given symbol cannot be found in the current scope.
    Missing,
    /// The error returned when a symbol is defined twice in the same scope.
    Duplicated,
}

type Scope = HashMap<Interned<str>, (Interned<Binding>, bool), IdentityHasherBuilder>;

struct SymbolResolver<'a> {
    global_scope: Scope,
    scopes: Vec<Scope>,
    errors: Vec<ResolutionError>,
    global: &'a Global,
    mod_binding: Option<Interned<Binding>>,
}

impl<'a> SymbolResolver<'a> {
    pub fn new(global: &'a Global) -> Self {
        SymbolResolver {
            global_scope: HashMap::default(),
            scopes: vec![],
            errors: Vec::new(),
            global,
            mod_binding: None,
        }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::default());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn resolve(&self, str: Interned<str>) -> Option<Interned<Binding>> {
        for scope in self.scopes.iter().rev() {
            if let Some((binding, enabled)) = scope.get(&str) {
                if !*enabled {
                    continue;
                }
                return Some(*binding);
            }
        }
        if let Some(binding) = self.mod_binding
            && let Some((binding, enabled)) =
                binding.get(self.global).mutable.read().fields.get(&str)
            && *enabled
        {
            return Some(*binding);
        }
        self.global_scope
            .get(&str)
            .filter(|(_, enabled)| *enabled)
            .map(|(binding, _)| *binding)
    }

    pub fn get_entry_mut<F: FnOnce(Option<&mut (Interned<Binding>, bool)>)>(
        &mut self,
        str: Interned<str>,
        op: F,
    ) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(&str) {
                op(Some(entry));
                return;
            }
        }
        if let Some(binding) = self.mod_binding
            && let Some(entry) = binding
                .get(self.global)
                .mutable
                .write()
                .fields
                .get_mut(&str)
        {
            op(Some(entry));
            return;
        }
        let entry = self
            .global_scope
            .get_mut(&str)
            .filter(|(_, enabled)| *enabled);
        op(entry);
    }

    pub fn disable(&mut self, str: Interned<str>) {
        println!("{:?}", self.global_scope);
        let str_display = str.display(self.global);
        self.get_entry_mut(str, |entry| {
            let (_, enabled) = entry
                .unwrap_or_else(|| panic!("cannot disable non-existant binding `{str_display}`!"));
            *enabled = false;
        });
    }

    pub fn enable(&mut self, str: Interned<str>) {
        let str_display = str.display(self.global);
        self.get_entry_mut(str, |entry| {
            let (_, enabled) = entry
                .unwrap_or_else(|| panic!("cannot disable non-existant binding `{str_display}`!"));
            *enabled = true;
        });
    }

    pub fn define(&mut self, str: Interned<str>, binding: Binding) -> Interned<Binding> {
        let interned = self.global.intern(binding);
        self.current_scope_mut().insert(str, (interned, true));
        self.current_scope().get(&str).unwrap().0
    }

    pub fn resolve_or_define(&mut self, str: Interned<str>, binding: Binding) -> Interned<Binding> {
        self.resolve(str)
            .unwrap_or_else(|| self.define(str, binding))
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().unwrap_or(&self.global_scope)
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap_or(&mut self.global_scope)
    }
}

impl SymbolResolver<'_> {
    fn register_item_to_global(&mut self, item: &mut Item, global_binding: Interned<Binding>) {
        let (symbol, exported, binding) = match item {
            Item::ConstItem(item) => (item.ident, item.exported, &mut item.binding),
            Item::FuncItem(item) => (item.ident, item.exported, &mut item.binding),
        };
        if !exported {
            return;
        }
        let new = self.global.intern(Binding::new(symbol));
        *binding = Some(new);
        let old = global_binding.get(self.global).add_field(symbol.inner, new);
        if old.is_some() {
            self.errors.push(ResolutionError {
                symbol,
                kind: ErrorKind::Duplicated,
                node: item.id(),
            });
        }
    }

    fn register_program_to_global(&mut self, prog: &mut Program) {
        println!("{}", prog.mod_stmt.display(self.global));
        let global = self.get_or_register_mod_statement_binding(&mut prog.mod_stmt);

        for item in &mut prog.items {
            self.register_item_to_global(item, global);
            println!("{}", global.get(self.global).display(self.global));
        }
    }

    fn get_or_register_mod_statement_binding(
        &mut self,
        mod_stmt: &mut ModStatement,
    ) -> Interned<Binding> {
        let ((symbol, binding), components) = mod_stmt
            .path
            .components
            .split_first_mut()
            .expect("`Path` requires one component!");

        let mut last = self.resolve_or_define(symbol.inner, Binding::new(*symbol));
        *binding = Some(last);
        for (symbol, binding) in components {
            let new = {
                let mut guard = last.get(self.global).mutable.write();

                let (interned, _) = *guard
                    .fields
                    .entry(symbol.inner)
                    .or_insert_with(|| (self.global.intern(Binding::new(*symbol)), true));
                interned
            };
            *binding = Some(new);
            last = new;
        }

        last
    }
}

impl VisitorMut for SymbolResolver<'_> {
    fn visit_func_item_mut(&mut self, item: &mut FuncItem) {
        let env = mem::take(&mut self.scopes);
        self.push();
        self.disable(item.ident.inner);

        for ty in item.params.iter_mut().filter_map(|(_, ty, _)| ty.as_mut()) {
            ty.accept_mut(self);
        }
        if let MaybeError::Some(ty) = &mut item.ret_ty {
            ty.accept_mut(self);
        }
        for (symbol, ty, binding) in &mut item.params {
            *binding = Some(self.define(
                symbol.inner,
                Binding::with_ty(*symbol, ty.as_ref().map(|ty| ty.id)),
            ));
        }
        item.block.accept_mut(self);

        self.enable(item.ident.inner);
        self.pop();
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
        self.push();
        self.mod_binding = Some(self.get_or_register_mod_statement_binding(&mut program.mod_stmt));

        for item in &mut program.items {
            item.accept_mut(self);
        }

        self.mod_binding = None;
        self.pop();
    }

    fn visit_primary_mut(&mut self, primary: &mut Primary) {
        if let PrimaryKind::Binding(symbol, binding) = &mut primary.kind {
            let Some(new) = self.resolve(symbol.inner) else {
                self.errors.push(ResolutionError {
                    symbol: *symbol,
                    kind: ErrorKind::Missing,
                    node: primary.id(),
                });
                return;
            };

            *binding = Some(new);
        }
    }

    fn visit_expression_mut(&mut self, expr: &mut Expression) {
        match &mut *expr.kind {
            ExpressionKind::Add(lhs, rhs)
            | ExpressionKind::Sub(lhs, rhs)
            | ExpressionKind::Mul(lhs, rhs)
            | ExpressionKind::Div(lhs, rhs)
            | ExpressionKind::Equal(lhs, rhs)
            | ExpressionKind::Index(lhs, rhs)
            | ExpressionKind::Gt(lhs, rhs)
            | ExpressionKind::Lt(lhs, rhs)
            | ExpressionKind::GtEq(lhs, rhs)
            | ExpressionKind::LtEq(lhs, rhs)
            | ExpressionKind::NotEqual(lhs, rhs) => {
                lhs.accept_mut(self);
                rhs.accept_mut(self);
            }
            ExpressionKind::Call(lhs, args) => {
                lhs.accept_mut(self);
                args.iter_mut()
                    .flatten()
                    .for_each(|arg| arg.accept_mut(self));
            }
            ExpressionKind::Grouping(lhs)
            | ExpressionKind::Neg(lhs)
            | ExpressionKind::Member(lhs, _) => lhs.accept_mut(self),
            ExpressionKind::Primary(primary) => primary.accept_mut(self),
            ExpressionKind::Block(block) => block.accept_mut(self),
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
            Binding::with_ty(stmt.ident, stmt.ty.as_ref().map(|expr| expr.id)),
        );
    }
}

/// Resolve a program's bindings
pub fn resolve(progs: &mut [Program], global: &Global) -> (Vec<ResolutionError>, Scope) {
    let mut resolver = SymbolResolver::new(global);
    for prog in progs.iter_mut() {
        resolver.register_program_to_global(prog);
    }

    for prog in progs.iter_mut() {
        prog.accept_mut(&mut resolver);
    }

    (resolver.errors, resolver.global_scope)
}
