// A lot of code in this module makes more sense with match statements instead of `Option` methods.
#![expect(clippy::option_if_let_else)] // CHECKED(Chloe)
//! The module for AST nodes.

use std::{hash::Hash, num::NonZeroU32};

use serde::{Deserialize, Serialize};

use crate::{
    file::FileId, frontend::lexer::Token, global::Global, span::Span, utils::intern::Interned,
};

pub mod expr;
pub use expr::*;

pub mod item;
pub use item::*;

pub mod stmt;
pub use stmt::*;

pub mod block;
pub use block::*;

pub mod module;
pub use module::*;

/// A symbol, consisting of a interned string, a span, and a file ID.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Symbol(pub Interned<str>, pub Span);

impl Symbol {
    /// Displays the string of this symbol.
    pub fn display<'a>(&self, global: &'a Global) -> &'a str {
        self.0.get(&global.interner)
    }
}

impl Symbol {
    /// Creates a new symbol from a token, and a file ID.
    pub const fn new(tok: Token) -> Self {
        Self(tok.lexeme, tok.span)
    }
}

/// IDs for AST nodes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct NodeId {
    /// The numerical ID of this node.
    pub id: NonZeroU32,
    /// The file ID of this node.
    pub file: FileId,
}

impl PartialOrd for NodeId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NodeId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

/// A tri-state value for things that might not exist, or might have an error.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TriState<T> {
    /// Some type, contains a value.
    Some(T),
    /// None type, doesn't contain a value.
    None,
    /// Error type, shows an error occured.
    Error,
}

impl<T> TriState<T> {
    /// Converts an [`Option<T>`] to a [`TriState<T>`] by treating a [`None`] as an error.
    pub fn from_error(option: Option<T>) -> Self {
        match option {
            Some(value) => Self::Some(value),
            None => Self::Error,
        }
    }

    /// Converts an [`Option<T>`] to a [`TriState<T>`] by treating a [`None`] as [`TriState::None`].
    pub fn from_optional(option: Option<T>) -> Self {
        match option {
            Some(value) => Self::Some(value),
            None => Self::None,
        }
    }

    /// Converts a [`TriState<T>`] to a [`Option<T>`] by returning the value in the [`TriState::Some`] variant, if any.
    pub fn some(self) -> Option<T> {
        match self {
            Self::Some(val) => Some(val),
            Self::None | Self::Error => None,
        }
    }
}

/// Visitor for the AST.
pub trait AstVisitor<'src> {
    /// The function called when visiting a module.
    fn visit_module(&mut self, module: &'src AstModule) {
        if let Some(ref stmt) = module.mod_stmt {
            self.visit_mod_statement(stmt);
        }

        for item in module.items.iter().flatten() {
            self.visit_item(item);
        }
    }

    /// The function called when visiting a module statement.
    fn visit_mod_statement(&mut self, stmt: &'src ModStatement) {
        if let Some(ref path) = stmt.path {
            self.visit_path(path);
        }
    }

    /// The function called when visiting a path.
    fn visit_path(&mut self, path: &'src AstPath) {
        _ = path;
    }

    /// The function called when visiting an item.
    fn visit_item(&mut self, item: &'src Item) {
        match item.kind {
            ItemKind::ConstItem(ref item) => self.visit_const_item(item),
            ItemKind::FuncItem(ref item) => self.visit_func_item(item),
            ItemKind::ImpItem(ref item) => self.visit_imp_item(item),
        }
    }

    /// The function called when visiting a constant.
    fn visit_const_item(&mut self, item: &'src ConstItem) {
        if let TriState::Some(ref ty) = item.ty {
            self.visit_expression(ty);
        }

        if let Some(ref ty) = item.expr {
            self.visit_expression(ty);
        }
    }

    /// The function called when visiting a function.
    fn visit_func_item(&mut self, item: &'src FuncItem) {
        for i in item.params.iter().flat_map(|tuple| &tuple.1) {
            self.visit_expression(i);
        }
        if let TriState::Some(ref ty) = item.ret_type {
            self.visit_expression(ty);
        }
        if let Some(ref block) = item.block {
            self.visit_block(block);
        }
    }

    /// The function called when visiting a import.
    fn visit_imp_item(&mut self, item: &'src ImpItem) {
        if let Some(ref path) = item.path {
            self.visit_path(path);
        }
    }

    /// The function called when visiting an expression.
    fn visit_expression(&mut self, expression: &'src Expression) {
        match expression.kind {
            ExpressionKind::IntLit(_)
            | ExpressionKind::FloatLit(_)
            | ExpressionKind::StringLit(_)
            | ExpressionKind::Ident(_)
            | ExpressionKind::GlobalIdent(_) => {}
            ExpressionKind::Grouping(ref expr) => {
                expr.as_ref().inspect(|inner| self.visit_expression(inner));
            }
            ExpressionKind::Unary(_, ref expr) | ExpressionKind::Member(ref expr, _) => {
                self.visit_expression(expr);
            }
            ExpressionKind::Call(ref lhs, ref args) => {
                self.visit_expression(lhs);
                for arg in args.iter().flatten() {
                    self.visit_expression(arg);
                }
            }
            ExpressionKind::Index(ref lhs, ref rhs) => {
                self.visit_expression(lhs);
                if let Some(ref rhs) = *rhs {
                    self.visit_expression(rhs);
                }
            }
            ExpressionKind::Block(ref block) => self.visit_block(block),
            ExpressionKind::Binary(_, ref lhs, ref rhs) => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
            }
            ExpressionKind::Function(ref func) => self.visit_function(func),
            ExpressionKind::FunctionType(ref func_type) => self.visit_function_type(func_type),
        }
    }

    /// The function called when visiting a block.
    fn visit_block(&mut self, block: &'src Block) {
        for stmt in block.stmts.iter().flatten() {
            self.visit_statement(stmt);
        }
        if let TriState::Some(ref expr) = block.expr {
            self.visit_expression(expr);
        }
    }

    /// The function called when visiting a function expression.
    fn visit_function(&mut self, func: &'src Function) {
        for i in func.params.iter().flat_map(|tuple| &tuple.1) {
            self.visit_expression(i);
        }
        if let TriState::Some(ref ty) = func.ret_type {
            self.visit_expression(ty);
        }
        self.visit_block(&func.block);
    }

    /// The function called when visiting a function type.
    fn visit_function_type(&mut self, func: &'src FunctionType) {
        for i in func.params.iter().flatten() {
            self.visit_expression(i);
        }
        if let TriState::Some(ref ty) = func.ret_type {
            self.visit_expression(ty);
        }
    }

    /// The function called when visiting a statement.
    fn visit_statement(&mut self, stmt: &'src Statement) {
        match stmt.kind {
            StatementKind::LetStatement(ref stmt) => self.visit_let_statement(stmt),
            StatementKind::AssignmentStatement(ref stmt) => self.visit_assignment_statement(stmt),
            StatementKind::Item(ref item) => self.visit_item(item),
            StatementKind::ExpressionStatement(ref stmt) => self.visit_expression_statement(stmt),
        }
    }

    /// The function called when visiting a let statement.
    fn visit_let_statement(&mut self, stmt: &'src LetStatement) {
        if let TriState::Some(ref ty) = stmt.ty {
            self.visit_expression(ty);
        }
        if let TriState::Some(ref expr) = stmt.expr {
            self.visit_expression(expr);
        }
    }

    /// The function called when visiting an assignment statement.
    fn visit_assignment_statement(&mut self, stmt: &'src AssignmentStatement) {
        self.visit_expression(&stmt.lhs);
        if let Some(ref expr) = stmt.rhs {
            self.visit_expression(expr);
        }
    }

    /// The function called when visiting an expression statement.
    fn visit_expression_statement(&mut self, stmt: &'src ExpressionStatement) {
        self.visit_expression(&stmt.expr);
    }
}
