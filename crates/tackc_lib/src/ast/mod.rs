//! The module for AST nodes.

use std::num::NonZeroU32;

use crate::global::{Global, Interned};
use crate::lexer::Token;
use crate::span::Span;
use serde::{Deserialize, Serialize};

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
pub struct Symbol(pub Interned<str>, pub Span, pub NonZeroU32);

impl Symbol {
    /// Displays the string of this symbol.
    pub fn display<'a>(&self, global: &'a Global) -> &'a str {
        self.0.display(global)
    }
}

impl Symbol {
    /// Creates a new symbol from a token, and a file ID.
    pub const fn new(tok: Token, file: NonZeroU32) -> Self {
        Self(tok.lexeme, tok.span, file)
    }
}

/// IDs for AST nodes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct NodeId {
    /// The numerical ID of this node.
    pub id: NonZeroU32,
    /// The file ID of this node.
    pub file: NonZeroU32,
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

/// Visitor for the AST.
pub trait AstVisitor {
    /// The function called when visiting a module.
    fn visit_module(&mut self, module: &AstModule) {
        if let Some(stmt) = &module.mod_stmt {
            self.visit_mod_statement(stmt);
        }

        for item in module.items.iter().flatten() {
            self.visit_item(item);
        }
    }

    /// The function called when visiting a module statement.
    fn visit_mod_statement(&mut self, stmt: &ModStatement) {
        if let Some(path) = &stmt.path {
            self.visit_path(path);
        }
    }

    /// The function called when visiting a path.
    fn visit_path(&mut self, path: &AstPath) {
        _ = path;
    }

    /// The function called when visiting an item.
    fn visit_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::ConstItem(item) => self.visit_const_item(item),
            ItemKind::FuncItem(item) => self.visit_func_item(item),
            ItemKind::ImpItem(item) => self.visit_imp_item(item),
        }
    }

    /// The function called when visiting a constant.
    fn visit_const_item(&mut self, item: &ConstItem) {
        if let Some(Some(ty)) = &item.ty {
            self.visit_expression(ty);
        }

        if let Some(ty) = &item.expr {
            self.visit_expression(ty);
        }
    }

    /// The function called when visiting a function.
    fn visit_func_item(&mut self, item: &FuncItem) {
        for i in item.params.iter().flat_map(|tuple| &tuple.1) {
            self.visit_expression(i);
        }
        if let Some(Some(ty)) = &item.ret_type {
            self.visit_expression(ty);
        }
        if let Some(block) = &item.block {
            self.visit_block(block);
        }
    }

    /// The function called when visiting a import.
    fn visit_imp_item(&mut self, item: &ImpItem) {
        if let Some(path) = &item.path {
            self.visit_path(path);
        }
    }

    /// The function called when visiting an expression.
    fn visit_expression(&mut self, expression: &Expression) {
        match &expression.kind {
            ExpressionKind::IntLit(_)
            | ExpressionKind::FloatLit(_)
            | ExpressionKind::StringLit(_)
            | ExpressionKind::Ident(_)
            | ExpressionKind::GlobalIdent(_) => {}
            ExpressionKind::Grouping(expr) => {
                expr.as_ref().inspect(|inner| self.visit_expression(inner));
            }
            ExpressionKind::Unary(_, expr) | ExpressionKind::Member(expr, _) => {
                self.visit_expression(expr);
            }
            ExpressionKind::Call(lhs, args) => {
                self.visit_expression(lhs);
                for arg in args.iter().flatten() {
                    self.visit_expression(arg);
                }
            }
            ExpressionKind::Index(lhs, rhs) => {
                self.visit_expression(lhs);
                if let Some(rhs) = rhs {
                    self.visit_expression(rhs);
                }
            }
            ExpressionKind::Block(block) => self.visit_block(block),
            ExpressionKind::Binary(_, lhs, rhs) => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
            }
        }
    }

    /// The function called when visiting a block.
    fn visit_block(&mut self, block: &Block) {
        for stmt in block.stmts.iter().flatten() {
            self.visit_statement(stmt);
        }
        if let Some(Some(expr)) = &block.expr {
            self.visit_expression(expr);
        }
    }

    /// The function called when visiting a statement.
    fn visit_statement(&mut self, stmt: &Statement) {
        match &stmt.kind {
            StatementKind::LetStatement(stmt) => self.visit_let_statement(stmt),
            StatementKind::AssignmentStatement(stmt) => self.visit_assignment_statement(stmt),
            StatementKind::Item(item) => self.visit_item(item),
            StatementKind::ExpressionStatement(stmt) => self.visit_expression_statement(stmt),
        }
    }

    /// The function called when visiting a let statement.
    fn visit_let_statement(&mut self, stmt: &LetStatement) {
        if let Some(Some(ty)) = &stmt.ty {
            self.visit_expression(ty);
        }
        if let Some(Some(expr)) = &stmt.expr {
            self.visit_expression(expr);
        }
    }

    /// The function called when visiting an assignment statement.
    fn visit_assignment_statement(&mut self, stmt: &AssignmentStatement) {
        self.visit_expression(&stmt.lhs);
        if let Some(expr) = &stmt.rhs {
            self.visit_expression(expr);
        }
    }

    /// The function called when visiting an expression statement.
    fn visit_expression_statement(&mut self, stmt: &ExpressionStatement) {
        self.visit_expression(&stmt.expr);
    }
}
