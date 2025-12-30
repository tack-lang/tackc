use std::num::NonZeroU32;

use serde::{Deserialize, Serialize};
use tackc_global::{Global, Interned};
use tackc_lexer::Token;
use tackc_span::Span;

pub mod expr;
pub use expr::*;

pub mod item;
pub use item::*;

pub mod stmt;
pub use stmt::*;

pub mod block;
pub use block::*;

pub mod prog;
pub use prog::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Symbol(pub Interned<str>, pub Span);

impl Symbol {
    pub fn display<'a>(&self, global: &'a Global) -> &'a str {
        self.0.display(global)
    }
}

impl From<Token> for Symbol {
    fn from(value: Token) -> Self {
        Self(value.lexeme, value.span)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct NodeId {
    pub id: NonZeroU32,
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

pub trait AstVisitor {
    fn visit_program(&mut self, prog: &Program) {
        if let Some(stmt) = &prog.mod_stmt {
            self.visit_mod_statement(stmt);
        }

        for item in prog.items.iter().flatten() {
            self.visit_item(item);
        }
    }

    fn visit_mod_statement(&mut self, stmt: &ModStatement) {
        if let Some(path) = &stmt.path {
            self.visit_path(path);
        }
    }

    fn visit_path(&mut self, path: &AstPath) {
        _ = path;
    }

    fn visit_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::ConstItem(item) => self.visit_const_item(item),
            ItemKind::FuncItem(item) => self.visit_func_item(item),
            ItemKind::ImpItem(item) => self.visit_imp_item(item),
        }
    }

    fn visit_const_item(&mut self, item: &ConstItem) {
        if let Some(Some(ty)) = &item.ty {
            self.visit_expression(ty);
        }

        if let Some(ty) = &item.expr {
            self.visit_expression(ty);
        }
    }

    fn visit_func_item(&mut self, item: &FuncItem) {
        for i in item.params.iter().flat_map(|tuple| &tuple.1) {
            self.visit_expression(i);
        }
        if let Some(Some(ty)) = &item.ret_type {
            self.visit_expression(ty);
        }
    }

    fn visit_imp_item(&mut self, item: &ImpItem) {
        if let Some(path) = &item.path {
            self.visit_path(path);
        }
    }

    fn visit_expression(&mut self, expression: &Expression) {
        match &expression.kind {
            ExpressionKind::IntLit(_)
            | ExpressionKind::FloatLit(_)
            | ExpressionKind::StringLit(_)
            | ExpressionKind::Ident(_) => {}
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

    fn visit_block(&mut self, block: &Block) {
        for stmt in block.stmts.iter().flatten() {
            self.visit_statement(stmt);
        }
        if let Some(Some(expr)) = &block.expr {
            self.visit_expression(expr);
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        match &stmt.kind {
            StatementKind::LetStatement(stmt) => self.visit_let_statement(stmt),
            StatementKind::AssignmentStatement(stmt) => self.visit_assignment_statement(stmt),
            StatementKind::Item(item) => self.visit_item(item),
            StatementKind::ExpressionStatement(stmt) => self.visit_expression_statement(stmt),
        }
    }

    fn visit_let_statement(&mut self, stmt: &LetStatement) {
        if let Some(Some(ty)) = &stmt.ty {
            self.visit_expression(ty);
        }
        if let Some(Some(expr)) = &stmt.expr {
            self.visit_expression(expr);
        }
    }

    fn visit_assignment_statement(&mut self, stmt: &AssignmentStatement) {
        self.visit_expression(&stmt.lhs);
        if let Some(expr) = &stmt.rhs {
            self.visit_expression(expr);
        }
    }

    fn visit_expression_statement(&mut self, stmt: &ExpressionStatement) {
        self.visit_expression(&stmt.expr);
    }
}
