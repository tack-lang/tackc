use std::hash::Hash;
use std::{any::Any, fmt::Debug};

use crate::error::Result;
use serde::{Deserialize, Serialize};
use tackc_file::File;
use tackc_global::Global;
use tackc_lexer::Token;
use tackc_span::Span;

use crate::Parser;

/// Returns a closure that takes one value, and returns whether the value matches the given pattern.
#[macro_export]
macro_rules! kind {
    ($pat:pat) => {
        |kind| matches!(kind, $pat)
    };
}

/// This trait is implemented by all AST nodes.
pub trait AstNode:
    Debug + PartialEq + Eq + Hash + Sized + Serialize + for<'a> Deserialize<'a> + Any
{
    /// Parse the AST node using the given parser.
    ///
    /// # Errors
    /// If the node cannot be parsed from the given parser, this function will return an error.
    /// The parser may be in any state; do not trust it.
    /// If you need to, the parser can be cloned previous to calling this function.
    fn parse<I, F: File>(p: &mut Parser<I, F>, recursion: u32) -> Result<Self>
    where
        I: Iterator<Item = Token> + Clone;
    /// Get the span of the AST node
    fn span(&self) -> Span;
    /// Display the AST node
    fn display(&self, global: &Global) -> String;
    /// Get the ID of the AST node
    fn id(&self) -> NodeId;

    fn accept<V: Visitor + ?Sized>(&self, v: &mut V);
    fn accept_mut<V: VisitorMut + ?Sized>(&mut self, v: &mut V);
}

mod expr;
pub use expr::*;
mod block;
mod item;
mod prog;
mod stmt;
pub use prog::*;
mod prim;
mod util;
pub use tackc_ast::*;

pub trait Visitor {
    fn visit_program(&mut self, program: &Program) {
        program.mod_stmt.accept(self);
        for item in &program.items {
            item.accept(self);
        }
    }

    fn visit_mod_statement(&mut self, mod_statement: &ModStatement) {
        mod_statement.path.accept(self);
    }

    fn visit_path(&mut self, path: &Path) {
        let _ = path;
    }

    fn visit_const_item(&mut self, item: &ConstItem) {
        if let Some(ty) = &item.ty {
            ty.accept(self);
        }
        item.expr.accept(self);
    }

    fn visit_func_item(&mut self, item: &FuncItem) {
        for (_, ty, _) in &item.params {
            ty.accept(self);
        }
        if let Some(ty) = &item.ret_ty {
            ty.accept(self);
        }
        item.block.accept(self);
    }

    fn visit_expression(&mut self, expr: &Expression) {
        match &expr.kind {
            ExpressionKind::Add(lhs, rhs)
            | ExpressionKind::Sub(lhs, rhs)
            | ExpressionKind::Mul(lhs, rhs)
            | ExpressionKind::Div(lhs, rhs)
            | ExpressionKind::Index(lhs, rhs)
            | ExpressionKind::Equal(lhs, rhs)
            | ExpressionKind::NotEqual(lhs, rhs)
            | ExpressionKind::Gt(lhs, rhs)
            | ExpressionKind::Lt(lhs, rhs)
            | ExpressionKind::GtEq(lhs, rhs)
            | ExpressionKind::LtEq(lhs, rhs) => {
                lhs.accept(self);
                rhs.accept(self);
            }
            ExpressionKind::Grouping(lhs) | ExpressionKind::Neg(lhs) => lhs.accept(self),
            ExpressionKind::Call(lhs, args) => {
                lhs.accept(self);
                for arg in args {
                    arg.accept(self);
                }
            }
            ExpressionKind::Member(lhs, _symbol) => lhs.accept(self),
            ExpressionKind::Primary(primary) => primary.accept(self),
            ExpressionKind::Block(block) => block.accept(self),
        }
    }

    fn visit_primary(&mut self, primary: &Primary) {
        match primary.kind {
            PrimaryKind::Binding(_, _)
            | PrimaryKind::IntLit(_, _)
            | PrimaryKind::FloatLit(_)
            | PrimaryKind::U8
            | PrimaryKind::U16
            | PrimaryKind::U32
            | PrimaryKind::U64
            | PrimaryKind::I8
            | PrimaryKind::I16
            | PrimaryKind::I32
            | PrimaryKind::I64 => {}
        }
    }

    fn visit_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            stmt.accept(self);
        }

        if let Some(expr) = &block.expr {
            expr.accept(self);
        }
    }

    fn visit_expression_statement(&mut self, stmt: &ExpressionStatement) {
        stmt.inner.accept(self);
    }

    fn visit_let_statement(&mut self, stmt: &LetStatement) {
        if let Some(ty) = &stmt.ty {
            ty.accept(self);
        }
        if let Some(expr) = &stmt.expr {
            expr.accept(self);
        }
    }

    fn visit_assignment_statement(&mut self, stmt: &AssignmentStatement) {
        stmt.lvalue.accept(self);
        stmt.rvalue.accept(self);
    }
}

pub trait VisitorMut {
    fn visit_program_mut(&mut self, program: &mut Program) {
        program.mod_stmt.accept_mut(self);
        for item in &mut program.items {
            item.accept_mut(self);
        }
    }

    fn visit_mod_statement_mut(&mut self, mod_statement: &mut ModStatement) {
        mod_statement.path.accept_mut(self);
    }

    fn visit_path_mut(&mut self, path: &mut Path) {
        let _ = path;
    }

    fn visit_const_item_mut(&mut self, item: &mut ConstItem) {
        if let Some(ty) = &mut item.ty {
            ty.accept_mut(self);
        }
        item.expr.accept_mut(self);
    }

    fn visit_func_item_mut(&mut self, item: &mut FuncItem) {
        for (_, ty, _) in &mut item.params {
            ty.accept_mut(self);
        }
        if let Some(ty) = &mut item.ret_ty {
            ty.accept_mut(self);
        }
        item.block.accept_mut(self);
    }

    fn visit_expression_mut(&mut self, expr: &mut Expression) {
        match &mut expr.kind {
            ExpressionKind::Add(lhs, rhs)
            | ExpressionKind::Sub(lhs, rhs)
            | ExpressionKind::Mul(lhs, rhs)
            | ExpressionKind::Div(lhs, rhs)
            | ExpressionKind::Index(lhs, rhs)
            | ExpressionKind::Equal(lhs, rhs)
            | ExpressionKind::NotEqual(lhs, rhs)
            | ExpressionKind::Gt(lhs, rhs)
            | ExpressionKind::Lt(lhs, rhs)
            | ExpressionKind::GtEq(lhs, rhs)
            | ExpressionKind::LtEq(lhs, rhs) => {
                lhs.accept_mut(self);
                rhs.accept_mut(self);
            }
            ExpressionKind::Grouping(lhs) | ExpressionKind::Neg(lhs) => lhs.accept_mut(self),
            ExpressionKind::Call(lhs, args) => {
                lhs.accept_mut(self);
                for arg in args {
                    arg.accept_mut(self);
                }
            }
            ExpressionKind::Member(lhs, _symbol) => lhs.accept_mut(self),
            ExpressionKind::Primary(primary) => primary.accept_mut(self),
            ExpressionKind::Block(block) => block.accept_mut(self),
        }
    }

    fn visit_primary_mut(&mut self, primary: &mut Primary) {
        match primary.kind {
            PrimaryKind::Binding(_, _)
            | PrimaryKind::IntLit(_, _)
            | PrimaryKind::FloatLit(_)
            | PrimaryKind::U8
            | PrimaryKind::U16
            | PrimaryKind::U32
            | PrimaryKind::U64
            | PrimaryKind::I8
            | PrimaryKind::I16
            | PrimaryKind::I32
            | PrimaryKind::I64 => {}
        }
    }

    fn visit_block_mut(&mut self, block: &mut Block) {
        for stmt in &mut block.stmts {
            stmt.accept_mut(self);
        }

        if let Some(expr) = &mut block.expr {
            expr.accept_mut(self);
        }
    }

    fn visit_expression_statement_mut(&mut self, stmt: &mut ExpressionStatement) {
        stmt.inner.accept_mut(self);
    }

    fn visit_let_statement_mut(&mut self, stmt: &mut LetStatement) {
        if let Some(ty) = &mut stmt.ty {
            ty.accept_mut(self);
        }
        if let Some(expr) = &mut stmt.expr {
            expr.accept_mut(self);
        }
    }

    fn visit_assignment_statement_mut(&mut self, stmt: &mut AssignmentStatement) {
        stmt.lvalue.accept_mut(self);
        stmt.rvalue.accept_mut(self);
    }
}
