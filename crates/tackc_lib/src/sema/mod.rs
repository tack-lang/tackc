use std::{cmp::Ordering, collections::HashMap};

use thin_vec::ThinVec;

use crate::{
    ast::{AstPath, Item, NodeId},
    global::{Global, Interned},
    span::Span,
};

pub mod module_resolution;
pub mod name_resolution;

#[derive(Debug)]
pub struct LogicalModule {
    pub items: ThinVec<Option<Item>>,
    pub spans: HashMap<NodeId, Span>,
    pub exported: bool,
    pub path: LogicalPath,
}

impl LogicalModule {
    fn new(path: LogicalPath) -> Self {
        Self {
            items: ThinVec::new(),
            spans: HashMap::new(),
            exported: true,
            path,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct LogicalPath {
    comps: ThinVec<Interned<str>>,
}

impl LogicalPath {
    pub fn push(&mut self, symbol: Interned<str>) {
        self.comps.push(symbol);
    }

    pub fn display(&self, global: &Global) -> String {
        self.comps
            .iter()
            .map(|comp| comp.display(global))
            .collect::<Vec<_>>()
            .join(".")
    }
}

impl PartialOrd for LogicalPath {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LogicalPath {
    fn cmp(&self, other: &Self) -> Ordering {
        let mut iter = self.comps.iter();
        let mut other_iter = other.comps.iter();

        loop {
            match (iter.next(), other_iter.next()) {
                (Some(id), Some(other_id)) => {
                    if id == other_id {
                        continue;
                    }

                    break id.inner().cmp(&other_id.inner());
                }
                (None, Some(_)) => break Ordering::Less,
                (Some(_), None) => break Ordering::Greater,
                (None, None) => break Ordering::Equal,
            }
        }
    }
}

impl<const N: usize> From<[Interned<str>; N]> for LogicalPath {
    fn from(value: [Interned<str>; N]) -> Self {
        Self {
            comps: ThinVec::from(value),
        }
    }
}

impl TryFrom<AstPath> for LogicalPath {
    type Error = ();

    fn try_from(value: AstPath) -> Result<Self, Self::Error> {
        let mut path = Self::default();
        for comp in value.components {
            path.push(comp.ok_or(())?.0);
        }

        Ok(path)
    }
}
