use std::collections::HashMap;

use thin_vec::ThinVec;

use crate::{
    ast::{Item, NodeId},
    global::Interned,
    hash::IdentityHashMap,
    span::Span,
};

pub mod module_resolution;

#[derive(Debug)]
pub struct LogicalModule {
    pub submodules: IdentityHashMap<Interned<str>, Self>,
    pub items: ThinVec<Option<Item>>,
    pub spans: HashMap<NodeId, Span>,
    pub exported: bool,
}

impl Default for LogicalModule {
    fn default() -> Self {
        Self {
            submodules: IdentityHashMap::default(),
            items: ThinVec::new(),
            spans: HashMap::new(),
            exported: true,
        }
    }
}
