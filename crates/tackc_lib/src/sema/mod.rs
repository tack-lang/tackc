use std::{borrow::Cow, collections::HashMap, fmt::Write};

use thin_vec::ThinVec;

use crate::{
    ast::{AstPath, Item, NodeId},
    global::{Global, Interned},
    hash::IdentityHashMap,
    span::Span,
    utils::tree::TreeItem,
};

pub mod module_resolution;
pub mod name_resolution;

#[derive(Debug)]
pub struct LogicalModule {
    pub submodules: IdentityHashMap<Interned<str>, Self>,
    pub items: ThinVec<Option<Item>>,
    pub spans: HashMap<NodeId, Span>,
    pub exported: bool,
    pub path: LogicalPath,
}

impl TreeItem for LogicalModule {
    fn name<'a>(&'a self, global: &'a Global) -> Cow<'a, str> {
        ((if self.exported { "exp " } else { "" }).to_string()
            + self.path.comps.last().unwrap().display(global))
        .into()
    }

    fn children(&self) -> Vec<&'_ dyn TreeItem> {
        self.submodules
            .values()
            .map(|module| module as &dyn TreeItem)
            .chain(
                self.items
                    .iter()
                    .filter_map(|opt| opt.as_ref())
                    .map(|item| item as &dyn TreeItem),
            )
            .collect()
    }
}

impl LogicalModule {
    fn new(path: LogicalPath) -> Self {
        Self {
            submodules: IdentityHashMap::default(),
            items: ThinVec::new(),
            spans: HashMap::new(),
            exported: true,
            path,
        }
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn display(&self, global: &Global) -> String {
        let exp = if self.exported { "exp " } else { "" };
        let name = self.path.comps.last().unwrap().display(global);

        let mut content = String::new();

        let mut i = 0;
        let len = self.submodules.len()
            + self
                .items
                .iter()
                .filter(|opt| Option::is_some(*opt))
                .count();

        for module in self.submodules.values() {
            i += 1;

            let module_display = module.display(global);

            if i == len {
                _ = write!(content, "\n+-- {}", module_display.replace('\n', "\n    "));
            } else {
                _ = write!(content, "\n+-- {}", module_display.replace('\n', "\n|   "));
            }
        }

        for item in &self.items {
            let Some(item) = item else { continue };

            _ = write!(content, "\n+-- {}", item.display_ident(global));
        }

        format!("{exp}{name}{content}")
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
