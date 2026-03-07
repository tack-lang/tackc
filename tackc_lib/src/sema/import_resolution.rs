//! Import resolution, or turning all [`ImportBinding::Path`]s to [`ImportBinding::Binding`].

use crate::{
    error::Diag,
    global::{Global, Interned},
    sema::{Binding, BindingKind, BindingList, ImportBinding, LogicalPath},
    span::Span,
};

/// An error during import resolution.
pub enum ImportResolutionError {
    /// A dangling import.
    DanglingImport(LogicalPath, Span),
    /// A recursive import.
    RecursiveImport(LogicalPath, Span),
}

impl ImportResolutionError {
    /// Displays this error.
    pub fn display(&self, global: &Global) -> String {
        match self {
            Self::DanglingImport(path, span) => Diag::with_span(
                format!("path {} is a dangling import", path.display(global)),
                *span,
            )
            .display(global),
            Self::RecursiveImport(path, span) => Diag::with_span(
                format!("path {} starts a recursive import", path.display(global)),
                *span,
            )
            .display(global),
        }
    }
}

struct Resolver<'src> {
    global: &'src Global,
    bindings: &'src BindingList,
    errors: Vec<ImportResolutionError>,
}

impl Resolver<'_> {
    fn resolve_import(&mut self, binding: &mut ImportBinding) -> Option<Interned<Binding>> {
        match &*binding {
            ImportBinding::Path(path, span) => {
                let Some(pointed) = self.bindings.map.get(path) else {
                    // Error handling (dangling import)
                    self.errors
                        .push(ImportResolutionError::DanglingImport(*path, *span));
                    *binding = ImportBinding::Error;
                    return None;
                };
                let Some(mut kind) = pointed.get(self.global).kind.try_lock() else {
                    // Error handling (recursive import)
                    self.errors
                        .push(ImportResolutionError::RecursiveImport(*path, *span));
                    *binding = ImportBinding::Error;
                    return None;
                };
                let resolved = match &mut *kind {
                    BindingKind::Import(import_binding) => self.resolve_import(import_binding),
                    _ => Some(*pointed),
                };

                let import = resolved.map_or(ImportBinding::Error, ImportBinding::Binding);
                *binding = import;

                resolved
            }
            ImportBinding::Error => None,
            ImportBinding::Binding(binding) => Some(*binding),
        }
    }
}

/// Turns [`ImportBinding::Path`]s in a [`BindingList`] to [`ImportBinding::Binding`].
pub fn resolve_imports(bindings: &mut BindingList, global: &Global) -> Vec<ImportResolutionError> {
    let mut r = Resolver {
        global,
        bindings,
        errors: Vec::new(),
    };
    for binding in r.bindings.map.values() {
        if let BindingKind::Import(binding) = &mut *binding.get(global).kind.lock() {
            r.resolve_import(binding);
        }
    }

    r.errors
}
