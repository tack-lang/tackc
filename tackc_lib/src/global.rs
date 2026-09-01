//! The crate containing [`Global`], tackc's global context.

use std::fmt::Debug;

use crate::{file::FileList, utils::intern::Interner};

/// tackc's global context.
#[derive(Debug)]
pub struct Global {
    /// The global interner.
    pub interner: Interner,
    /// The global file list.
    file_list: FileList,
}

/// If this is `true`, a [`Global`] currently exists, and a new one should not be created.
/// This check is only enabled in debug mode.
#[cfg(debug_assertions)]
static GLOBAL_EXISTS: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

impl Global {
    /// Creates a new 'static `Global` by leaking it. Recomended for applications that compile entire files, and should hold one `Global` the entire time.
    /// Should only be called once during an entire program. For programs that need multiple `Global`s in a program, use [`Global::create_heap`].
    ///
    /// # Panics
    /// If `debug_assertions` is enabled, an extra check will be added.
    /// If this function is called more than once, that check will fail, and the function will panic.
    pub fn new() -> &'static mut Self {
        #[cfg(debug_assertions)]
        {
            use std::sync::atomic::{AtomicBool, Ordering};

            static USED: AtomicBool = AtomicBool::new(false);
            assert!(
                !USED.swap(true, Ordering::AcqRel),
                "`Global::new` should only be called once! If multiple `Global`s are needed in one program, use `Global::create_heap()`"
            );
        }

        Box::leak(Self::create_heap())
    }

    /// Creates a new `Global` on the heap. This `Global` is not 'static, in contrast to the `Global` created by [`Global::new`].
    /// If your program will only use one `Global`, and for the entire lifetime, use [`Global::new`].
    ///
    /// # Panics
    /// If a `Global` already exists, this function will panic.
    pub fn create_heap() -> Box<Self> {
        #[cfg(debug_assertions)]
        {
            use std::sync::atomic::Ordering;

            assert!(
                !GLOBAL_EXISTS.swap(true, Ordering::AcqRel),
                "Only one `Global` should exist at once!"
            );
        }

        Box::new(Self {
            interner: Interner::new(),
            file_list: FileList::default(),
        })
    }

    /// Sets the file list in this [`Global`].
    pub fn set_file_list(&mut self, file_list: FileList) {
        self.file_list = file_list;
    }

    /// Gets the file list of this [`Global`].
    pub const fn file_list(&self) -> &FileList {
        &self.file_list
    }
}

#[cfg(debug_assertions)]
impl Drop for Global {
    fn drop(&mut self) {
        use std::sync::atomic::Ordering;

        GLOBAL_EXISTS.store(false, Ordering::Release);
    }
}
