use std::{
    fs, io,
    ops::Deref,
    path::{Path, PathBuf},
};

use tackc_macros::Random;

pub trait File: Deref<Target = str> {
    fn src(&self) -> &str;
    fn path(&self) -> &Path;
}

#[derive(Debug, Hash, PartialEq, Eq, Random)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OwnedFile {
    src: String,
    path: PathBuf,
}

impl File for OwnedFile {
    fn src(&self) -> &str {
        &self.src
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl TryFrom<PathBuf> for OwnedFile {
    type Error = io::Error;

    fn try_from(path: PathBuf) -> io::Result<Self> {
        let src = fs::read_to_string(&path)?;
        Ok(OwnedFile { src, path })
    }
}

impl Deref for OwnedFile {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.src()
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct BorrowedFile<'src> {
    src: &'src str,
    path: &'src Path,
}

impl<'src> BorrowedFile<'src> {
    pub fn new(src: &'src str, path: &'src Path) -> Self {
        BorrowedFile { src, path }
    }
}

impl File for BorrowedFile<'_> {
    fn src(&self) -> &str {
        self.src
    }

    fn path(&self) -> &Path {
        self.path
    }
}

impl Deref for BorrowedFile<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.src()
    }
}
