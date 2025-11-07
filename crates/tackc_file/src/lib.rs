use std::{
    fs, io,
    ops::Deref,
    path::{Path, PathBuf},
};

use tackc_macros::Random;

#[derive(Debug, Hash, PartialEq, Eq, Random)]
pub struct File {
    src: String,
    path: PathBuf,
}

impl File {
    pub fn src(&self) -> &str {
        &self.src
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl TryFrom<PathBuf> for File {
    type Error = io::Error;

    fn try_from(path: PathBuf) -> io::Result<Self> {
        let src = fs::read_to_string(&path)?;
        Ok(File { src, path })
    }
}

impl Deref for File {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.src()
    }
}
