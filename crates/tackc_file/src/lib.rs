//! A crate containing structs for files.

use std::{
    borrow::Cow,
    fs,
    hash::{BuildHasher, BuildHasherDefault},
    io,
    ops::Deref,
    path::{Path, PathBuf},
};

use rustc_stable_hash::hashers::StableSipHasher128;

type StableSipHasher128Builder = BuildHasherDefault<StableSipHasher128>;

use serde::{Deserialize, Serialize};
use tackc_span::SpanValue;

/// The main trait of `tackc_file`.
/// To implement this trait, a source, path, and line starts must be provided. Line starts can be calculated by the [`line_starts`] function.
pub trait File: Deref<Target = str> {
    /// Get the file's source.
    fn src(&self) -> &str;
    /// Get the file's path.
    fn path(&self) -> &Path;
    /// Get the file's line starts.
    fn line_starts(&self) -> &[SpanValue];
    /// Get the file's ID. Should be unique to any other files.
    fn id(&self) -> u64;

    /// Find the line and column numbers of an index using the given line starts.
    fn line_and_column(&self, index: SpanValue) -> Option<(SpanValue, SpanValue)> {
        let starts = self.line_starts();
        if starts.is_empty() {
            return None;
        }

        let src_len = self.len();
        // reject out-of-range indexes
        if (index as usize) > src_len {
            return None;
        }

        // find the last line start that is <= index
        let line_idx = starts.iter().rposition(|&s| s <= index)?;

        let line_num: SpanValue = (line_idx + 1).try_into().ok()?;
        let col: SpanValue = index - (starts[line_idx] as SpanValue) + 1;

        Some((line_num, col))
    }
}

/// Returns a vector corresponding to the byte indexes of the start of each line.
///
/// # Panics
/// This function will panic if the input has a length greater than [`SpanValue::MAX`].
pub fn line_starts(str: &str) -> Vec<SpanValue> {
    assert!(str.len() <= SpanValue::MAX as usize, "string is too long!");

    let mut out = vec![0];
    let mut bytes = str.bytes().enumerate().peekable();
    while let Some((i, b)) = bytes.next() {
        match b {
            b'\n' => {}
            b'\r' => {
                if let Some((_, b'\n')) = bytes.peek() {
                    bytes.next();
                }
            }
            _ => continue,
        }
        #[allow(clippy::cast_possible_truncation)]
        out.push((i + 1) as SpanValue);
    }

    out
}

/// A simple implementation of file that uses a [`Cow`] to store either a borrowed or owned path/source, and the hash of the source and path as the ID.
#[derive(Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct BasicFile<'a> {
    src: Cow<'a, str>,
    path: Cow<'a, Path>,
    line_starts: Vec<SpanValue>,
    id: u64,
}

impl<'a> BasicFile<'a> {
    /// Create a new [`BasicFile`] from a source and a path. To open a file at a path, use [`BasicFile::try_from`].
    pub fn new<S: Into<Cow<'a, str>>, P: Into<Cow<'a, Path>>>(src: S, path: P) -> Self {
        let src = src.into();
        let path = path.into();
        let line_starts = line_starts(&src);
        let id = StableSipHasher128Builder::default().hash_one((&src, &path));
        BasicFile {
            src,
            path,
            line_starts,
            id,
        }
    }
}

impl File for BasicFile<'_> {
    fn src(&self) -> &str {
        &self.src
    }

    fn path(&self) -> &Path {
        &self.path
    }

    fn line_starts(&self) -> &[SpanValue] {
        &self.line_starts
    }

    fn id(&self) -> u64 {
        self.id
    }
}

impl Deref for BasicFile<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.src
    }
}

impl TryFrom<PathBuf> for BasicFile<'_> {
    type Error = io::Error;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        let src = fs::read_to_string(&path)?;
        Ok(Self::new(src, path))
    }
}

impl<'a> TryFrom<&'a Path> for BasicFile<'a> {
    type Error = io::Error;

    fn try_from(path: &'a Path) -> Result<Self, Self::Error> {
        let src = fs::read_to_string(path)?;
        Ok(Self::new(src, path))
    }
}
