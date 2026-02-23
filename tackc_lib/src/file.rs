//! A crate containing structs for files.

use std::{
    borrow::Cow,
    fs, io,
    num::NonZeroU32,
    ops::Deref,
    path::{Path, PathBuf},
};

use crate::span::SpanValue;
use crate::utils::{UnwrapExt, hash::NonZeroFxHasherBuilder};
use serde::{Deserialize, Serialize};

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

        // Since `str.len() <= SpanValue::MAX`, and `i` will only ever be as large as `str.len() - 1`,
        // i will only ever be as large as `str.len()`, which `try_into()` won't return `Err` on.
        out.push((i + 1).try_into().expect_unreachable()); // CHECKED(Chloe)
    }

    out
}

/// A file in tackc.
#[derive(Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct File<'a> {
    src: Cow<'a, str>,
    path: Cow<'a, Path>,
    line_starts: Vec<SpanValue>,
    id: NonZeroU32,
}

impl<'a> File<'a> {
    /// Create a new [`File`] from a source and a path. To open a file at a path, use [`File::try_from`].
    pub fn new<S: Into<Cow<'a, str>>, P: Into<Cow<'a, Path>>>(src: S, path: P) -> Self {
        let src = src.into();
        let path = path.into();
        let line_starts = line_starts(&src);
        let id = NonZeroFxHasherBuilder.hash_one_non_zero_truncated((&src, &path));
        File {
            src,
            path,
            line_starts,
            id,
        }
    }
}

impl File<'_> {
    /// Get the file's source.
    pub fn src(&self) -> &str {
        &self.src
    }

    /// Get the file's path.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Get the file's ID. Should be unique to any other files.
    pub const fn id(&self) -> NonZeroU32 {
        self.id
    }

    /// Find the line and column numbers of an index using the given line starts.
    pub fn line_and_column(&self, index: SpanValue) -> Option<(SpanValue, SpanValue)> {
        let starts = &self.line_starts;
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
        // `line_idx` is only as large as `starts.len()`
        // because `rposition`'s return value is only as
        // long as it's iterator, which in this case,
        // is only as long as `starts`.
        let col_index = *starts.get(line_idx).expect_unreachable() as SpanValue; // CHECKED(Chloe)
        let col: SpanValue = index - (col_index) + 1;

        Some((line_num, col))
    }
}

impl Deref for File<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.src
    }
}

impl TryFrom<PathBuf> for File<'_> {
    type Error = io::Error;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        let src = fs::read_to_string(&path)?;
        Ok(Self::new(src, path))
    }
}

impl<'a> TryFrom<&'a Path> for File<'a> {
    type Error = io::Error;

    fn try_from(path: &'a Path) -> Result<Self, Self::Error> {
        let src = fs::read_to_string(path)?;
        Ok(Self::new(src, path))
    }
}
