//! A crate containing structs for files.

use std::{
    borrow::Cow,
    collections::hash_map::Values,
    fs, io,
    num::NonZeroU32,
    ops::Deref,
    path::{Path, PathBuf},
};

use crate::span::SpanValue;
use crate::utils::{UnwrapExt, hash::NonZeroFxHasherBuilder};
use rustc_hash::FxHashMap;
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

insta_test!(
    line_starts_test,
    "line-starts-tests/*",
    run_line_starts_test
);

#[cfg(test)]
// Since this is used in insta_test!, we can't change the signature.
#[expect(clippy::needless_pass_by_value)] // CHECKED(Chloe)
fn run_line_starts_test(str: String) {
    let starts = line_starts(&str);
    insta::assert_ron_snapshot!(starts);
}

/// The ID that files use to identify themselves.
pub type FileId = NonZeroU32;

/// A file in tackc.
#[derive(Debug, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct File {
    src: Cow<'static, str>,
    path: Cow<'static, Path>,
    line_starts: Vec<SpanValue>,
    id: FileId,
}

impl File {
    /// Create a new [`File`] from a source and a path. To open a file at a path, use [`File::try_from`].
    pub fn new<S: Into<Cow<'static, str>>, P: Into<Cow<'static, Path>>>(src: S, path: P) -> Self {
        let src = src.into();
        let path = path.into();
        let line_starts = line_starts(&src);
        let id = NonZeroFxHasherBuilder.hash_one_non_zero_truncated((&src, &path));
        Self {
            src,
            path,
            line_starts,
            id,
        }
    }
}

impl File {
    /// Get the file's source.
    pub fn src(&self) -> &str {
        &self.src
    }

    /// Get the file's path.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Get the file's ID. Should be unique to any other files.
    pub const fn id(&self) -> FileId {
        self.id
    }

    /// Find the line and column numbers of an index using the given line starts.
    ///
    /// # Panics
    /// This function panics if the input index is greater than the file length.
    ///
    /// # Returns
    /// This function only returns [`None`] if one of the outputs would have been greater than [`SpanValue::MAX`].
    pub fn line_and_column(&self, index: SpanValue) -> Option<(SpanValue, SpanValue)> {
        let starts = &self.line_starts;

        let src_len = self.len();
        assert!(
            (index as usize) < src_len,
            "index is greater than file length!"
        );

        // find the last line start that is <= index
        let line_idx = starts.iter().rposition(|&start| start <= index)?;

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

impl Deref for File {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.src()
    }
}

impl TryFrom<PathBuf> for File {
    type Error = io::Error;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        let src = fs::read_to_string(&path)?;
        Ok(Self::new(src, path))
    }
}

impl TryFrom<&'static Path> for File {
    type Error = io::Error;

    fn try_from(path: &'static Path) -> Result<Self, Self::Error> {
        let src = fs::read_to_string(path)?;
        Ok(Self::new(src, path))
    }
}

/// A map of [`FileId`]s to [`File`].
#[derive(Debug, Default)]
pub struct FileList {
    files: FxHashMap<FileId, File>,
}

impl FileList {
    /// Creates a new [`FileList`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a file to the list, and returns a reference to it.
    pub fn add(&mut self, file: File) {
        let id = file.id();
        self.files.insert(id, file);
    }

    /// Gets a file from the list.
    pub fn get(&self, file_id: FileId) -> Option<&File> {
        self.files.get(&file_id)
    }

    /// Checks if a file is contained in the list.
    pub fn contains(&self, file_id: FileId) -> bool {
        self.files.contains_key(&file_id)
    }

    /// Creates an iterator from the list.
    pub fn iter(&self) -> Values<'_, FileId, File> {
        self.files.values()
    }

    /// Creates a [`FileList`] from a single [`File`].
    pub fn from_one(file: File) -> Self {
        let mut file_list = Self::new();
        file_list.add(file);
        file_list
    }
}

impl From<Vec<File>> for FileList {
    fn from(value: Vec<File>) -> Self {
        value.into_iter().collect()
    }
}

impl<'a> IntoIterator for &'a FileList {
    type Item = &'a File;
    type IntoIter = Values<'a, FileId, File>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl FromIterator<File> for FileList {
    fn from_iter<T: IntoIterator<Item = File>>(iter: T) -> Self {
        Self {
            files: iter.into_iter().map(|file| (file.id(), file)).collect(),
        }
    }
}
