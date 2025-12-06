use std::{
    fs, io,
    ops::Deref,
    path::{Path, PathBuf},
};

use tackc_span::SpanValue;

pub trait File: Deref<Target = str> {
    fn src(&self) -> &str;
    fn path(&self) -> &Path;
    fn line_starts(&self) -> &[SpanValue];

    fn line_and_column(&self, index: SpanValue) -> Option<(SpanValue, SpanValue)> {
        let starts = self.line_starts();
        if starts.is_empty() {
            return None;
        }

        let src_len = self.src().len();
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
        out.push((i + 1).try_into().unwrap());
    }

    out
}

#[derive(Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OwnedFile {
    src: String,
    path: PathBuf,
    line_starts: Vec<SpanValue>,
}

impl File for OwnedFile {
    fn src(&self) -> &str {
        &self.src
    }

    fn path(&self) -> &Path {
        &self.path
    }

    fn line_starts(&self) -> &[SpanValue] {
        &self.line_starts
    }
}

impl TryFrom<PathBuf> for OwnedFile {
    type Error = io::Error;

    fn try_from(path: PathBuf) -> io::Result<Self> {
        let src = fs::read_to_string(&path)?;
        Ok(OwnedFile {
            line_starts: line_starts(&src),
            src,
            path,
        })
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
    path: &'static Path,
    line_starts: Vec<SpanValue>,
}

impl<'src> BorrowedFile<'src> {
    pub fn new<P: AsRef<Path> + ?Sized>(src: &'src str, path: &'static P) -> Self {
        BorrowedFile {
            src,
            path: path.as_ref(),
            line_starts: line_starts(src),
        }
    }
}

impl File for BorrowedFile<'_> {
    fn src(&self) -> &str {
        self.src
    }

    fn path(&self) -> &Path {
        self.path
    }

    fn line_starts(&self) -> &[SpanValue] {
        &self.line_starts
    }
}

impl Deref for BorrowedFile<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.src()
    }
}
