//! Various code for meta tackc binaries.

use std::{
    env, fs, io,
    path::{Path, PathBuf},
};

/// Change current working directory to the Tack project root.
///
/// Looks upward from the current directory until it finds a directory
/// containing `crates/tackc_*`.
pub fn chdir_to_tack_root() -> io::Result<PathBuf> {
    let mut dir = env::current_dir()?;

    loop {
        if is_tack_root(&dir) {
            env::set_current_dir(&dir)?;
            return Ok(dir);
        }

        // If we hit filesystem root and didn't find it -> fail.
        if !dir.pop() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                "Could not find Tack root directory",
            ));
        }
    }
}

fn is_tack_root(dir: &Path) -> bool {
    let crates = dir.join("crates");
    if !crates.is_dir() {
        return false;
    }

    // Look for something like crates/tackc_*
    if let Ok(entries) = fs::read_dir(crates) {
        for entry in entries.flatten() {
            if let Some(name) = entry.file_name().to_str()
                && name.starts_with("tackc_")
            {
                return true;
            }
        }
    }

    false
}
