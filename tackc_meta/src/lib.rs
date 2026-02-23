//! Various code for meta tackc binaries.

use std::{
    env, io,
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
    let tackc_lib = dir.join("tackc_lib");

    tackc_lib.is_dir()
}
