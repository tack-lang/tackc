//! Various code for meta tackc binaries.

use std::{
    env, fs, io,
    path::{Path, PathBuf},
};

use sha2::{Digest, Sha256};
use walkdir::WalkDir;

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

pub fn hash() -> String {
    chdir_to_tack_root().expect("Failed to setup current directory!");

    let mut hasher = Sha256::new();

    let mut files: Vec<_> = WalkDir::new("tackc_lib")
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| {
            let p = e.path();
            p.is_file() && p.extension().map(|x| x == "rs").unwrap_or(false)
        })
        .map(|e| e.path().to_owned())
        .collect();

    // Deterministic ordering
    files.sort();

    // --- Hash source files (normalized) ---
    for path in files {
        if let Ok(src) = fs::read_to_string(path) {
            let normalized = normalize(&src);
            hasher.update(normalized.as_slice());
        }
    }

    let lock = fs::read("Cargo.lock").expect("Failed to read Cargo.lock");
    hasher.update(&lock);

    let array = hasher.finalize();
    let slice = &array.as_slice()[0..8];
    let hash = u64::from_be_bytes(slice.try_into().unwrap());

    // --- Compute final hash ---
    let hash_str = format!("{hash:x}");
    hash_str[..12].to_string() // 12-hex-char fingerprint
}

fn normalize(src: &str) -> Vec<u8> {
    let mut out = String::new();

    for mut line in src.replace('\r', "").lines() {
        // strip comments
        if let Some((base, _)) = line.split_once("//") {
            line = base;
        }

        let line = line.trim_end();
        if line.trim().is_empty() {
            continue;
        }

        out.push_str(line);
        out.push('\n');
    }

    out.into_bytes()
}
