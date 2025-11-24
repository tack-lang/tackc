//! Written by AI, verified by human.

use sha2::{Digest, Sha256};
use std::{
    env, fs, io,
    path::{Path, PathBuf},
};
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

fn main() {
    chdir_to_tack_root().expect("Failed to setup current directory!");

    let mut hasher = Sha256::new();

    // --- Hash source files (normalized) ---
    for entry in WalkDir::new("crates")
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().is_some_and(|x| x == "rs"))
    {
        let path = entry.path();
        if let Ok(src) = fs::read_to_string(path) {
            let normalized = normalize(&src);
            hasher.update(normalized.as_bytes());
        }
    }

    // --- Hash Cargo.lock if it exists ---
    if Path::new("Cargo.lock").exists() {
        let lock = fs::read("Cargo.lock").expect("Failed to read Cargo.lock");
        hasher.update(&lock);
    }

    // --- Compute final hash ---
    let hash = format!("{:x}", hasher.finalize());
    let short = &hash[..12]; // 12-hex-char fingerprint

    println!("{short}");
}

/// Remove comments + whitespace-only lines.
fn normalize(src: &str) -> String {
    let mut out = String::new();

    for line in src.lines() {
        // Strip trailing // comments
        let line = line.split_once("//").map_or(line, |x| x.0).trim_end();

        // Skip empty/whitespace-only lines
        if line.trim().is_empty() {
            continue;
        }

        out.push_str(line);
        out.push('\n');
    }

    out
}
