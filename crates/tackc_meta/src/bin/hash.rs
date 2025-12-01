//! Written by AI, verified by human.

use sha2::{Digest, Sha256};
use std::{fs, path::Path};
use tackc_meta::chdir_to_tack_root;
use walkdir::WalkDir;

fn main() {
    chdir_to_tack_root().expect("Failed to setup current directory!");

    let mut hasher = Sha256::new();

    let mut files: Vec<_> = WalkDir::new("crates")
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| {
            let p = e.path();
            p.is_file()
                && p.extension().map(|x| x == "rs").unwrap_or(false)
                && p.components()
                    .any(|c| c.as_os_str().to_string_lossy().starts_with("tackc_"))
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
