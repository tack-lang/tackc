//! A simple build script to make the compiler hash availible to the crate.

fn main() {
    println!("cargo::rerun-if-changed=../tackc_lib");
    let hash = tackc_meta::hash();
    println!("cargo::rustc-env=TACKC_COMPILER_HASH={hash}");
}
