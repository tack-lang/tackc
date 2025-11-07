use std::{env, fs, io, path::PathBuf};

use quote::{format_ident, quote};

fn main() -> io::Result<()> {
    let crate_names = fs::read_dir("../")?
        .filter_map(|entry| {
            let entry = entry.ok()?;
            entry.metadata().ok()?.is_dir().then_some(entry.file_name())
        })
        .filter(|name| name != "tackc" && name != "tackc_blob" && name != "tackc_macros_impl")
        .collect::<Vec<_>>();

    let crate_names2 = crate_names.clone();
    let mod_names = crate_names2.iter().map(|str| {
        let str = str.to_string_lossy().to_string();
        let mod_name = str
            .strip_prefix("tackc_")
            .unwrap_or_else(|| panic!("Unknown folder name {str}!"));
        format_ident!("{mod_name}")
    });

    let crate_idents = crate_names
        .into_iter()
        .map(|name| format_ident!("{}", name.to_string_lossy().to_string()));

    let file = quote! {
        #(pub mod #mod_names {
            pub use #crate_idents::*;
        })*
    }
    .to_string();

    let path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("blob.rs");
    fs::write(path, file)?;

    println!("cargo::rerun-if-changed=../");

    Ok(())
}
