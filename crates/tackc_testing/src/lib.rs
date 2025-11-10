use std::{ffi::OsStr, fs, io::Read, path::Path};

use anyhow::{Context, Result};
use flate2::Compression;
use serde::{Deserialize, Serialize};
use tackc_file::OwnedFile;

mod prelude {
    pub(crate) use super::{
        BlessType, TestResult, TestType, create_dir_all, exists, load_manifest, load_src_bytes,
        make_file, manifest_name, save_serialized_bytes,
    };
    pub use anyhow::{Context as _, Result};
    pub use serde::Deserialize;
    pub use std::path::{Path, PathBuf};
}

#[derive(Clone, Copy, clap::ValueEnum, PartialEq, Eq)]
pub enum BlessType {
    None,
    Bless,
    Safe,
}

#[derive(Clone, Copy)]
pub enum TestResult {
    Success,
    Failure,
    Skipped,
}

mod lexer;

fn exists(manifest_path: &Path, entry_relative_path: impl AsRef<Path>) -> bool {
    let manifest_dir = manifest_path.parent().unwrap();
    let data_path = manifest_dir.join(entry_relative_path);
    data_path.exists()
}

fn manifest_name(manifest_path: &Path) -> &OsStr {
    manifest_path.file_prefix().unwrap()
}

fn load_manifest<D: for<'de> Deserialize<'de>>(manifest_path: &Path) -> Result<D> {
    load_serialized_str::<D>(manifest_path, manifest_path.file_name().unwrap())
        .context("Failed to load manifest!")
}

fn load_serialized_str<D: for<'de> Deserialize<'de>>(
    manifest_path: &Path,
    data_path_relative: impl AsRef<Path>,
) -> Result<D> {
    let manifest_dir = manifest_path.parent().unwrap();
    let data_path = manifest_dir.join(data_path_relative);
    let data_src = fs::read_to_string(data_path).context("Failed to read data!")?;
    let data = toml::from_str::<D>(&data_src).context("Failed to deserialize data!")?;
    Ok(data)
}

fn load_src_bytes(manifest_path: &Path, data_path_relative: impl AsRef<Path>) -> Result<Vec<u8>> {
    let manifest_dir = manifest_path.parent().unwrap();
    let data_path = manifest_dir.join(data_path_relative);
    let compressed_data_bytes = fs::read(&data_path).context("Failed to read data!")?;
    let data = decompress(&compressed_data_bytes).context("Failed to decompress data!")?;

    Ok(data)
}

fn save_serialized_bytes<S: Serialize>(
    manifest_path: &Path,
    data_path_relative: impl AsRef<Path>,
    data: &S,
) -> Result<()> {
    let manifest_dir = manifest_path.parent().unwrap();
    let data_path = manifest_dir.join(data_path_relative);
    let uncompressed_data_bytes = sbof::to_bytes(data).context("Failed to serialize data!")?;
    let compressed_data_bytes =
        compress(&uncompressed_data_bytes).context("Failed to compress data!")?;
    fs::write(data_path, &compressed_data_bytes).context("Failed to write data!")?;

    Ok(())
}

fn make_file(manifest_path: &Path, file_path_relative: impl AsRef<Path>) -> Result<OwnedFile> {
    let manifest_dir = manifest_path.parent().unwrap();
    let file_path = manifest_dir.join(file_path_relative);
    let file: OwnedFile = file_path.try_into().context("Failed to create file!")?;
    Ok(file)
}

fn compress(data: &[u8]) -> Result<Vec<u8>> {
    let mut encoder = flate2::read::GzEncoder::new(data, Compression::new(6));
    let mut out = Vec::new();
    encoder
        .read_to_end(&mut out)
        .context("Failed to compress data!")?;
    Ok(out)
}

fn decompress(data: &[u8]) -> Result<Vec<u8>> {
    let mut encoder = flate2::read::GzDecoder::new(data);
    let mut out = Vec::new();
    encoder
        .read_to_end(&mut out)
        .context("Failed to decompress data!")?;
    Ok(out)
}

fn create_dir_all(manifest_path: &Path, folder_path_relative: impl AsRef<Path>) -> Result<()> {
    let manifest_dir = manifest_path.parent().unwrap();
    let folder_path = manifest_dir.join(folder_path_relative);
    fs::create_dir_all(folder_path).context("Failed to create directories!")?;
    Ok(())
}

pub struct TestType {
    run: fn(manifest_path: &Path, bless: BlessType) -> Result<TestResult>,
    view: fn(manifest_path: &Path) -> Result<()>,
    view_expected: fn(manifest_path: &Path) -> Result<()>,
}

impl TestType {
    pub fn lexer() -> &'static Self {
        lexer::get()
    }

    pub fn run(&self, manifest_path: &Path, bless: BlessType) -> Result<TestResult> {
        (self.run)(manifest_path, bless)
    }

    pub fn view(&self, manifest_path: &Path) -> Result<()> {
        (self.view)(manifest_path)
    }

    pub fn view_expected(&self, manifest_path: &Path) -> Result<()> {
        (self.view_expected)(manifest_path)
    }
}
