use std::{
    env,
    ffi::OsStr,
    path::{Path, PathBuf}, process::exit,
};

use anyhow::{Context, Result};
use clap::{ArgGroup, Parser, Subcommand};
use colored::Colorize;
use tackc_testing::{BlessType, TestResult, TestType};
use walkdir::WalkDir;

#[derive(Parser)]
#[command(name = "tackc_testing", bin_name = "cargo testing")]
struct Args {
    #[command(subcommand)]
    subcommand: Option<SubArgs>,
}

#[derive(Subcommand)]
enum SubArgs {
    /// Run tests.
    Run {
        /// Subpath of tackc_testing/data to run tests from.
        subpath: Option<PathBuf>,

        #[clap(flatten)]
        bless: BlessArgs,
    },

    /// View the output from tests.
    View {
        /// Test to view the output of.
        test: PathBuf,

        /// If the expected output should be viewed
        #[arg(long)]
        expected: bool,
    },
}

#[derive(Parser, Debug)]
#[command(group(
    ArgGroup::new("bless")
        .args(&["bless_flag", "safe_bless_flag"])
        .multiple(false)
))]
struct BlessArgs {
    /// Skip tests, write expected outputs.
    #[arg(long = "bless")]
    bless_flag: bool,

    /// Skip tests without expected outputs, write expected outputs.
    #[arg(long = "safe-bless")]
    safe_bless_flag: bool,

    /// Write expected outputs for failed tests.
    #[arg(long = "bless-failing")]
    failing_bless_flag: bool,

    /// Write expected outputs for tests returning errors.
    #[arg(long = "bless-error")]
    error_bless_flag: bool,
}

impl From<BlessType> for BlessArgs {
    fn from(value: BlessType) -> Self {
        match value {
            BlessType::Bless => BlessArgs {
                bless_flag: true,
                safe_bless_flag: false,
                failing_bless_flag: false,
                error_bless_flag: false,
            },
            BlessType::Safe => BlessArgs {
                bless_flag: false,
                safe_bless_flag: true,
                failing_bless_flag: false,
                error_bless_flag: false,
            },
            BlessType::Failing => BlessArgs {
                bless_flag: false,
                safe_bless_flag: false,
                failing_bless_flag: true,
                error_bless_flag: false,
            },
            BlessType::Error => BlessArgs {
                bless_flag: false,
                safe_bless_flag: false,
                failing_bless_flag: false,
                error_bless_flag: true,
            },
            BlessType::None => BlessArgs {
                bless_flag: false,
                safe_bless_flag: false,
                failing_bless_flag: false,
                error_bless_flag: false,
            },
        }
    }
}

impl From<BlessArgs> for BlessType {
    fn from(value: BlessArgs) -> Self {
        if value.bless_flag {
            BlessType::Bless
        } else if value.safe_bless_flag {
            BlessType::Safe
        } else if value.failing_bless_flag {
            BlessType::Failing
        } else if value.error_bless_flag {
            BlessType::Error
        } else {
            BlessType::None
        }
    }
}

impl Default for SubArgs {
    fn default() -> Self {
        SubArgs::Run {
            subpath: None,
            bless: BlessType::None.into(),
        }
    }
}

fn main() -> Result<()> {
    let package_dir = find_package_dir().context("Failed to find package dir!")?;
    env::set_current_dir(package_dir).context("Failed to set current directory!")?;

    let args = Args::parse();
    let command = args.subcommand.unwrap_or_default();

    match command {
        SubArgs::Run { subpath, bless } => {
            let subpath = subpath.unwrap_or_default();
            let mut components = subpath.components();
            let mut test_type: Option<&TestType> = None;
            let mut normalized_subpath: Option<PathBuf> = None;

            while let Some(comp) = components.next() {
                if comp.as_os_str() == "data" || comp.as_os_str() == "/" {
                    continue;
                } else if comp.as_os_str() == "lexer" {
                    test_type = Some(TestType::lexer());
                    normalized_subpath =
                        Some(PathBuf::from("data/lexer").join(components.collect::<PathBuf>()));
                    break;
                } else if comp.as_os_str() == "expr" {
                    test_type = Some(TestType::expr());
                    normalized_subpath =
                        Some(PathBuf::from("data/expr").join(components.collect::<PathBuf>()));
                    break;
                } else {
                    break;
                }
            }

            if let Some(ty) = test_type {
                let mut normalized = normalized_subpath.unwrap();
                if !normalized.exists() {
                    normalized.add_extension("toml");
                }
                run_test_ty(ty, normalized, bless.into());
            } else {
                let failed = run_test_tys(bless.into());
                if failed != 0 {
                    exit(1);
                }
            }
        }
        SubArgs::View { test, expected } => {
            let mut components = test.components();
            let mut test_type: Option<&TestType> = None;
            let mut normalized_test_path: Option<PathBuf> = None;

            while let Some(comp) = components.next() {
                if comp.as_os_str() == "data" || comp.as_os_str() == "/" {
                    continue;
                } else if comp.as_os_str() == "lexer" {
                    test_type = Some(TestType::lexer());
                    normalized_test_path =
                        Some(PathBuf::from("data/lexer").join(components.collect::<PathBuf>()));
                    break;
                } else if comp.as_os_str() == "expr" {
                    test_type = Some(TestType::expr());
                    normalized_test_path =
                        Some(PathBuf::from("data/expr").join(components.collect::<PathBuf>()));
                    break;
                } else {
                    break;
                }
            }
            if let Some(ty) = test_type {
                let normalized = normalized_test_path.unwrap();
                let normalized_toml = normalized.with_extension("toml");
                if normalized.exists() && !normalized.is_file() {
                    panic!("Cannot view multiple tests at once!");
                } else if !normalized_toml.exists() {
                    panic!("Cannot find test {}", normalized.display());
                }

                if expected {
                    ty.view_expected(&normalized_toml)?;
                } else {
                    ty.view(&normalized_toml)?;
                }
            } else {
                panic!("Cannot view multiple tests at once!");
            }
        }
    }

    Ok(())
}

fn run_test_tys(bless: BlessType) -> u32 {
    let mut failed = 0;
    failed += run_test_ty(TestType::lexer(), "data/lexer", bless);
    failed += run_test_ty(TestType::expr(), "data/expr", bless);
    failed
}

// Returns the failure count
fn run_test_ty(ty: &TestType, path: impl AsRef<Path>, bless: BlessType) -> u32 {
    let mut failed = 0;

    println!("Running all tests in {}", path.as_ref().display());

    for entry in WalkDir::new(path).into_iter().filter_map(Result::ok) {
        if entry.file_type().is_file() && entry.path().extension() == Some(OsStr::new("toml")) {
            let manifest_path = entry.path();
            if let Some(TestResult::Failure) = run_test(ty, manifest_path, bless) {
                failed += 1;
            }
        }
    }

    failed
}

fn run_test(ty: &TestType, manifest_path: &Path, bless: BlessType) -> Option<TestResult> {
    let result = ty.run(manifest_path, bless);
    match &result {
        Ok(TestResult::Success) => {
            let out = format!("[{}] SUCCESS", display_test(manifest_path).display()).bright_green();
            println!("{out}");
        }
        Ok(TestResult::Failure) => {
            let out = format!("[{}] FAILED", display_test(manifest_path).display()).bright_red();
            println!("{out}");
        }
        Ok(TestResult::Skipped) => {
            let out =
                format!("[{}] SKIPPED", display_test(manifest_path).display()).bright_yellow();
            println!("{out}");
        }
        Err(e) => {
            let out = format!("[{}] ERROR: {e:?}", display_test(manifest_path).display()).red();
            println!("{out}");
        }
    }
    result.ok()
}

fn display_test(manifest_path: &Path) -> PathBuf {
    manifest_path
        .components()
        .skip_while(|component| component.as_os_str() != "data")
        .skip(1)
        .collect::<PathBuf>()
        .with_extension("")
}

fn find_package_dir() -> Result<PathBuf> {
    fn _find_package_dir(current_dir: PathBuf) -> PathBuf {
        if is_package_dir(&current_dir) {
            current_dir
        } else if is_crates_dir(&current_dir) {
            current_dir.join("tackc_testing")
        } else if is_workspace_dir(&current_dir) {
            current_dir.join("crates/tackc_testing")
        } else {
            #[allow(clippy::used_underscore_items)]
            _find_package_dir(
                current_dir
                    .parent()
                    .expect("must run tackc_testing inside tack workspace!")
                    .to_path_buf(),
            )
        }
    }

    let current_dir = env::current_dir()?;

    #[allow(clippy::used_underscore_items)]
    Ok(_find_package_dir(current_dir))
}

fn is_package_dir(dir: &Path) -> bool {
    dir.ends_with("crates/tackc_testing")
        && dir.join("Cargo.toml").exists()
        && dir.join("data").exists()
}

fn is_crates_dir(dir: &Path) -> bool {
    dir.ends_with("crates")
        && !dir.join("Cargo.toml").exists()
        && dir.join("tackc_testing").exists()
}

fn is_workspace_dir(dir: &Path) -> bool {
    dir.join("Cargo.toml").exists() && dir.join("crates").exists()
}
