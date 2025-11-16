use tackc_global::Global;
use tackc_lexer::{Error, Lexer, Token};

use super::prelude::*;

#[derive(Deserialize)]
pub struct Manifest {
    src: PathBuf,
}

pub fn get() -> &'static TestType {
    &TestType {
        run,
        view,
        view_expected,
    }
}

pub fn run(manifest_path: &Path, bless: BlessType) -> Result<TestResult> {
    let global = Global::create_heap();

    let manifest = load_manifest::<Manifest>(manifest_path).context("Failed to load manifest")?;
    let src = make_file(manifest_path, &manifest.src).context("Failed to make source file!")?;
    let manifest_name = manifest_name(manifest_path);

    let lexer = Lexer::new(&src, &global);
    let tokens = lexer.collect::<Vec<_>>();

    let out_path = PathBuf::from(format!("out/{}.bin.gz", manifest_name.display()));
    let expected_path = PathBuf::from(format!("expected/{}.bin.gz", manifest_name.display()));

    create_dir_all(manifest_path, "out").context("Failed to create out directory!")?;
    save_serialized_bytes(manifest_path, &out_path, &tokens)
        .context("Failed to save tokens to out file!")?;

    if bless == BlessType::Bless
        || (bless == BlessType::Safe && !exists(manifest_path, &expected_path))
    {
        create_dir_all(manifest_path, "expected")
            .context("Failed to create expected directory!")?;
        save_serialized_bytes(manifest_path, &expected_path, &tokens)
            .context("Failed to save tokens to expected file!")?;
        Ok(TestResult::Skipped)
    } else {
        let expected_bytes = load_src_bytes(manifest_path, &expected_path).context("Failed to load bytes from expected file! If there is no expected file, try running with --bless!")?;
        let expected = sbof::from_bytes::<Vec<Result<Token, Error>>>(&expected_bytes)
            .context("Failed to deserialize expected file!")?;

        if expected != tokens {
            if bless == BlessType::Failing {
                create_dir_all(manifest_path, "expected")
                    .context("Failed to create expected directory!")?;
                save_serialized_bytes(manifest_path, &expected_path, &tokens)
                    .context("Failed to save tokens to expected file!")?;
                Ok(TestResult::Skipped)
            } else {
                Ok(TestResult::Failure)
            }
        } else {
            Ok(TestResult::Success)
        }
    }
}

pub fn view(manifest_path: &Path) -> Result<()> {
    let manifest_name = manifest_name(manifest_path);
    let out_path = format!("out/{}.bin.gz", manifest_name.display());
    run(manifest_path, BlessType::None).context("Failed to run test!")?;

    let bytes = load_src_bytes(manifest_path, &out_path).context("Failed to read output bytes!")?;
    let data = sbof::from_bytes::<Vec<Result<Token, Error>>>(&bytes)
        .context("Failed to deserialize output bytes!")?;

    for res in data {
        match res {
            Ok(tok) => println!("{}", tok.kind),
            Err(e) => println!("{e}"),
        }
    }

    Ok(())
}

pub fn view_expected(manifest_path: &Path) -> Result<()> {
    let manifest_name = manifest_name(manifest_path);
    let expected_path = format!("expected/{}.bin.gz", manifest_name.display());

    let bytes =
        load_src_bytes(manifest_path, &expected_path).context("Failed to read expected bytes!")?;
    let data = sbof::from_bytes::<Vec<Result<Token, Error>>>(&bytes)
        .context("Failed to deserialize expected bytes!")?;

    for res in data {
        match res {
            Ok(tok) => println!("{}", tok.kind),
            Err(e) => println!("{e}"),
        }
    }

    Ok(())
}
