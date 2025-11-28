use tackc_error::iter::IteratorExt;
use tackc_global::Global;
use tackc_lexer::Lexer;
use tackc_parser::{
    Parser,
    ast::{AstNode, Expression},
    error::{DiagResult, ParseErrors},
};

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

    run_with(manifest_path, bless, &global)
}

fn run_with(manifest_path: &Path, bless: BlessType, global: &Global) -> Result<TestResult> {
    let manifest = load_manifest::<Manifest>(manifest_path).context("Failed to load manifest")?;
    let src = make_file(manifest_path, &manifest.src).context("Failed to make source file!")?;
    let manifest_name = manifest_name(manifest_path);

    let lexer = Lexer::new(&src, global).consume_reporter(drop);
    let mut p = Parser::new(lexer);
    let expr = Expression::parse(&mut p, 0).expected("expression");

    let out_path = PathBuf::from(format!("out/{}.bin.gz", manifest_name.display()));
    let expected_path = PathBuf::from(format!("expected/{}.bin.gz", manifest_name.display()));

    create_dir_all(manifest_path, "out").context("Failed to create out directory!")?;
    save_serialized_bytes(manifest_path, &out_path, &expr)
        .context("Failed to save expression to out file!")?;

    if bless == BlessType::Bless
        || (bless == BlessType::Safe && !exists(manifest_path, &expected_path))
    {
        create_dir_all(manifest_path, "expected")
            .context("Failed to create expected directory!")?;
        save_serialized_bytes(manifest_path, &expected_path, &expr)
            .context("Failed to save expression to expected file!")?;
        Ok(TestResult::Skipped)
    } else {
        let expected_bytes = load_src_bytes(manifest_path, &expected_path).context("Failed to load bytes from expected file! If there is no expected file, try running with --bless!")?;
        let expected = sbof::from_bytes::<Result<Expression, ParseErrors>>(&expected_bytes)
            .context("Failed to deserialize expected file!")?;

        if expected != expr {
            if bless == BlessType::Failing {
                create_dir_all(manifest_path, "expected")
                    .context("Failed to create expected directory!")?;
                save_serialized_bytes(manifest_path, &expected_path, &expr)
                    .context("Failed to save expression to expected file!")?;
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
    let global = Global::create_heap();

    let manifest_name = manifest_name(manifest_path);
    let out_path = format!("out/{}.bin.gz", manifest_name.display());
    run_with(manifest_path, BlessType::None, &global).context("Failed to run test!")?;

    let bytes = load_src_bytes(manifest_path, &out_path).context("Failed to read output bytes!")?;
    let data = sbof::from_bytes::<Result<Expression, ParseErrors>>(&bytes)
        .context("Failed to deserialize output bytes!")?;

    let manifest = load_manifest::<Manifest>(manifest_path).context("Failed to load manifest")?;
    let src = make_file(manifest_path, &manifest.src).context("Failed to make source file!")?;

    match data {
        Ok(expr) => println!("{}", expr.display(&global)),
        Err(e) => println!("{}", e.display(&src, &global)),
    }

    Ok(())
}

pub fn view_expected(manifest_path: &Path) -> Result<()> {
    let manifest_name = manifest_name(manifest_path);
    let expected_path = format!("expected/{}.bin.gz", manifest_name.display());

    let bytes =
        load_src_bytes(manifest_path, &expected_path).context("Failed to read expected bytes!")?;
    let data = sbof::from_bytes::<Result<Expression, ParseErrors>>(&bytes)
        .context("Failed to deserialize expected bytes!")?;

    match data {
        Ok(expr) => println!("{expr:?}"),
        Err(e) => println!("{e:?}"),
    }

    Ok(())
}
