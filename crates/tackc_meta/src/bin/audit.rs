use std::{
    fs, io,
    path::Path,
    process,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use ignore::{DirEntry, WalkBuilder, WalkState};

const DANGEROUS_PATTERNS: [(&str, &str); 2] = [
    ("expect_unreachable()", "expect_unreachable"),
    ("[allow", "allow"),
];

fn main() -> io::Result<()> {
    tackc_meta::chdir_to_tack_root()?;

    let error = Arc::new(AtomicBool::new(false));

    let contributors = Arc::new(Mutex::new(vec![]));

    let ignore = WalkBuilder::new("crates").build_parallel();

    ignore.run(|| {
        Box::new({
            let error2 = error.clone();
            let contributors2 = contributors.clone();
            {
                move |e| {
                    'exit: {
                        let Ok(e) = e else {
                            break 'exit;
                        };

                        if e.file_type().is_some_and(|ty| ty.is_dir()) {
                            break 'exit;
                        }

                        if e.path().extension().is_some_and(|ext| ext != "rs") {
                            break 'exit;
                        }
                        if e.path() == Path::new("crates/tackc_meta/src/bin/audit.rs") {
                            break 'exit;
                        }

                        run_line(e, error2.clone(), contributors2.clone());
                    }
                    WalkState::Continue
                }
            }
        })
    });

    if error.load(Ordering::Relaxed) {
        process::exit(1);
    } else {
        process::exit(0);
    }
}

fn run_line(dir: DirEntry, error: Arc<AtomicBool>, contributors: Arc<Mutex<Vec<String>>>) {
    let str = fs::read_to_string(dir.path()).expect("Error opening file!");

    for (i, line) in str.lines().enumerate() {
        for (pat, disp) in DANGEROUS_PATTERNS {
            if line.contains(pat) {
                let Some(idx) = line.find("CHECKED") else {
                    error.store(true, Ordering::SeqCst);
                    eprintln!(
                        "{}:{}: Contains {disp}, missing CHECKED",
                        dir.path().display(),
                        i + 1,
                    );
                    continue;
                };

                let Some(contributor) = line[(idx + "CHECKED".len())..].strip_prefix('(') else {
                    error.store(true, Ordering::SeqCst);
                    report_error(dir.path(), i, "Badly formatted contributor");
                    continue;
                };
                let Some(contributor) = contributor.strip_suffix(")") else {
                    error.store(true, Ordering::SeqCst);
                    report_error(dir.path(), i, "Badly formatted contributor");
                    continue;
                };

                contributors.lock().unwrap().push(contributor.to_string());
            }
        }
    }
}

fn report_error(path: &Path, line: usize, message: &str) {
    eprintln!("{}:{}: {message}", path.display(), line + 1);
}
