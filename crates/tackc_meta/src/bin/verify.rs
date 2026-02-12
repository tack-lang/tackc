use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
    process,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use aho_corasick::{AhoCorasick, AhoCorasickBuilder};
use ignore::{DirEntry, WalkBuilder, WalkState};

const DANGEROUS_PATTERNS: [(&str, &str); 7] = [
    (".expect_unreachable()", "expect_unreachable()"),
    ("[expect", "#[expect]"),
    (".expect(", "expect()"),
    (".unwrap(", "unwrap()"),
    ("panic!", "panic!()"),
    ("unreachable!", "unreachable!()"),
    ("unimplemented!", "unimplemented!()"),
];

fn main() -> io::Result<()> {
    tackc_meta::chdir_to_tack_root()?;

    let searcher = Arc::new(
        AhoCorasickBuilder::new()
            .build(
                DANGEROUS_PATTERNS
                    .into_iter()
                    .map(|(str, _)| str)
                    .collect::<Vec<_>>()
                    .as_slice(),
            )
            .unwrap(),
    );
    let error = Arc::new(AtomicBool::new(false));
    let contributors = Arc::new(Mutex::new(vec![]));
    let ignore = WalkBuilder::new("crates").build_parallel();

    ignore.run(|| {
        Box::new({
            let error = error.clone();
            let contributors = contributors.clone();
            let searcher = searcher.clone();
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
                        if e.path().starts_with("crates/tackc_meta") {
                            break 'exit;
                        }
                        if e.path().starts_with("crates/tackc") {
                            break 'exit;
                        }

                        run_line(e, error.clone(), contributors.clone(), searcher.clone());
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

fn run_line(
    dir: DirEntry,
    error: Arc<AtomicBool>,
    contributors: Arc<Mutex<Vec<String>>>,
    searcher: Arc<AhoCorasick>,
) {
    let file = File::open(dir.path()).expect("Error opening file!");
    let reader = BufReader::new(file);

    for (i, line) in reader.lines().enumerate() {
        let line = line.expect("Error reading file!");

        let Some(result) = searcher.find(&line) else {
            continue;
        };

        let disp = DANGEROUS_PATTERNS[result.pattern().as_usize()].1;

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

fn report_error(path: &Path, line: usize, message: &str) {
    eprintln!("{}:{}: {message}", path.display(), line + 1);
}
