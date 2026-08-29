//! Custom verification tool.

use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
    process,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use aho_corasick::{AhoCorasick, AhoCorasickBuilder};
use anyhow::Result;
use ignore::{DirEntry, WalkBuilder, WalkState};
use serde::Deserialize;

const PATTERN_JSON: &str = include_str!("verify.json");

#[derive(Deserialize)]
pub struct Pattern {
    pattern: String,
    finish: Option<char>,
    display: String,
    suggestion: Option<String>,
    allow_checked: bool,
}

fn main() -> Result<()> {
    tackc_meta::chdir_to_tack_root()?;

    let patterns = serde_json::from_str::<Arc<[Pattern]>>(PATTERN_JSON)?;

    let searcher = Arc::new(
        AhoCorasickBuilder::new()
            .build(
                patterns
                    .iter()
                    .map(|pat| &*pat.pattern)
                    .collect::<Vec<_>>()
                    .as_slice(),
            )
            .unwrap(),
    );
    let error = Arc::new(AtomicBool::new(false));
    let contributors = Arc::new(Mutex::new(vec![]));
    let ignore = WalkBuilder::new("tackc_lib").build_parallel();

    ignore.run(|| {
        Box::new({
            let error = error.clone();
            let contributors = contributors.clone();
            let searcher = searcher.clone();
            let patterns = patterns.clone();
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

                        run_line(
                            e,
                            error.clone(),
                            contributors.clone(),
                            searcher.clone(),
                            patterns.clone(),
                        );
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
    patterns: Arc<[Pattern]>,
) {
    let file = File::open(dir.path()).expect("Error opening file!");
    let reader = BufReader::new(file);

    for (i, line) in reader.lines().enumerate() {
        let line = line.expect("Error reading file!");

        let Some(result) = searcher.find(&line) else {
            continue;
        };

        let Pattern {
            pattern: _,
            finish: end,
            ref display,
            ref suggestion,
            allow_checked,
        } = patterns[result.pattern().as_usize()];

        let mut len = result.len();

        if let Some(end) = end {
            for (i, c) in line[result.end()..].chars().enumerate() {
                if c == end {
                    len += i;
                    break;
                }
            }
        }

        let Some(idx) = line.find("// CHECKED").filter(|_| allow_checked) else {
            error.store(true, Ordering::SeqCst);

            eprint!("{}:{}: Contains {display}", dir.path().display(), i + 1);
            if let Some(suggestion) = suggestion {
                eprint!(", {suggestion}");
            }
            eprintln!();
            continue;
        };

        let ratio = (len as f32) / (line[..idx].trim().len() as f32);

        if ratio < (1.0 / 2.0) {
            error.store(true, Ordering::SeqCst);
            eprintln!(
                "{}:{}: CHECKED area is too large!",
                dir.path().display(),
                i + 1
            );
        }

        let Some(contributor) = line[(idx + "// CHECKED".len())..].strip_prefix('(') else {
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
