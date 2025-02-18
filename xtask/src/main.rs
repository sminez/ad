// https://github.com/matklad/cargo-xtask
use std::{
    env, fs,
    path::{Path, PathBuf},
    process::exit,
};

type DynResult = Result<(), Box<dyn std::error::Error>>;

macro_rules! err {
    ($msg:expr) => {
        Err($msg.to_string().into())
    };

    ($template:expr, $($arg:expr),+) => {
        Err(format!($template, $($arg),+).into())
    };
}

fn main() {
    if let Err(e) = try_main() {
        eprintln!("{e}");
        exit(1);
    }
}

fn try_main() -> DynResult {
    let task = env::args().nth(1);

    match task.as_deref() {
        Some("lint-ts-queries") => lint_ts_queries()?,

        _ => list_tasks(),
    }

    Ok(())
}

fn list_tasks() {
    eprintln!(
        "Available tasks:
  * lint-ts-queries         ensure that the tree-sitter queries in /data are valid"
    );
}

fn lint_ts_queries() -> DynResult {
    let unsupported_predicates = [
        // neovim
        "any-contains?",
        "any-lua-match?",
        "any-vim-match?",
        "contains?",
        "has-ancestor?",
        "has-parent?",
        "lua-match?",
        "vim-match?",
    ];
    let mut valid = true;
    eprintln!(">> Linting tree-sitter queries");

    let query_root = project_root().join("data/tree-sitter/queries");
    for entry in fs::read_dir(query_root)? {
        let path = entry?.path();
        if path.is_file() {
            eprintln!(
                "[x] unexpected file in data/tree-sitter/queries:\n{}",
                path.display()
            );
            valid = false;
        }

        let lang = path.file_name().unwrap().to_string_lossy();
        let highlights = path.join("highlights.scm");
        eprintln!("[ ] checking highlights for {lang}...");

        if !highlights.exists() {
            eprintln!("  [x] no highlights.scm found for {lang}");
            valid = false;
            continue;
        }

        let query = fs::read_to_string(highlights)?;
        for p in unsupported_predicates {
            if query.contains(p) {
                eprintln!("  [x] highlights for {lang} contain an unsupported predicate: {p}");
                valid = false;
            }
        }
        if query.contains("@spell") {
            eprintln!("  [x] highlights for {lang} contain '@spell' which needs to be removed");
            valid = false;
        }
    }

    if !valid {
        return err!("validation failed: see logs for details");
    }

    eprintln!("\ntree-sitter queries linted successfully");
    Ok(())
}

fn project_root() -> PathBuf {
    Path::new(&env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(1)
        .unwrap()
        .to_path_buf()
}
