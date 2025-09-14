use crate::{DynResult, err, project_root};
use std::fs;

pub fn lint_ts_queries() -> DynResult {
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
