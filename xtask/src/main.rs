// https://github.com/matklad/cargo-xtask
use std::{
    env,
    path::{Path, PathBuf},
    process::exit,
};

mod setup;
mod ts;

type DynResult = Result<(), Box<dyn std::error::Error>>;

#[macro_export]
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
        Some("lint-ts-queries") => ts::lint_ts_queries()?,
        Some("gen-man-page") => setup::generate_manpage()?,
        Some("setup-dotfiles") => setup::setup_dotfiles()?,

        _ => list_tasks(),
    }

    Ok(())
}

fn list_tasks() {
    eprintln!(
        "Available tasks:
  * setup-dotfiles          copy over the default dotfiles from the /data directory
  * lint-ts-queries         ensure that the tree-sitter queries in /data are valid
  * gen-man-page            re-generate the man page"
    );
}

fn project_root() -> PathBuf {
    Path::new(&env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(1)
        .unwrap()
        .to_path_buf()
}

fn dist_dir() -> PathBuf {
    project_root().join("target/dist")
}
