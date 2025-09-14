use crate::{DynResult, dist_dir, project_root};
use man::prelude::*;
use std::{fs, path::PathBuf};

const DESCRIPTION: &str = "\
ad is a text editor and command line stream editor. The text editor interface for
ad is inspired by the likes of vim and kakoune, along with the acme and sam editors
from plan9. ad aims to provide an 'integrating development environment' as opposed
to an 'integrated' one: leveraging the surrounding system for the majority of
functionality outside of the core text editing actions.
";

const DOTFILE_DIR: &str = ".ad";

pub fn generate_manpage() -> DynResult {
    eprintln!(">> Generating man page");
    let dir = dist_dir();
    fs::create_dir_all(&dir)?;

    let content = Manual::new("ad")
        .about("An adaptable text editor")
        .author(Author::new("Innes Anderson-Morrison"))
        .description(DESCRIPTION)
        .option(
            Opt::new("script")
                .short("-e")
                .long("--expression")
                .help("Execute an edit script on file(s)"),
        )
        .option(
            Opt::new("script-file")
                .short("-f")
                .long("--script-file")
                .help("Execute an edit script loaded from a script-file on file(s)"),
        )
        .arg(Arg::new("[file...]"))
        .flag(
            Flag::new()
                .short("-h")
                .long("--help")
                .help("Print command line help and exit"),
        )
        .flag(
            Flag::new()
                .short("-v")
                .long("--version")
                .help("Print version information and exit"),
        )
        .render();

    let p = dir.join("ad.1");
    eprintln!("  [ ] writing manpage to {}", p.display());
    fs::write(p, content)?;
    eprintln!("  [ ] done");

    Ok(())
}

pub fn setup_dotfiles() -> DynResult {
    eprintln!(">> Setting up dotfile directory");
    let data_dir = project_root().join("data");
    let dot_dir = PathBuf::from(env!("HOME")).join(DOTFILE_DIR);

    eprintln!("  [ ] creating dotfile directory: {}", dot_dir.display());
    fs::create_dir_all(dot_dir.join("mnt"))?;

    let cp = |path: &str| fs::copy(data_dir.join(path), dot_dir.join(path));
    let cp_dir = |path: &str| {
        fs::create_dir_all(dot_dir.join(path))?;
        for entry in fs::read_dir(data_dir.join(path))? {
            let p = entry?.path();
            let fname = p.file_name().unwrap().to_string_lossy();
            cp(&format!("{path}/{fname}"))?;
        }
        DynResult::Ok(())
    };

    eprintln!("  [ ] copying default config file");
    cp("config.toml")?;
    eprintln!("  [ ] copying default plumbing rules");
    cp("plumbing.rules")?;
    eprintln!("  [ ] copying data/bin...");
    cp_dir("bin")?;
    eprintln!("  [ ] copying data/lib...");
    cp_dir("lib")?;
    eprintln!("  [ ] copying data/colorschemes...");
    cp_dir("colorschemes")?;
    eprintln!("  [ ] copying data/syntax...");
    cp_dir("syntax")?;

    Ok(())
}
