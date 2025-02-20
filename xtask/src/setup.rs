use crate::{dist_dir, project_root, DynResult};
use man::prelude::*;
use std::{fs, path::PathBuf};

const DESCRIPTION: &str = "\
ad is a text editor and command line stream editor. The text editor interface for
ad is inspired by the likes of vim and kakoune, along with the acme and sam editors
from plan9. ad aims to provide an 'integrating development environment' as opposed
to an 'integrated' one: leveraging the surrounding system for the majority of
functionality outisde of the core text editing actions.
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
                .help("Execute an edit script on file(s)"),
        )
        .option(
            Opt::new("script-file")
                .short("-f")
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

    eprintln!("  [ ] copying default config file");
    cp("config.toml")?;
    eprintln!("  [ ] copying default plumbing rules");
    cp("plumbing.rules")?;
    eprintln!("  [ ] copying data/bin...");
    fs::create_dir_all(dot_dir.join("bin"))?;
    for entry in fs::read_dir(data_dir.join("bin"))? {
        let path = entry?.path();
        let fname = path.file_name().unwrap().to_string_lossy();
        cp(&format!("bin/{fname}"))?;
    }
    eprintln!("  [ ] copying data/lib...");
    fs::create_dir_all(dot_dir.join("lib"))?;
    for entry in fs::read_dir(data_dir.join("lib"))? {
        let path = entry?.path();
        let fname = path.file_name().unwrap().to_string_lossy();
        cp(&format!("lib/{fname}"))?;
    }

    Ok(())
}
