use ad::{Editor, Program};
use ropey::Rope;
use std::{
    env, fs,
    io::{self, Write},
    process::exit,
};

const USAGE: &str = "\
usage:
  ad [file ...]                 Edit file(s)
  ad -e script [file ...]       Execute edit script on file(s)
  ad -f script-file [file ...]  Execute edit script loaded from script-file on file(s)

  ad -h | --help                Print this help message
  ad -v | --version             Print version information
";

fn main() {
    let Args { script, files } = parse_args();

    if let Some(script) = script {
        return run_script(&script, files);
    }

    let mut e = Editor::new();
    for fname in files.iter() {
        e.open_file(fname);
    }

    e.run()
}

struct Args {
    script: Option<String>,
    files: Vec<String>,
}

fn fatal(msg: &str) -> ! {
    eprintln!("{msg}");
    exit(1);
}

fn parse_args() -> Args {
    let mut args = env::args().skip(1);

    match args.next().as_deref() {
        // no files to open
        None => Args {
            script: None,
            files: vec![],
        },

        // script expression to run
        Some("-e" | "--expression") => {
            let script = match args.next() {
                Some(script) => Some(script),
                None => fatal("no script provided"),
            };
            let files: Vec<String> = args.collect();
            Args { script, files }
        }

        // script file to run
        Some("-f" | "--script-file") => {
            let script = match args.next() {
                Some(fname) => {
                    let script = match fs::read_to_string(&fname) {
                        Ok(s) => s,
                        Err(e) => fatal(&format!("unable to load script file from {fname}: {e}")),
                    };
                    Some(script)
                }
                None => fatal("no script file provided"),
            };
            let files: Vec<String> = args.collect();
            Args { script, files }
        }

        Some("-h" | "--help") => show_help(),
        Some("-v" | "--version") => show_version_info(),

        _ => {
            let files: Vec<String> = args.collect();
            Args {
                script: None,
                files,
            }
        }
    }
}

fn run_script(script: &str, files: Vec<String>) {
    let mut prog = Program::try_parse(script).unwrap();
    let mut buf = vec![];

    for path in files.iter() {
        let s = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("unable to open file '{path}': {e}");
                exit(1);
            }
        };

        if let Err(e) = prog.execute(&mut Rope::from_str(&s), path, &mut buf) {
            eprintln!("error running script: {e:?}");
            exit(1);
        }
    }

    io::stdout().write_all(&buf).unwrap();
}

fn show_help() -> ! {
    println!(
        "ad v{}\nInnes Anderson-Morrison (sminez)",
        env!("CARGO_PKG_VERSION")
    );
    println!("\nad is an minimal, adaptable text editor\n");
    println!("{USAGE}");
    exit(0);
}

fn show_version_info() -> ! {
    println!("ad v{}", env!("CARGO_PKG_VERSION"));
    exit(0);
}
