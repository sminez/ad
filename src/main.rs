use ad_editor::{
    CachedStdin, Config, Editor, EditorMode, GapBuffer, LogBuffer, PlumbingRules, Program,
    LOG_LEVEL_ENV_VAR,
};
use ninep::sync::client::UnixClient;
use std::{
    env, fs,
    io::{self, Read, Write},
    process::exit,
};
use tracing::{error, level_filters::LevelFilter, subscriber::set_global_default};

const USAGE: &str = "\
usage:
  ad [file ...]                 Edit file(s)
  ad -e script [file ...]       Execute edit script on file(s)
  ad -f script-file [file ...]  Execute edit script loaded from script-file on file(s)
  ad -9p [-A aname] cmd args    Interact with a 9p server using a simple 9p client
                                  Commands:
                                    read  ns/path
                                    write ns/path
                                    ls    ns/path

  ad -h | --help                Print this help message
  ad -v | --version             Print version information
";

fn main() {
    let Args {
        script,
        files,
        ninep_args,
    } = parse_args();

    if !ninep_args.is_empty() {
        return run_9p_oneshot(ninep_args);
    } else if let Some(script) = script {
        return run_script(&script, files);
    }

    let log_buffer = LogBuffer::default();
    let builder = tracing_subscriber::fmt()
        .compact()
        .with_ansi(false)
        .with_target(false)
        .with_writer(log_buffer.clone())
        .with_max_level(log_level_from_env());

    let subscriber = builder.finish();
    set_global_default(subscriber).expect("unable to set a global tracing subscriber");

    let config = match Config::try_load() {
        Ok(config) => config,
        Err(s) => {
            error!("unable to load config: {s}");
            Config::default()
        }
    };

    let plumbing_rules = match PlumbingRules::try_load() {
        Ok(rules) => rules,
        Err(s) => {
            error!("unable to load plumbing rules: {s}");
            PlumbingRules::default() // Empty plumbing rules
        }
    };

    let mut e = Editor::new(config, plumbing_rules, EditorMode::Terminal, log_buffer);
    for fname in files.iter() {
        e.open_file_relative_to_cwd(fname, false);
    }

    e.run()
}

fn log_level_from_env() -> LevelFilter {
    match env::var(LOG_LEVEL_ENV_VAR) {
        Ok(s) => s.parse().unwrap_or(LevelFilter::INFO),
        Err(_) => LevelFilter::INFO,
    }
}

struct Args {
    script: Option<String>,
    files: Vec<String>,
    ninep_args: Vec<String>,
}

fn fatal(msg: &str) -> ! {
    eprintln!("{msg}");
    exit(1);
}

fn parse_args() -> Args {
    let mut args = env::args().skip(1).peekable();

    match args.next().as_deref() {
        // no files to open
        None => Args {
            script: None,
            files: Vec::new(),
            ninep_args: Vec::new(),
        },

        // Running as a simple 9p client
        Some("-9p") => Args {
            script: None,
            files: Vec::new(),
            ninep_args: args.collect(),
        },

        // script expression to run
        Some("-e" | "--expression") => {
            let script = match args.next() {
                Some(script) => Some(script),
                None => fatal("no script provided"),
            };
            let files: Vec<String> = args.collect();
            Args {
                script,
                files,
                ninep_args: Vec::new(),
            }
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
            Args {
                script,
                files,
                ninep_args: Vec::new(),
            }
        }

        // help and version info
        Some("-h" | "--help") => show_help(),
        Some("-v" | "--version") => show_version_info(),

        // files to open
        Some(fname) => {
            let mut files = vec![fname.to_string()];
            files.extend(args);

            Args {
                script: None,
                files,
                ninep_args: Vec::new(),
            }
        }
    }
}

fn run_script(script: &str, files: Vec<String>) {
    let mut prog = match Program::try_parse(script) {
        Ok(prog) => prog,
        Err(e) => {
            eprintln!("error parsing script: {e:?}");
            exit(1);
        }
    };
    let mut buf = vec![];

    if files.is_empty() {
        // Read from stdin and write directly to stdout
        match prog.execute(&mut CachedStdin::new(), "stdin", &mut io::stdout()) {
            Ok(_) => return,
            Err(e) => {
                eprintln!("error running script: {e:?}");
                exit(1);
            }
        }
    }

    // Buffer output from running over each provided file
    for path in files.iter() {
        let s = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("unable to open file '{path}': {e}");
                exit(1);
            }
        };

        if let Err(e) = prog.execute(&mut GapBuffer::from(s), path, &mut buf) {
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

fn run_9p_oneshot(args: Vec<String>) {
    let mut args = args.into_iter().peekable();

    let aname = match args.peek() {
        Some(s) => {
            if s == "-A" {
                args.next();
                match args.next() {
                    Some(aname) => aname,
                    None => fatal("no aname provided"),
                }
            } else {
                String::new()
            }
        }
        None => fatal("no aname provided"),
    };

    let cmd = match args.next() {
        Some(cmd) => cmd,
        None => fatal("no 9p command provided"),
    };

    let path_opt = args.next();
    let (ns, path) = match path_opt.as_ref() {
        Some(path) => match path.split_once('/') {
            Some((ns, path)) => (ns, path),
            None => (path.as_str(), ""),
        },
        None => fatal("no path provided"),
    };

    let client = match UnixClient::new_unix(ns, aname) {
        Ok(client) => client,
        Err(e) => fatal(&e.to_string()),
    };

    if let Err(e) = run_9p_command(&cmd, path, client) {
        fatal(&e.to_string());
    }
}

fn run_9p_command(cmd: &str, path: &str, mut client: UnixClient) -> io::Result<()> {
    match cmd {
        "read" => print!("{}", client.read_str(path)?),

        "write" => {
            let mut content = String::new();
            io::stdin().read_to_string(&mut content)?;
            client.write_str(path, 0, &content)?;
        }

        "ls" => {
            for stat in client.read_dir(path)?.into_iter() {
                println!("{}", stat.fm.name);
            }
        }

        _ => fatal(&format!("unknown command: {cmd}")),
    }

    Ok(())
}
