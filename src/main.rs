use ad_editor::{
    Args, CachedStdin, Config, Editor, EditorMode, LogBuffer, PlumbingRules, Program,
    LOG_LEVEL_ENV_VAR,
};
use ninep::sync::client::UnixClient;
use std::{
    env, fs,
    io::{self, Read, Write},
    process::exit,
};
use tracing::{error, level_filters::LevelFilter, subscriber::set_global_default};

fn main() {
    let args = match Args::try_parse() {
        Ok(args) => args,
        Err((msg, code)) => {
            println!("{msg}");
            exit(code);
        }
    };

    let Args {
        script,
        files,
        ninep_args,
    } = args;

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

fn fatal(msg: &str) -> ! {
    eprintln!("{msg}");
    exit(1);
}

fn log_level_from_env() -> LevelFilter {
    match env::var(LOG_LEVEL_ENV_VAR) {
        Ok(s) => s.parse().unwrap_or(LevelFilter::INFO),
        Err(_) => LevelFilter::INFO,
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

        if let Err(e) = prog.execute_on_string(s, path, &mut buf) {
            eprintln!("error running script: {e:?}");
            exit(1);
        }
    }

    io::stdout().write_all(&buf).unwrap();
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
