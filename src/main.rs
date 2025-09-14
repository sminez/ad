use ad_editor::{
    Args, CachedStdin, Config, Editor, EditorMode, LOG_LEVEL_ENV_VAR, LogBuffer, PlumbingRules,
    Program,
};
use ninep::{sansio::server::socket_dir, sync::client::UnixClient};
use std::{
    env, fmt, fs,
    io::{self, Read, Write},
    process::exit,
};
use tracing::{level_filters::LevelFilter, subscriber::set_global_default};

fn main() {
    let args = match Args::try_parse() {
        Ok(args) => args,
        Err((msg, code)) => {
            println!("{msg}");
            exit(code);
        }
    };

    let files = match args {
        Args::RunScript { script, files } => return run_script(&script, files),
        Args::NineP { args } => return run_9p_oneshot(args),
        Args::ListSessions => return list_open_sessions(),
        Args::RmSockets => return remove_open_sockets(),
        Args::OpenEditor { files } => files,
    };

    let log_buffer = LogBuffer::default();
    let builder = tracing_subscriber::fmt()
        .compact()
        .with_ansi(false)
        .with_target(false)
        .with_writer(log_buffer.clone())
        .with_max_level(log_level_from_env());

    let subscriber = builder.finish();
    set_global_default(subscriber).expect("unable to set a global tracing subscriber");

    let mut e = Editor::new_with_initial_files(
        Config::try_load(),
        PlumbingRules::try_load(),
        EditorMode::Terminal,
        log_buffer,
        &files,
    );

    e.run()
}

fn fatal(msg: impl fmt::Display) -> ! {
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

    let client = match client_for_ns(ns, aname) {
        Ok(client) => client,
        Err(e) => fatal(e.to_string()),
    };

    if let Err(e) = run_9p_command(&cmd, path, client) {
        fatal(e.to_string());
    }
}

/// Depending on the requested namespace and the presence or absence of an "AD_PID" env var we may
/// need to adjust the ns to include an ad PID
fn client_for_ns(ns: &str, aname: String) -> io::Result<UnixClient> {
    if ns != "ad" {
        return UnixClient::new_unix(ns, aname);
    }

    let mut ns = ns.to_string();
    if let Ok(pid) = env::var("AD_PID") {
        ns.push('-');
        ns.push_str(&pid);
    } else {
        // If there is only a single running ad instance then we can attach to that, otherwise we
        // need to error out and prompt the user to select the appropriate instance they want to
        // connect to.
        let mut ad_sockets = open_9p_sockets()?;
        match ad_sockets.len() {
            1 => ns = ad_sockets.remove(0),
            0 => fatal("No such file or directory"),
            _ => fatal(format!(
                "please specify which ad instance to connect to:\n{}",
                ad_sockets.join("\n")
            )),
        }
    };

    UnixClient::new_unix(ns, aname)
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

        _ => fatal(format!("unknown command: {cmd}")),
    }

    Ok(())
}

fn open_9p_sockets() -> io::Result<Vec<String>> {
    let mut ad_sockets = Vec::new();
    for entry in fs::read_dir(socket_dir())? {
        let entry = entry?;
        let fname = entry.file_name();
        if let Some(s) = fname.to_str()
            && s.starts_with("ad-")
        {
            ad_sockets.push(s.to_string());
        }
    }

    Ok(ad_sockets)
}

fn list_open_sessions() {
    fn inner() -> io::Result<()> {
        let mut had_unresponsive = false;

        for ns in open_9p_sockets()?.into_iter() {
            let mut client = match UnixClient::new_unix(&ns, "") {
                Ok(client) => client,
                Err(e) => {
                    println!("{ns}\tunresponsive: {e}");
                    had_unresponsive = true;
                    continue;
                }
            };
            let id = client.read_str("buffers/current")?;
            let fname = client.read_str(format!("buffers/{id}/filename"))?;
            println!("{ns}\t{fname}");
        }

        if had_unresponsive {
            println!("\nYou can remove unresponsive sockets using --rm-sockets");
        }

        Ok(())
    }

    if let Err(e) = inner() {
        fatal(format!("unable to list open editor sessions: {e}"));
    }
}

fn remove_open_sockets() {
    fn inner() -> io::Result<()> {
        let d = socket_dir();
        for ns in open_9p_sockets()?.into_iter() {
            let path = d.join(ns);
            println!("removing {}", path.display());
            fs::remove_file(path)?;
        }

        Ok(())
    }

    if let Err(e) = inner() {
        fatal(format!("unable to remove open 9p sockets: {e}"));
    }
}
