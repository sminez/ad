use ad_editor::{
    CliAction, Cmd9p, Config, ConfigSource, Editor, EditorMode, LOG_LEVEL_ENV_VAR, LogBuffer,
    ParsedArgs, PlumbingRules, Program, USAGE, VERSION, buffer::GapBuffer, exec::SystemRunner,
    regex::CachingStream,
};
use ninep::{sansio::server::socket_dir, sync::client::UnixClient};
use std::{
    env, fmt, fs,
    io::{self, Read, stdin},
    path::PathBuf,
    process::exit,
};
use tracing::{level_filters::LevelFilter, subscriber::set_global_default};

fn main() {
    let ParsedArgs {
        action,
        config_source,
    } = match ParsedArgs::try_parse() {
        Ok(parsed) => parsed,
        Err(msg) => {
            println!("{msg}");
            exit(1);
        }
    };

    let files = match action {
        // Only the OpenEditor action results in running the main editor behaviour
        CliAction::OpenEditor { files } => files,

        // All other actions are run immediately before exiting
        CliAction::ShowHelp => print_and_exit(USAGE),
        CliAction::ShowVersion => print_and_exit(&format!("ad v{VERSION}")),
        CliAction::RunScript { script, files } => return run_script(&script, files),
        CliAction::NineP { aname, cmd, path } => return run_9p(aname, cmd, path),
        CliAction::ListSessions => return list_open_sessions(),
        CliAction::RmSockets => return remove_open_sockets(),
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

    let config = match config_source {
        ConfigSource::Default => Ok(Config::default()),
        ConfigSource::User => Config::try_load(),
        ConfigSource::Custom(path) => {
            let home = env::var("HOME").unwrap();
            Config::try_load_from_path(&path.to_string_lossy(), &home)
        }
    };

    let mut e = Editor::new_with_initial_files(
        config,
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

fn print_and_exit(msg: &str) -> ! {
    println!("{msg}");
    exit(0);
}

fn log_level_from_env() -> LevelFilter {
    match env::var(LOG_LEVEL_ENV_VAR) {
        Ok(s) => s.parse().unwrap_or(LevelFilter::INFO),
        Err(_) => LevelFilter::INFO,
    }
}

fn run_script(script: &str, files: Vec<PathBuf>) {
    let prog = match Program::try_parse(script) {
        Ok(prog) => prog,
        Err(e) => {
            eprintln!("error parsing script: {e:?}");
            exit(1);
        }
    };
    let mut stdout = io::stdout();
    let mut runner = SystemRunner::new(env::current_dir().unwrap());

    if files.is_empty() {
        let mut haystack = CachingStream::new(stdin());
        match prog.execute(&mut haystack, &mut runner, "stdin", &mut stdout) {
            Ok(_) => return,
            Err(e) => {
                eprintln!("error running script: {e:?}");
                exit(1);
            }
        }
    }

    for path in files.iter() {
        let s = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("unable to open file '{}': {e}", path.display());
                exit(1);
            }
        };

        let mut gb = GapBuffer::from(s);
        runner.set_dir(path.parent().unwrap());
        if let Err(e) = prog.execute(&mut gb, &mut runner, path.to_str().unwrap(), &mut stdout) {
            eprintln!("error running script: {e:?}");
            exit(1);
        }
    }
}

fn run_9p(aname: String, action: Cmd9p, path: String) {
    let (ns, path) = match path.split_once('/') {
        Some((ns, path)) => (ns, path),
        None => (path.as_str(), ""),
    };

    let client = match client_for_ns(ns, aname) {
        Ok(client) => client,
        Err(e) => fatal(e.to_string()),
    };

    if let Err(e) = run_9p_command(action, path, client) {
        fatal(e.to_string());
    }
}

fn run_9p_command(action: Cmd9p, path: &str, mut client: UnixClient) -> io::Result<()> {
    match action {
        Cmd9p::Read => {
            for line in client.iter_lines(path)? {
                println!("{line}");
            }
        }

        Cmd9p::Write => {
            let mut content = String::new();
            io::stdin().read_to_string(&mut content)?;
            client.write_str(path, 0, &content)?;
        }

        Cmd9p::List => {
            for stat in client.read_dir(path)?.into_iter() {
                println!("{}", stat.fm.name);
            }
        }
    }

    Ok(())
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
            if UnixClient::new_unix(&ns, "").is_err() {
                let path = d.join(ns);
                println!("removing unresponsive ad socket at {}", path.display());
                fs::remove_file(path)?;
            }
        }

        Ok(())
    }

    if let Err(e) = inner() {
        fatal(format!("unable to remove open 9p sockets: {e}"));
    }
}
