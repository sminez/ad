use ad_editor::{
    CliAction, Config, ConfigSource, Editor, EditorMode, LOG_LEVEL_ENV_VAR, LogBuffer, ParsedArgs,
    PlumbingRules, Program, USAGE, VERSION,
    buffer::GapBuffer,
    client::{list_open_sessions, oneshot_9p, remove_open_sockets},
    exec::SystemRunner,
    regex::CachingStream,
};
use std::{
    env, fs,
    io::{self, stdin},
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
            eprintln!("{msg}");
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
        CliAction::NineP { cmd, path, aname } => return oneshot_9p(cmd, path, aname),
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
