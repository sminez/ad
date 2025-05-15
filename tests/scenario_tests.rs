//! Data driven tests that run the editor in headless mode to provide full testing
//! of the main editor behaviour (other than the UI).
//!
//! Test files are written in txtar format and have the following supported
//! sections:
//!   - config
//!     path to a file to load as config (default: data/config.toml)
//!   - plumbing-rules
//!     path to a file to load as plumbing rules (default: empty ruleset)
//!   - file-X
//!     inline file content for a file that should be created and opened on startup
//!     first line is the path to use for the file
//!   - actions
//!     one action per line (see the "TestAction" enum below)
//!   - buffer-list
//!     a list of buffer IDs and their expected file paths in the format used
//!     by the minibuffer buffer list. '*' marks the buffer that is expected
//!     to be focused
//!   - expected-windows
//!     the expected columns and their ordered buffer IDs (column per-line)
//!     "1 3"   <- first column should contain two windows with ids 1 & 3
//!     "2"     <- second column should contain a single window with id 2
//!   - expected-buffer-X
//!     the expected final content for the buffer with ID X
//!   - expected-buffer-dot-X
//!     the expected content of the Dot for the buffer with ID X
//!
//! Any comment section present at the top of a test file will be printed before
//! the test is run for additional debugging context in the event of a test
//! failure.
use ad_editor::{
    buffer::BufferId,
    editor::{Action, Click, MiniBufferState},
    input::Event,
    key::Input,
    system::DefaultSystem,
    term::CurShape,
    ui::{Layout, StateChange, UserInterface},
    Config, Editor, EditorMode, LogBuffer, PlumbingRules,
};
use ninep::sync::client::UnixClient;
use serial_test::serial;
use simple_test_case::dir_cases;
use simple_txtar::{Archive, File};
use std::{
    env, fs, io,
    path::{Path, PathBuf},
    sync::{mpsc::Sender, Arc, Mutex},
    thread::{sleep, spawn},
    time::{Duration, SystemTime},
};

/// The number of milliseconds to sleep before sending a noop when a render is triggered while we
/// have an outstanding fsys opertaion pending.
/// Also used as our poll interval while we wait for fsys to come up before starting a test run
/// that requires it to be running.
const FSYS_SLEEP_MS: u64 = 100;
/// The maximum number of times we will check to see if the fsys socket has been created before
/// bailing on a test case that requires it.
const FSYS_MAX_TRIES: usize = 10;

#[dir_cases(
    "tests/data/editor-scenarios/edit_mode",
    "tests/data/editor-scenarios/exec",
    "tests/data/editor-scenarios/fsys",
    "tests/data/editor-scenarios/general",
    "tests/data/editor-scenarios/issues",
    "tests/data/editor-scenarios/plumbing"
)]
#[test]
#[serial]
fn editor_scenarios(path: &str, content: &str) {
    // Parse the given test case file and validate it before initialising the editor
    let TestCase {
        test_id,
        mut setup,
        assertions,
        status_messages,
    } = TestCase::from_archive(path, content);

    if !assertions.is_valid() {
        panic!("no assertions provided");
    }

    // Create a new temp directory to hold our test files while the test runs
    let dir = env::temp_dir().join(&test_id);
    let test_file_dir = dir.join("files");
    let socket_path = dir.join("sock");

    fs::create_dir_all(&test_file_dir).expect("unable to create temp directory");
    println!("using {} for the fsys socket", socket_path.display());
    println!("using {} for test files", test_file_dir.display());
    println!("config fsys.enabled={}", setup.config.filesystem.enabled);

    setup.ui.socket_path = socket_path.clone();

    let mut e = Editor::new_with_system(
        setup.config,
        setup.plumbing_rules,
        EditorMode::Boxed(Box::new(setup.ui)),
        LogBuffer::default(),
        DefaultSystem::without_clipboard_provider(),
    );

    for f in setup.files.into_iter() {
        let p = test_file_dir.join(&f.name);
        if let Some(parent) = p.parent() {
            _ = fs::create_dir_all(parent);
        }
        if let Err(e) = fs::write(&p, f.content) {
            fs::remove_dir_all(test_file_dir).expect("unable to remove temp directory");
            panic!("failed to write test file {}: {e}", f.name);
        }

        e.open_file_relative_to_cwd(p, false);
    }

    if let Some(err) = setup.config_err {
        e.open_virtual(
            "+config-error",
            format!("Unable to load config file:\n{err}"),
            true,
        );
        println!(">> CONFIG LOAD ERROR:\n{err}\n");
    }

    e.run_with_explicit_fsys_path(socket_path.clone());

    _ = fs::remove_file(socket_path);
    _ = fs::remove_dir_all(&dir);

    let status_hist = status_messages.lock().unwrap().join("\n");
    println!(">> STATUS HISTORY:\n{status_hist}");

    assertions.verify(&e, &test_file_dir);
}

/// A parsed test case from a scenario file
#[derive(Debug)]
struct TestCase {
    test_id: String,
    setup: Setup,
    assertions: Assertions,
    status_messages: Arc<Mutex<Vec<String>>>,
}

impl TestCase {
    fn from_archive(path: &str, content: &str) -> Self {
        let arr = Archive::from(content);
        let home = env::var("HOME").unwrap();
        let uname = env::var("USER").unwrap();

        let comment = arr.comment();
        if !comment.is_empty() {
            println!("{}", comment.trim());
        }

        let config_path = match arr.get("config") {
            Some(f) => f.content.trim(),
            None => "data/config.toml",
        };
        let (config, config_err) = Config::try_load_from_path(config_path, &home);

        let plumbing_rules = match arr.get("plumbing-rules") {
            Some(f) => PlumbingRules::try_load_from_path(f.content.trim()).unwrap_or_default(),
            None => PlumbingRules::default(),
        };

        let actions = match arr.get("actions") {
            Some(f) => parse_actions(f.content.trim()),
            None => Vec::new(),
        };

        let buffer_list = arr.get("buffer-list").map(|f| f.content.trim().to_string());
        let windows = match arr.get("expected-windows") {
            Some(f) => {
                let mut windows = Vec::with_capacity(f.content.lines().count());
                for line in f.content.lines() {
                    if line.starts_with('#') || line.is_empty() {
                        continue;
                    }
                    let col: Vec<BufferId> = line
                        .split_whitespace()
                        .map(|s| s.parse().unwrap())
                        .collect();
                    windows.push(col);
                }
                windows
            }
            None => Vec::new(),
        };

        let mut files = Vec::new();
        let mut buffer_dots = Vec::new();
        let mut buffer_contents = Vec::new();

        for mut file in arr.into_iter() {
            if file.name.starts_with("file-") {
                file.name = file.name.strip_prefix("file-").unwrap().to_string();
                if file.content.ends_with('\n') {
                    file.content.pop();
                }
                files.push(file);
            } else if file.name.starts_with("expected-buffer-dot-") {
                let id: BufferId = file
                    .name
                    .strip_prefix("expected-buffer-dot-")
                    .unwrap()
                    .parse()
                    .unwrap();
                if file.content.ends_with('\n') {
                    file.content.pop();
                }
                buffer_dots.push((id, file.content));
            } else if file.name.starts_with("expected-buffer-") {
                let id: BufferId = file
                    .name
                    .strip_prefix("expected-buffer-")
                    .unwrap()
                    .parse()
                    .unwrap();
                if file.content.ends_with('\n') {
                    file.content.pop();
                }
                buffer_contents.push((id, file.content));
            }
        }

        let status_messages = Arc::new(Mutex::new(Vec::new()));

        // Unique ID for our testing temp directory and any fsys socket that gets created
        let test_id = format!(
            "ad-tests-{}-{}",
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
            path.replace("tests/data/editor-scenarios/", "")
        );

        Self {
            test_id,
            setup: Setup {
                config,
                config_err,
                plumbing_rules,
                files,
                ui: ScriptedUi {
                    uname,
                    socket_path: PathBuf::new(),
                    actions,
                    pending_fsys: Arc::new(Mutex::new(false)),
                    status_messages: status_messages.clone(),
                    tx: None,
                },
            },
            assertions: Assertions {
                buffer_list,
                windows,
                buffer_contents,
                buffer_dots,
            },
            status_messages,
        }
    }
}

#[derive(Debug)]
struct Setup {
    config: Config,
    config_err: Option<String>,
    plumbing_rules: PlumbingRules,
    files: Vec<File>,
    ui: ScriptedUi,
}

#[derive(Debug)]
struct Assertions {
    buffer_list: Option<String>,
    windows: Vec<Vec<BufferId>>,
    buffer_contents: Vec<(BufferId, String)>,
    buffer_dots: Vec<(BufferId, String)>,
}

impl Assertions {
    /// Test case assertions are valid as long as there is at least one thing being asserted
    fn is_valid(&self) -> bool {
        self.buffer_list.is_some()
            || !self.windows.is_empty()
            || !self.buffer_contents.is_empty()
            || !self.buffer_dots.is_empty()
    }

    fn verify(&self, e: &Editor<DefaultSystem>, test_dir: &Path) {
        if let Some(s) = self.buffer_list.as_ref() {
            let blist = e
                .buffer_list()
                .join("\n")
                .replace(&format!("{}/", test_dir.display()), "");

            assert_eq!(s, &blist, "incorrect buffer listing");
        }

        if !self.windows.is_empty() {
            assert_eq!(self.windows, e.layout_ids(), "incorrect window state")
        }

        for (id, expected) in self.buffer_contents.iter() {
            assert_eq!(
                Some(expected),
                e.buffer_content(*id).as_ref(),
                "incorrect buffer content for id={id}"
            );
        }

        for (id, expected) in self.buffer_dots.iter() {
            assert_eq!(
                Some(expected),
                e.buffer_dot(*id).as_ref(),
                "incorrect buffer dot content for id={id}"
            );
        }
    }
}

#[derive(Debug)]
struct ScriptedUi {
    uname: String,
    socket_path: PathBuf,
    actions: Vec<TestAction>,
    pending_fsys: Arc<Mutex<bool>>,
    status_messages: Arc<Mutex<Vec<String>>>,
    tx: Option<Sender<Event>>,
}

impl ScriptedUi {
    fn spawn_fsys(&self, f: Fsys) {
        let mut client =
            match UnixClient::new_unix_with_explicit_path(&self.uname, &self.socket_path, "") {
                Ok(client) => client,
                Err(e) => {
                    println!(">>> UNABLE TO CREATE FSYS CLIENT: {e}");
                    return;
                }
            };

        *self.pending_fsys.lock().unwrap() = true;
        let pending = self.pending_fsys.clone();

        spawn(move || {
            let inner = move || {
                match f {
                    Fsys::Read(path) => {
                        let s = client.read_str(&path)?;
                        println!("read {path}: {s:?}");
                    }

                    Fsys::ReadDir(path) => {
                        for stat in client.read_dir(&path)?.into_iter() {
                            println!("read dir ({path}): {}", stat.fm.name);
                        }
                    }

                    Fsys::Write(path, content) => {
                        client.write_str(&path, 0, &content)?;
                        println!("wrote to {path}: {content:?}");
                    }
                }

                io::Result::Ok(())
            };

            if let Err(e) = inner() {
                println!(">>> FSYS ERROR: {e}");
            }

            *pending.lock().unwrap() = false;
        });
    }
}

impl UserInterface for ScriptedUi {
    fn init(&mut self, tx: Sender<Event>) -> (usize, usize) {
        self.tx = Some(tx);

        let need_fsys_socket = self
            .actions
            .iter()
            .any(|a| matches!(a, TestAction::Fsys(_)));

        if need_fsys_socket {
            let mut n = 0;
            while !fs::exists(&self.socket_path).unwrap() {
                n += 1;
                if n == FSYS_MAX_TRIES {
                    panic!("fsys failed to come up...");
                }

                println!("waiting for fsys to come up");
                sleep(Duration::from_millis(FSYS_SLEEP_MS));
            }
        }

        (60, 80)
    }

    fn shutdown(&mut self) {}

    fn state_change(&mut self, change: StateChange) {
        match change {
            StateChange::ConfigUpdated => (),
            StateChange::StatusMessage { msg } => {
                self.status_messages.lock().unwrap().push(msg);
            }
        }
    }

    fn refresh(
        &mut self,
        _mode_name: &str,
        _layout: &Layout,
        _n_running: usize,
        _pending_keys: &[Input],
        _held_click: Option<&Click>,
        _mb: Option<MiniBufferState<'_>>,
    ) {
        let event = if *self.pending_fsys.lock().unwrap() {
            // We need to allow for the fsys thread to communicate with the main editor event loop
            // which triggers additional refreshes for when our message actually comes through to
            // the event loop
            sleep(Duration::from_millis(FSYS_SLEEP_MS));
            Event::Action(Action::Noop)
        } else {
            match self.actions.pop() {
                Some(TestAction::Fsys(f)) => {
                    self.spawn_fsys(f);
                    Event::Action(Action::Noop)
                }

                Some(TestAction::SleepMs(n)) => {
                    sleep(Duration::from_millis(n));
                    Event::Action(Action::Noop)
                }

                Some(TestAction::Input(input)) => Event::Input(input),

                None => Event::Action(Action::Exit { force: true }),
            }
        };

        self.tx.as_ref().unwrap().send(event).unwrap();
    }

    fn set_cursor_shape(&mut self, _cur_shape: CurShape) {}
}

#[derive(Debug)]
enum Fsys {
    Read(String),
    ReadDir(String),
    Write(String, String),
}

/// User input actions to send to the editor.
/// Note that the whitespace after the colon following the action name is required for the simple
/// parser being used here. Blank lines and lines beginning with a '#' will be treated as comments
/// and are ignored, for valid action definitions see each of the variants of the enum.
#[derive(Debug)]
enum TestAction {
    /// Send input to the editor as if it had been typed at the keyboard.
    ///
    /// # Examples
    ///
    /// type: ihello, world!
    /// type: <esc>
    /// type: <alt>$single_character
    Input(Input),
    /// Sleep for a given number of miliseconds.
    /// This may be required when running external programs through loading and executing.
    ///
    /// # Examples
    ///
    /// sleep_ms: 200
    SleepMs(u64),
    /// Read or write a control file from the 9p virtual filesystem.
    ///
    /// # Examples
    ///
    /// fsys: read buffers/1/body
    /// fsys: write buffers/1/dot
    /// fsys: ls buffers/1
    Fsys(Fsys),
}

fn parse_actions(raw: &str) -> Vec<TestAction> {
    let mut actions = Vec::with_capacity(raw.lines().count());

    for line in raw.lines() {
        if line.starts_with('#') || line.is_empty() {
            continue;
        } else if let Some(s) = line.strip_prefix("sleep_ms: ") {
            let n: u64 = match s.trim().parse() {
                Ok(n) => n,
                Err(e) => panic!("invalid sleep duration: {e}"),
            };
            actions.push(TestAction::SleepMs(n));
        } else if let Some(s) = line.strip_prefix("type: ") {
            match s {
                "<esc>" => actions.push(TestAction::Input(Input::Esc)),

                s if s.starts_with("<alt>") => {
                    let tail = escape(s.strip_prefix("<alt>").unwrap().trim());
                    let mut it = tail.chars();
                    match (it.next(), it.next()) {
                        (Some('\n'), None) => actions.push(TestAction::Input(Input::AltReturn)),
                        (Some(ch), None) => actions.push(TestAction::Input(Input::Alt(ch))),
                        (None, _) => panic!("invalid <alt> input: expected a character"),
                        (_, Some(_)) => {
                            panic!("invalid <alt> input: expected a single char, got {tail:?}");
                        }
                    }
                }

                _ => {
                    for ch in escape(s).chars() {
                        actions.push(TestAction::Input(char_as_input(ch)));
                    }
                }
            }
        } else if let Some(s) = line.strip_prefix("fsys: ") {
            let f = match s.split_once(' ') {
                Some(("read", path)) => Fsys::Read(path.to_string()),
                Some(("ls", path)) => Fsys::ReadDir(path.to_string()),
                Some(("write", tail)) => match tail.split_once(' ') {
                    Some((path, content)) => Fsys::Write(path.to_string(), content.to_string()),
                    None => panic!("invalid fsys line: {s:?}"),
                },

                _ => panic!("invalid fsys line: {s:?}"),
            };

            actions.push(TestAction::Fsys(f));
        } else {
            panic!("malformed action line: {line:?}");
        }
    }

    actions.reverse(); // so we can pop from the end while running
    actions
}

fn escape(s: &str) -> String {
    s.replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\\\", "\\")
}

fn char_as_input(ch: char) -> Input {
    match ch {
        '\n' => Input::Return,
        '\t' => Input::Tab,
        _ => Input::Char(ch),
    }
}
