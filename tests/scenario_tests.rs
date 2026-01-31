//! Data driven tests that run the editor in headless mode to provide full testing
//! of the main editor behaviour (other than the UI).
//!
//! See TestCase::from_archive for details of the supported file sections.
use ad_editor::{
    Config, Editor, EditorMode, LogBuffer, PlumbingRules,
    buffer::BufferId,
    editor::{Action, Click, MiniBufferState},
    input::Event,
    key::Input,
    system::DefaultSystem,
    ui::{Layout, StateChange, UserInterface, style::CurShape},
};
use assert_fs::TempDir;
use ninep::sync::client::UnixClient;
use simple_test_case::dir_cases;
use simple_txtar::{Archive, File};
use std::{
    env, fs, io,
    path::{Path, PathBuf},
    str::FromStr,
    sync::{Arc, Mutex, mpsc::Sender},
    thread::{sleep, spawn},
    time::Duration,
};

/// The number of milliseconds to sleep before sending a noop when a render is triggered while we
/// have an outstanding fsys operation pending.
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
fn editor_scenarios(path: &str, content: &str) {
    // Parse the given test case file and validate it before initialising the editor
    let TestCase {
        mut setup,
        assertions,
        status_messages,
    } = TestCase::from_archive(path, content);

    if assertions.is_empty() {
        panic!("no assertions provided");
    }

    // Create a new temp directory to hold our test files while the test runs
    let tmp = TempDir::new().unwrap();
    let test_file_dir = tmp.join("files");
    let socket_path = tmp.join("sock");
    setup.ui.socket_path = socket_path.clone();

    fs::create_dir_all(&test_file_dir).expect("unable to create temp directory");
    println!("using temp directory: {}", tmp.path().display());
    println!("using {} for the fsys socket", socket_path.display());
    println!("using {} for test files", test_file_dir.display());

    // Write out all of our test files into the new temp directory and store the paths
    // so they can be passed to the editor.
    let file_paths: Vec<PathBuf> = setup
        .files
        .into_iter()
        .map(|f| {
            let p = test_file_dir.join(&f.name);
            if let Some(parent) = p.parent() {
                _ = fs::create_dir_all(parent);
            }
            if let Err(e) = fs::write(&p, f.content) {
                panic!("failed to write test file {}: {e}", f.name);
            }

            p
        })
        .collect();

    let mut e = Editor::new_with_system_and_initial_files(
        setup.config_res,
        setup.plumbing_rules_res,
        EditorMode::Boxed(Box::new(setup.ui)),
        LogBuffer::default(),
        DefaultSystem::without_clipboard_provider(),
        &file_paths,
    );

    e.run_with_explicit_fsys_path(socket_path);

    let status_hist = status_messages.lock().unwrap().join("\n");
    println!(">> STATUS HISTORY:\n{status_hist}");

    assertions.verify(&e, &test_file_dir);
}

/// A parsed test case from a scenario file
#[derive(Debug)]
struct TestCase {
    setup: Setup,
    assertions: Assertions,
    status_messages: Arc<Mutex<Vec<String>>>,
}

impl TestCase {
    /// A bare bones parser for the txtar test file format used by the editor_scenarios test above.
    ///
    /// If the file being parsed is malformed in any way this method will panic in order to fail
    /// the test. The parsing of each section of the archive is documented in inline comments
    /// within parser itself.
    fn from_archive(path: &str, content: &str) -> Self {
        let arr = Archive::from(content);
        let home = env::var("HOME").expect("HOME env var required for parsing config");
        let uname = env::var("USER").expect("USER env var required for connecting to fsys");

        // If there is a top level comment to the archive we print it for additional debugging
        // context if the test case fails or the rest of the file fails to parse.
        let comment = arr.comment();
        if !comment.is_empty() {
            println!("{}", comment.trim());
        }

        // -- config --
        // Default config can overwritten by providing a config file inline or as a file path by
        // providing a single line of the form "path: path/to/config.toml".
        let config_res = match arr.get("config") {
            Some(f) => {
                let s = f.content.trim();
                match s.strip_prefix("path: ") {
                    Some(p) => Config::try_load_from_path(p, &home),
                    None => Config::try_load_from_str(s, path, &home),
                }
            }
            None => Ok(Config::default()),
        };

        // -- plumbing-rules --
        // Same idea for plumbing rules
        let plumbing_rules_res = match arr.get("plumbing-rules") {
            Some(f) => {
                let s = f.content.trim();
                match s.strip_prefix("path: ") {
                    Some(p) => PlumbingRules::try_load_from_path(p),
                    None => PlumbingRules::from_str(s),
                }
            }
            None => Ok(PlumbingRules::default()),
        };

        // -- actions --
        // Actions are not required as the setup of the test alone may be all we need but
        // we provide a default sleep no-op action to handle the first render call that
        // comes through when the editor starts up.
        let actions = match arr.get("actions") {
            Some(f) => parse_actions(f.content.trim()),
            None => vec![TestAction::SleepMs(100)],
        };

        // -- buffer-list --
        // The buffer list is just a raw string that we compare to the final listing that
        // the user can open in the minibuffer using 2"<space> b". We trim the working
        // directory from the start of each path in the real listing so the content in a
        // test case should just be the paths as provided in -- file-X -- sections.
        // (See the file-X section below for more details)
        let buffer_list = arr.get("buffer-list").map(|f| f.content.trim().to_string());

        // -- expected-windows --
        // Expected windows are specified as the ordered buffer IDs per column, one
        // column per line. So,
        // ```
        // 1 2
        // 3 4 5
        // ```
        // Would expect there to be two column in the UI layout, the first containing
        // buffers 1 and 2 (top to bottom) and the second containing buffers 3, 4 and
        // 5 (again, top to bottom).
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

        // The remaining file sections are defined using prefixes rather than each
        // having a pre-defined name. They may be repeated as often as desired but
        // collisions in names are invalid and will result in a panic.
        let mut files: Vec<File> = Vec::new();
        let mut buffer_dots: Vec<(BufferId, String)> = Vec::new();
        let mut buffer_contents: Vec<(BufferId, String)> = Vec::new();

        let strip_trailing_newline = |f: &mut File| {
            if f.content.ends_with('\n') {
                f.content.pop();
            }
        };
        let parse_bufid = |str_id: &str| {
            str_id
                .parse::<BufferId>()
                .unwrap_or_else(|_| panic!("invalid buffer ID: {str_id:?}"))
        };

        for mut file in arr.into_iter() {
            if let Some(fname) = file.name.strip_prefix("file-") {
                // -- file-$filepath --
                // File sections define a file that should be present and loaded in the editor
                // as part of startup (as if the file path had been provided as an argument on
                // the command line).
                // Trailing newlines are stripped so that it is possible to have a completely
                // empty input file by providing a file section without any content.
                file.name = fname.to_string();
                if files.iter().any(|f| f.name == file.name) {
                    panic!(">>> ERROR duplicate test file name: {:?}", file.name);
                }
                strip_trailing_newline(&mut file);
                files.push(file);
            } else if let Some(str_id) = file.name.strip_prefix("expected-buffer-dot-") {
                // --expected-buffer-dot-$bufid --
                // Specify the expected content of the dot for a given buffer after all test
                // actions have been run.
                let id = parse_bufid(str_id);
                if buffer_dots.iter().any(|(known_id, _)| *known_id == id) {
                    panic!(">>> ERROR duplicate expected-buffer-dot section for ID={id}");
                }
                strip_trailing_newline(&mut file);
                buffer_dots.push((id, file.content));
            } else if let Some(str_id) = file.name.strip_prefix("expected-buffer-") {
                // --expected-buffer-$bufid --
                // Specify the expected content of a given buffer after all test actions have
                // been run.
                let id = parse_bufid(str_id);
                if buffer_contents.iter().any(|(known_id, _)| *known_id == id) {
                    panic!(">>> ERROR duplicate expected-buffer section for ID={id}");
                }
                strip_trailing_newline(&mut file);
                buffer_contents.push((id, file.content));
            }
        }

        let (ui, status_messages) = ScriptedUi::new(uname, actions);
        let setup = Setup {
            config_res,
            plumbing_rules_res,
            files,
            ui,
        };
        let assertions = Assertions {
            buffer_list,
            windows,
            buffer_contents,
            buffer_dots,
        };

        Self {
            setup,
            assertions,
            status_messages,
        }
    }
}

#[derive(Debug)]
struct Setup {
    config_res: Result<Config, String>,
    plumbing_rules_res: Result<PlumbingRules, String>,
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
    fn is_empty(&self) -> bool {
        self.buffer_list.is_none()
            && self.windows.is_empty()
            && self.buffer_contents.is_empty()
            && self.buffer_dots.is_empty()
    }

    fn verify(&self, e: &Editor<DefaultSystem>, test_dir: &Path) {
        if let Some(s) = self.buffer_list.as_ref() {
            println!(">> {:?}\n\n", e.buffer_list());
            let blist = e
                .buffer_list()
                .join("\n")
                .replace(&format!("{}/", test_dir.display()), "")
                .replace("/private", ""); // OSX-ism

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
    fn new(uname: String, actions: Vec<TestAction>) -> (Self, Arc<Mutex<Vec<String>>>) {
        let status_messages = Arc::new(Mutex::new(Vec::new()));
        let ui = ScriptedUi {
            uname,
            socket_path: PathBuf::new(),
            actions,
            pending_fsys: Arc::new(Mutex::new(false)),
            status_messages: status_messages.clone(),
            tx: None,
        };

        (ui, status_messages)
    }

    fn spawn_fsys(&self, f: Fsys) {
        match UnixClient::new_unix_with_explicit_path(&self.uname, &self.socket_path, "") {
            Ok(client) => {
                // We need to mark that we are pending before spawning the background thread
                // for running the fsys operation otherwise we race with the main editor
                // event loop and can fail to wait for the client to connect.
                let pending = self.pending_fsys.clone();
                *pending.lock().unwrap() = true;
                spawn(move || f.run(client, pending));
            }
            Err(e) => {
                // Not panicking here so we can let the rest of the test run to completion
                // and allow the cleanup logic to run.
                println!(">>> UNABLE TO CREATE FSYS CLIENT: {e}");
            }
        }
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
        _layout: &mut Layout,
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

impl Fsys {
    fn run(self, mut client: UnixClient, pending: Arc<Mutex<bool>>) {
        let inner = move || {
            match self {
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
    }
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
    /// type: <ctrl>$single_character
    /// type: <ctrl-alt>$single_character
    Input(Input),
    /// Sleep for a given number of milliseconds.
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

/// See [TestAction] for example valid input lines.
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
            // We need to provide a special syntax for control characters etc.
            // Rather than make things complicated and try to support marking sequences of characters
            // as being typed while control characters are held, we required that they are given one
            // per line. ("normal" typing can be given as a concatenation).
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

                s if s.starts_with("<ctrl>") => {
                    let tail = escape(s.strip_prefix("<ctrl>").unwrap().trim());
                    let mut it = tail.chars();
                    match (it.next(), it.next()) {
                        (Some(ch), None) => actions.push(TestAction::Input(Input::Ctrl(ch))),
                        (None, _) => panic!("invalid <ctrl> input: expected a character"),
                        (_, Some(_)) => {
                            panic!("invalid <ctrl> input: expected a single char, got {tail:?}");
                        }
                    }
                }

                s if s.starts_with("<ctrl-alt>") => {
                    let tail = escape(s.strip_prefix("<ctrl-alt>").unwrap().trim());
                    let mut it = tail.chars();
                    match (it.next(), it.next()) {
                        (Some(ch), None) => actions.push(TestAction::Input(Input::CtrlAlt(ch))),
                        (None, _) => panic!("invalid <ctrl-alt> input: expected a character"),
                        (_, Some(_)) => {
                            panic!(
                                "invalid <ctrl-alt> input: expected a single char, got {tail:?}"
                            );
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
