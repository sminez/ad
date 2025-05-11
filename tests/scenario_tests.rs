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
use simple_test_case::dir_cases;
use simple_txtar::{Archive, File};
use std::{env, fs, path::Path, sync::mpsc::Sender, time::SystemTime};

#[dir_cases("tests/data/editor-scenarios")]
#[test]
fn editor_scenarios(path: &str, content: &str) {
    // Parse the given test case file and validate it before initialising the editor
    let TestCase { setup, assertions } = TestCase::from_archive(content);
    if !assertions.is_valid() {
        panic!("no assertions provided");
    }
    let mut e = Editor::new_with_system(
        setup.config,
        setup.plumbing_rules,
        EditorMode::Boxed(Box::new(setup.ui)),
        LogBuffer::default(),
        DefaultSystem::without_clipboard_provider(),
    );

    // Create a new temp directory to hold our test files while the test runs
    let dir = env::temp_dir().join(format!(
        "ad-tests-{}-{}",
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs(),
        path.replace("tests/data/editor-scenarios/", "")
    ));

    fs::create_dir_all(&dir).expect("unable to create temp directory");
    println!("using {} for test files", dir.display());

    for f in setup.files.into_iter() {
        let p = dir.join(&f.name);
        if let Some(parent) = p.parent() {
            _ = fs::create_dir_all(parent);
        }
        if let Err(e) = fs::write(&p, f.content) {
            fs::remove_dir_all(dir).expect("unable to remove temp directory");
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

    e.run();

    fs::remove_dir_all(&dir).expect("unable to remove temp directory");

    assertions.verify(&e, &dir);
}

/// A parsed test case from a scenario file
#[derive(Debug)]
struct TestCase {
    setup: Setup,
    assertions: Assertions,
}

impl TestCase {
    fn from_archive(content: &str) -> Self {
        let arr = Archive::from(content);
        let home = env::var("HOME").unwrap();

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
        let mut buffer_contents = Vec::new();

        for mut file in arr.into_iter() {
            if file.name.starts_with("file-") {
                file.name = file.name.strip_prefix("file-").unwrap().to_string();
                if file.content.ends_with('\n') {
                    file.content.pop();
                }
                files.push(file);
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

        Self {
            setup: Setup {
                config,
                config_err,
                plumbing_rules,
                files,
                ui: ScriptedUi {
                    actions,
                    status_message: String::new(),
                    tx: None,
                },
            },
            assertions: Assertions {
                buffer_list,
                windows,
                buffer_contents,
            },
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
    // buffer_dot_contents: Vec<(BufferId, String)>
}

impl Assertions {
    /// Test case assertions are valid as long as there is at least one thing being asserted
    fn is_valid(&self) -> bool {
        self.buffer_list.is_some() || !self.windows.is_empty() || !self.buffer_contents.is_empty()
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
    }
}

#[derive(Debug)]
struct ScriptedUi {
    actions: Vec<TestAction>,
    status_message: String,
    tx: Option<Sender<Event>>,
}

impl UserInterface for ScriptedUi {
    fn init(&mut self, tx: Sender<Event>) -> (usize, usize) {
        self.tx = Some(tx);
        (60, 80)
    }

    fn shutdown(&mut self) {}

    fn state_change(&mut self, change: StateChange) {
        match change {
            StateChange::ConfigUpdated => (),
            StateChange::StatusMessage { msg } => {
                self.status_message = msg;
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
        let event = match self.actions.pop() {
            Some(TestAction::Input(input)) => Event::Input(input),
            None => Event::Action(Action::Exit { force: true }),
        };

        self.tx.as_ref().unwrap().send(event).unwrap();
    }

    fn set_cursor_shape(&mut self, _cur_shape: CurShape) {}
}

/// User input actions to send to the editor.
/// The whitespace after the colon following the action name is required.
///
///   - '# comments are ignored'
///   - 'type: ihello, world!'
#[derive(Debug)]
enum TestAction {
    Input(Input),
    // FsysMessage(???),   <- will require being able to specify the mount point for fsys
}

fn parse_actions(raw: &str) -> Vec<TestAction> {
    let mut actions = Vec::with_capacity(raw.lines().count());

    for line in raw.lines() {
        if line.starts_with('#') || line.is_empty() {
            continue;
        } else if let Some(s) = line.strip_prefix("type: ") {
            match s {
                "<esc>" => actions.push(TestAction::Input(Input::Esc)),

                s if s.starts_with("<alt>") => {
                    let tail = s.strip_prefix("<alt>").unwrap().trim();
                    let mut it = tail.chars();
                    match (it.next(), it.next()) {
                        (Some(ch), None) => actions.push(TestAction::Input(Input::Alt(ch))),
                        (None, _) => panic!("invalid <alt> input: expected a character"),
                        (_, Some(_)) => {
                            panic!("invalid <alt> input: expected a single char, got {tail}");
                        }
                    }
                }

                _ => {
                    let s = s
                        .replace("\\n", "\n")
                        .replace("\\t", "\t")
                        .replace("\\\\", "\\");
                    for ch in s.chars() {
                        actions.push(TestAction::Input(Input::Char(ch)));
                    }
                }
            }
        } else {
            panic!("malformed action line: {line:?}");
        }
    }

    actions.reverse(); // so we can pop from the end while running
    actions
}
