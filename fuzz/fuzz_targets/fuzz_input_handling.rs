#![no_main]

use ad_editor::{
    Config, Editor, EditorMode, LogBuffer, PlumbingRules,
    editor::{Action, Click, MiniBufferState},
    input::Event,
    key::Input,
    system::System,
    term::CurShape,
    ui::{Layout, StateChange, UserInterface},
};
use assert_fs::TempDir;
use libfuzzer_sys::fuzz_target;
use ninep::sync::client::UnixClient;
use std::{
    env, fs, io,
    path::{Path, PathBuf},
    process::Child,
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
const UNAME: &str = env!("USER");

fuzz_target!(|data: FuzzData| {
    let tmp = TempDir::new().unwrap();
    let test_file_dir = tmp.join("files");
    let socket_path = tmp.join("sock");

    fs::create_dir_all(&test_file_dir).expect("unable to create temp directory");

    let file_paths: Vec<PathBuf> = data
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
        Ok(Config::default()),
        Ok(PlumbingRules::default()),
        EditorMode::Boxed(Box::new(ScriptedUi::new(data.actions, socket_path.clone()))),
        LogBuffer::default(),
        FuzzSystem::default(),
        &file_paths,
    );

    e.run_with_explicit_fsys_path(socket_path);
});

#[derive(Debug, arbitrary::Arbitrary)]
struct FuzzData {
    files: Vec<File>,
    actions: Vec<TestAction>,
}

#[derive(Debug, arbitrary::Arbitrary)]
struct File {
    name: String,
    content: String,
}

#[derive(Debug, arbitrary::Arbitrary)]
enum TestAction {
    Input(Input),
    Fsys(Fsys),
}

#[derive(Debug, arbitrary::Arbitrary)]
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

#[derive(Debug)]
struct ScriptedUi {
    socket_path: PathBuf,
    actions: Vec<TestAction>,
    pending_fsys: Arc<Mutex<bool>>,
    tx: Option<Sender<Event>>,
}

impl ScriptedUi {
    fn new(actions: Vec<TestAction>, socket_path: PathBuf) -> Self {
        ScriptedUi {
            socket_path,
            actions,
            pending_fsys: Arc::new(Mutex::new(false)),
            tx: None,
        }
    }

    fn spawn_fsys(&self, f: Fsys) {
        match UnixClient::new_unix_with_explicit_path(UNAME, &self.socket_path, "") {
            Ok(client) => {
                // We need to mark that we are pending before spawning the background thread
                // for running the fsys operation otherwise we race with the main editor
                // event loop and can fail to wait for the client to connect.
                let pending = self.pending_fsys.clone();
                *pending.lock().unwrap() = true;
                spawn(move || f.run(client, pending));
            }
            Err(e) => {
                panic!("Unable to create fsys client: {e}");
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

                sleep(Duration::from_millis(FSYS_SLEEP_MS));
            }
        }

        (60, 80)
    }

    fn shutdown(&mut self) {}

    fn state_change(&mut self, _change: StateChange) {}

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

                Some(TestAction::Input(input)) => Event::Input(input),

                None => Event::Action(Action::Exit { force: true }),
            }
        };

        self.tx.as_ref().unwrap().send(event).unwrap();
    }

    fn set_cursor_shape(&mut self, _cur_shape: CurShape) {}
}

/// A System impl that can't run commands (we don't want the fuzzer to accidentally trash the host)
#[derive(Debug, Default)]
pub struct FuzzSystem {
    selection: String,
}

impl System for FuzzSystem {
    fn set_clipboard(&mut self, s: &str) -> io::Result<()> {
        self.selection = s.to_string();

        Ok(())
    }

    fn read_clipboard(&self) -> io::Result<String> {
        Ok(self.selection.clone())
    }

    fn store_child_handle(&mut self, _cmd: &str, _child: Child) {}
    fn cleanup_child(&mut self, _id: u32) {}
    fn kill_child(&mut self, _idx: usize) {}

    fn running_children(&self) -> Vec<String> {
        Vec::new()
    }

    fn n_running_children(&self) -> usize {
        0
    }

    fn run_command(
        &mut self,
        _cmd: &str,
        _cwd: &Path,
        _bufid: usize,
        _tx: Sender<Event>,
    ) -> io::Result<()> {
        Err(io::Error::other("not implemented"))
    }

    fn pipe_through_command(
        &self,
        _cmd: &str,
        _input: &str,
        _cwd: &Path,
        _bufid: usize,
    ) -> io::Result<String> {
        Err(io::Error::other("not implemented"))
    }

    fn run_command_blocking(&self, _cmd: &str, _cwd: &Path, _bufid: usize) -> io::Result<String> {
        Err(io::Error::other("not implemented"))
    }
}
