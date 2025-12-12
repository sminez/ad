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
use arbitrary::Arbitrary;
use assert_fs::{
    TempDir,
    prelude::{PathChild, PathCreateDir},
};
use libfuzzer_sys::fuzz_target;
use std::{
    fs, io,
    path::{Path, PathBuf},
    process::Child,
    sync::mpsc::Sender,
};

fuzz_target!(|data: FuzzData| {
    let tmp = TempDir::new().unwrap();
    let test_file_dir = tmp.child("files");
    test_file_dir.create_dir_all().unwrap();

    let file_paths: Vec<PathBuf> = data
        .files
        .into_iter()
        .enumerate()
        .flat_map(|(i, f)| {
            let p = test_file_dir.join(i.to_string());

            // If the fuzzer gives us a path that we can't use then we skip including this file
            // rather than erroring out at this stage. We can still run the generated inputs
            // against the default empty buffer in the case that all generated names are invalid.
            match fs::write(&p, f.content) {
                Ok(_) => Some(p),
                Err(_) => None,
            }
        })
        .collect();

    let mut config = Config::default();
    config.filesystem.enabled = false;

    // Attempt to force the editor out of any transient modes its in that would prevent it from
    // running the exit action at the end of the action sequence.
    let mut actions = data.actions;
    actions.push(TestAction::Input(Input::Esc));

    let mut e = Editor::new_with_system_and_initial_files(
        Ok(config),
        Ok(PlumbingRules::default()),
        EditorMode::Boxed(Box::new(ScriptedUi::new(actions))),
        LogBuffer::default(),
        FuzzSystem::default(),
        &file_paths,
    );

    e.run();
});

#[derive(Debug, Arbitrary)]
struct FuzzData {
    files: Vec<File>,
    actions: Vec<TestAction>,
}

#[derive(Debug, Arbitrary)]
struct File {
    // name: PathBuf,
    content: String,
}

#[derive(Debug, Arbitrary)]
enum TestAction {
    Input(Input),
}

#[derive(Debug)]
struct ScriptedUi {
    actions: Vec<TestAction>,
    tx: Option<Sender<Event>>,
}

impl ScriptedUi {
    fn new(actions: Vec<TestAction>) -> Self {
        ScriptedUi { actions, tx: None }
    }
}

impl UserInterface for ScriptedUi {
    fn init(&mut self, tx: Sender<Event>) -> (usize, usize) {
        self.tx = Some(tx);

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
        let event = match self.actions.pop() {
            Some(TestAction::Input(input)) => Event::Input(input),
            None => Event::Action(Action::Exit { force: true }),
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
