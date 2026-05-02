//! Helpers for writing the shared integration test suite
use crate::MiniBufferSelection;
use ad_editor::{
    Config, Editor, EditorMode, LogBuffer, PlumbingRules, input::Event, system::DefaultSystem,
};
use assert_fs::TempDir;
use std::{
    fs,
    path::PathBuf,
    sync::mpsc::Sender,
    thread::{JoinHandle, sleep, spawn},
    time::Duration,
};

pub fn mbs_line(index: usize, content: &str) -> MiniBufferSelection {
    MiniBufferSelection::Line {
        index,
        content: content.into(),
    }
}

pub fn mbs_user(content: &str) -> MiniBufferSelection {
    MiniBufferSelection::UserInput {
        content: content.into(),
    }
}

pub fn mbs_cancelled() -> MiniBufferSelection {
    MiniBufferSelection::Cancelled
}

/// A handle to a running [Editor] using a [TempDir] as scratch space for the files it is working
/// with.
#[derive(Debug)]
pub struct TestEditor {
    pub tx: Sender<Event>,
    pub tmp: TempDir,
    pub _handle: JoinHandle<Editor<DefaultSystem>>,
}

impl TestEditor {
    pub fn socket_path(&self) -> PathBuf {
        self.tmp.join("sock")
    }

    pub fn write_file(&self, name: &str, content: &str) -> String {
        let p = self.tmp.join("files").join(name);
        if let Err(e) = fs::write(&p, content) {
            panic!("failed to write test file {name}: {e}");
        }

        p.to_string_lossy().to_string()
    }

    /// Run a new [Editor] in a dedicated thread with the given initial files.
    pub fn prepare(files: &[(&str, &str)]) -> TestEditor {
        let tmp = TempDir::new().unwrap();
        let test_file_dir = tmp.join("files");
        let socket_path = tmp.join("sock");

        fs::create_dir_all(&test_file_dir).expect("unable to create temp directory");
        println!("using temp directory: {}", tmp.path().display());
        println!("using {} for the fsys socket", socket_path.display());
        println!("using {} for test files", test_file_dir.display());

        // Write out all of our test files into the new temp directory and store the paths
        // so they can be passed to the editor.
        let file_paths: Vec<PathBuf> = files
            .iter()
            .map(|(name, content)| {
                let p = test_file_dir.join(name);
                if let Some(parent) = p.parent() {
                    _ = fs::create_dir_all(parent);
                }
                if let Err(e) = fs::write(&p, content) {
                    panic!("failed to write test file {name}: {e}");
                }

                p
            })
            .collect();

        let mut e = Editor::new_with_system_and_initial_files(
            Ok(Config::default()),
            Ok(PlumbingRules::default()),
            EditorMode::Headless,
            LogBuffer::default(),
            DefaultSystem::without_clipboard_provider(),
            &file_paths,
        );

        let spath = socket_path.clone();
        let tx = e.tx_events();
        let _handle = spawn(|| {
            e.run_with_explicit_fsys_path(spath);
            e
        });

        // Give the editor time to create the fsys socket
        sleep(Duration::from_millis(5));

        TestEditor { tx, tmp, _handle }
    }
}
