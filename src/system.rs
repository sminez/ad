//! An abstraction around system interactions to support testing and
//! platform specific behaviour
use crate::{editor::Action, input::Event, util::normalize_line_endings};
use std::{
    env, fmt,
    io::{self, BufRead, BufReader, Read, Write},
    path::Path,
    process::{Child, Command, Stdio},
    sync::mpsc::Sender,
    thread::spawn,
};
use tracing::info;

/// Wrapper around storing system interactions
pub trait System: fmt::Debug {
    /// Set the clipboard to the given string
    fn set_clipboard(&mut self, s: &str) -> io::Result<()>;

    /// Read the current contents of the clipboard
    fn read_clipboard(&self) -> io::Result<String>;

    /// Store a handle to a running [Child] followinga  call to [System::run_command].
    fn store_child_handle(&mut self, cmd: &str, child: Child);

    /// Provide an ordered list of currently running child processes by their command string
    fn running_children(&self) -> Vec<String>;

    /// The number of currently running child processes
    fn n_running_children(&self) -> usize {
        self.running_children().len()
    }

    /// Cleanup any resources associated with a child process that is now complete
    fn cleanup_child(&mut self, id: u32);

    /// Kill a child process by its index in the list returned from [System::running_children].
    fn kill_child(&mut self, idx: usize);

    /// Run an external command and collect its output.
    fn run_command_blocking(&self, cmd: &str, cwd: &Path, bufid: usize) -> io::Result<String> {
        run_command_blocking(cmd, cwd, bufid)
    }

    /// Run an external command and append its output to the output buffer for `bufid` from a
    /// background thread. If the command is successfully spawned then a [Child] should be stored
    /// for later resource cleanup and support for user initiated killing.
    fn run_command(
        &mut self,
        cmd: &str,
        cwd: &Path,
        bufid: usize,
        tx: Sender<Event>,
    ) -> io::Result<()> {
        let child = run_command(cmd, cwd, bufid, tx)?;
        self.store_child_handle(cmd, child);

        Ok(())
    }

    /// Pipe input text through an external command, returning the output
    fn pipe_through_command(
        &self,
        cmd: &str,
        input: &str,
        cwd: &Path,
        bufid: usize,
    ) -> io::Result<String> {
        pipe_through_command(cmd, input, cwd, bufid)
    }
}

#[derive(Debug, Clone)]
struct ClipboardProvider {
    copy_cmd: &'static str,
    copy_args: Vec<&'static str>,
    paste_cmd: &'static str,
    paste_args: Vec<&'static str>,
}

impl ClipboardProvider {
    pub fn try_from_env() -> Option<Self> {
        let paths = env::var("PATH").expect("path not set");
        let exists = |cmd: &str| env::split_paths(&paths).any(|dir| dir.join(cmd).is_file());

        let (copy_cmd, copy_args, paste_cmd, paste_args) = if exists("pbcopy") {
            info!("clipboard provider found: pbcopy");
            ("pbcopy", vec![], "pbpaste", vec![])
        } else if env::var("WAYLAND_DISPLAY").is_ok() && exists("wl-copy") && exists("wl-paste") {
            info!("clipboard provider found: wl-copy");
            (
                "wl-copy",
                vec!["--foreground", "--type", "text/plain"],
                "wl-paste",
                vec!["--no-newline"],
            )
        } else if env::var("DISPLAY").is_ok() && exists("xclip") {
            info!("clipboard provider found: xclip");
            (
                "xclip",
                vec!["-i", "-selection", "clipboard"],
                "xclip",
                vec!["-o", "-selection", "clipboard"],
            )
        } else {
            info!("no clipboard provider found");
            return None;
        };

        Some(Self {
            copy_cmd,
            copy_args,
            paste_cmd,
            paste_args,
        })
    }
}

/// A default implementation for system interactions
#[derive(Debug)]
pub struct DefaultSystem {
    selection: String,
    cp: Option<ClipboardProvider>,
    running_children: Vec<(String, Child)>,
}

impl DefaultSystem {
    pub fn from_env() -> Self {
        Self {
            selection: String::new(),
            cp: ClipboardProvider::try_from_env(),
            running_children: Vec::new(),
        }
    }

    pub fn without_clipboard_provider() -> Self {
        Self {
            selection: String::new(),
            cp: None,
            running_children: Vec::new(),
        }
    }
}

impl System for DefaultSystem {
    fn set_clipboard(&mut self, s: &str) -> io::Result<()> {
        match &self.cp {
            Some(cp) => {
                let mut child = Command::new(cp.copy_cmd)
                    .args(&cp.copy_args)
                    .stdin(Stdio::piped())
                    .spawn()?;

                child.stdin.take().unwrap().write_all(s.as_bytes())
            }

            None => {
                self.selection = s.to_string();
                Ok(())
            }
        }
    }

    fn read_clipboard(&self) -> io::Result<String> {
        match &self.cp {
            Some(cp) => {
                let output = Command::new(cp.paste_cmd).args(&cp.paste_args).output()?;

                Ok(String::from_utf8(output.stdout).unwrap_or_default())
            }

            None => Ok(self.selection.clone()),
        }
    }

    fn store_child_handle(&mut self, cmd: &str, child: Child) {
        self.running_children.push((cmd.to_owned(), child));
    }

    fn running_children(&self) -> Vec<String> {
        self.running_children
            .iter()
            .map(|(cmd, _)| cmd.clone())
            .collect()
    }

    fn n_running_children(&self) -> usize {
        self.running_children.len()
    }

    fn cleanup_child(&mut self, id: u32) {
        for (_, child) in self.running_children.iter_mut() {
            if child.id() == id {
                _ = child.wait();
            }
        }

        self.running_children.retain(|(_, child)| child.id() != id);
    }

    fn kill_child(&mut self, idx: usize) {
        let (_, mut child) = self.running_children.remove(idx);
        _ = child.kill();
        _ = child.wait();
    }
}

fn prepare_command(cmd: &str, cwd: &Path, bufid: usize) -> Command {
    let mut args: Vec<&str> = cmd.split_whitespace().collect();
    if args.is_empty() {
        return Command::new("");
    }

    let cmd = args.remove(0);
    let path = env::var("PATH").unwrap();
    let home = env::var("HOME").unwrap();
    let mut command = Command::new(cmd);
    command
        .env("PATH", format!("{home}/.ad/bin:{path}"))
        .env("AD_PID", crate::pid().to_string())
        .env("AD_BUFID", bufid.to_string())
        .current_dir(cwd)
        .args(args);

    command
}

fn run_command_blocking(cmd: &str, cwd: &Path, bufid: usize) -> io::Result<String> {
    let output = prepare_command(cmd, cwd, bufid).output()?;
    let mut stdout = String::from_utf8(output.stdout).unwrap_or_default();
    let stderr = String::from_utf8(output.stderr).unwrap_or_default();
    stdout.push_str(&stderr);

    Ok(normalize_line_endings(stdout))
}

fn run_command(cmd: &str, cwd: &Path, bufid: usize, tx: Sender<Event>) -> io::Result<Child> {
    let mut child = prepare_command(cmd, cwd, bufid)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    let stdout = BufReader::new(child.stdout.take().unwrap());
    let stderr = BufReader::new(child.stderr.take().unwrap());
    let id = child.id();

    spawn(move || {
        let tx2 = tx.clone();
        spawn(move || send_lines(bufid, stderr.lines(), tx2));
        send_lines(bufid, stdout.lines(), tx.clone());
        _ = tx.send(Event::Action(Action::CleanupChild { id }));
    });

    Ok(child)
}

fn send_lines(bufid: usize, it: impl Iterator<Item = io::Result<String>>, tx: Sender<Event>) {
    for res in it {
        match res {
            Ok(mut line) => {
                line.push('\n');
                _ = tx.send(Event::Action(Action::AppendToOutputBuffer {
                    bufid,
                    content: normalize_line_endings(line),
                }));
            }
            Err(_) => break,
        }
    }
}

/// Pipe input text through an external command, returning the output
pub fn pipe_through_command(
    cmd: &str,
    input: &str,
    cwd: &Path,
    bufid: usize,
) -> io::Result<String> {
    let mut child = prepare_command(cmd, cwd, bufid)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    let mut buf = String::new();
    child.stdin.take().unwrap().write_all(input.as_bytes())?;
    child.stdout.take().unwrap().read_to_string(&mut buf)?;
    child.stderr.take().unwrap().read_to_string(&mut buf)?;
    _ = child.wait();

    Ok(normalize_line_endings(buf))
}
