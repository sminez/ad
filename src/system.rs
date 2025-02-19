//! An abstraction around system interactions to support testing and
//! platform specific behaviour
use crate::{editor::Action, input::Event, util::normalize_line_endings};
use std::{
    env,
    ffi::OsStr,
    fmt,
    io::{self, Read, Write},
    path::Path,
    process::{Command, Stdio},
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

    /// Run an external command and collect its output.
    fn run_command_blocking<I, S>(
        &self,
        cmd: &str,
        args: I,
        cwd: &Path,
        bufid: usize,
    ) -> io::Result<String>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        run_command_blocking(cmd, args, cwd, bufid)
    }

    /// Run an external command and append its output to the output buffer for `bufid` from a
    /// background thread.
    fn run_command<I, S>(&self, cmd: &str, args: I, cwd: &Path, bufid: usize, tx: Sender<Event>)
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        run_command(cmd, args, cwd, bufid, tx)
    }

    /// Pipe input text through an external command, returning the output
    fn pipe_through_command<I, S>(
        &self,
        cmd: &str,
        args: I,
        input: &str,
        cwd: &Path,
        bufid: usize,
    ) -> io::Result<String>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        pipe_through_command(cmd, args, input, cwd, bufid)
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
#[derive(Debug, Clone)]
pub struct DefaultSystem {
    selection: String,
    cp: Option<ClipboardProvider>,
}

impl DefaultSystem {
    pub fn from_env() -> Self {
        Self {
            selection: String::new(),
            cp: ClipboardProvider::try_from_env(),
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
}

fn prepare_command<I, S>(cmd: &str, args: I, cwd: &Path, bufid: usize) -> Command
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let path = env::var("PATH").unwrap();
    let home = env::var("HOME").unwrap();
    let mut command = Command::new(cmd);
    command
        .env("PATH", format!("{home}/.ad/bin:{path}"))
        .env("bufid", bufid.to_string())
        .current_dir(cwd)
        .args(args);

    command
}

fn run_command_blocking<I, S>(cmd: &str, args: I, cwd: &Path, bufid: usize) -> io::Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = prepare_command(cmd, args, cwd, bufid).output()?;
    let mut stdout = String::from_utf8(output.stdout).unwrap_or_default();
    let stderr = String::from_utf8(output.stderr).unwrap_or_default();
    stdout.push_str(&stderr);

    Ok(normalize_line_endings(stdout))
}

fn run_command<I, S>(cmd: &str, args: I, cwd: &Path, bufid: usize, tx: Sender<Event>)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut command = prepare_command(cmd, args, cwd, bufid);

    spawn(move || {
        let output = match command.output() {
            Ok(output) => output,
            Err(err) => {
                _ = tx.send(Event::Action(Action::SetStatusMessage {
                    message: err.to_string(),
                }));
                return;
            }
        };

        let mut content = String::from_utf8(output.stdout).unwrap_or_default();
        let stderr = String::from_utf8(output.stderr).unwrap_or_default();
        content.push_str(&stderr);
        if content.is_empty() {
            return;
        }
        _ = tx.send(Event::Action(Action::AppendToOutputBuffer {
            bufid,
            content: normalize_line_endings(content),
        }));
    });
}

/// Pipe input text through an external command, returning the output
pub fn pipe_through_command<I, S>(
    cmd: &str,
    args: I,
    input: &str,
    cwd: &Path,
    bufid: usize,
) -> io::Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut child = prepare_command(cmd, args, cwd, bufid)
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
