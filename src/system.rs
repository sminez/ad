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

/// A default implementation for system interactions
#[derive(Debug, Clone, Copy)]
pub struct DefaultSystem;

#[cfg(target_os = "linux")]
impl System for DefaultSystem {
    fn set_clipboard(&mut self, s: &str) -> io::Result<()> {
        let mut child = Command::new("xclip")
            .args(["-selection", "clipboard", "-i"])
            .stdin(Stdio::piped())
            .spawn()?;

        child.stdin.take().unwrap().write_all(s.as_bytes())
    }

    fn read_clipboard(&self) -> io::Result<String> {
        let output = Command::new("xclip")
            .args(["-selection", "clipboard", "-o"])
            .output()?;

        Ok(String::from_utf8(output.stdout).unwrap_or_default())
    }
}

#[cfg(target_os = "macos")]
impl System for DefaultSystem {
    fn set_clipboard(&mut self, s: &str) -> io::Result<()> {
        let mut child = Command::new("pbcopy").stdin(Stdio::piped()).spawn()?;

        child.stdin.take().unwrap().write_all(s.as_bytes())
    }

    fn read_clipboard(&self) -> io::Result<String> {
        let output = Command::new("pbpaste").output()?;

        Ok(String::from_utf8(output.stdout).unwrap_or_default())
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
