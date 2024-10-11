//! Utility functions
use crate::{editor::Action, input::Event};
use std::{
    env,
    ffi::OsStr,
    io::{self, Read, Write},
    iter::Peekable,
    path::{Component, Path, PathBuf},
    process::{Command, Stdio},
    str::Chars,
    sync::mpsc::Sender,
    thread::spawn,
};
use tracing::warn;

#[cfg(target_os = "linux")]
/// Set the current system clipboard state using xclip.
pub fn set_clipboard(s: &str) -> io::Result<()> {
    let mut child = Command::new("xclip")
        .args(["-selection", "clipboard", "-i"])
        .stdin(Stdio::piped())
        .spawn()?;

    child.stdin.take().unwrap().write_all(s.as_bytes())
}

#[cfg(target_os = "linux")]
/// Read the current system clipboard state using xclip.
pub fn read_clipboard() -> io::Result<String> {
    let output = Command::new("xclip")
        .args(["-selection", "clipboard", "-o"])
        .output()?;
    Ok(String::from_utf8(output.stdout).unwrap_or_default())
}

#[cfg(target_os = "macos")]
/// Set the current system clipboard state using pbcopy.
pub fn set_clipboard(s: &str) -> io::Result<()> {
    let mut child = Command::new("pbcopy").stdin(Stdio::piped()).spawn()?;
    child.stdin.take().unwrap().write_all(s.as_bytes())
}

#[cfg(target_os = "macos")]
/// Read the current system clipboard state using pbpaste.
pub fn read_clipboard() -> io::Result<String> {
    let output = Command::new("pbpaste").output()?;
    Ok(String::from_utf8(output.stdout).unwrap_or_default())
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

/// Run an external command and collect its output.
pub fn run_command_blocking<I, S>(
    cmd: &str,
    args: I,
    cwd: &Path,
    bufid: usize,
) -> io::Result<String>
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

/// Run an external command and append its output to the output buffer for `bufid` from a
/// background thread.
pub(crate) fn run_command<I, S>(cmd: &str, args: I, cwd: &Path, bufid: usize, tx: Sender<Event>)
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

/// Both the base path and p must be absolute paths and base must be a directory
pub(crate) fn relative_path_from(base: &Path, p: &Path) -> PathBuf {
    let mut base_comps = base.components();
    let mut path_comps = p.components();
    let mut comps = vec![];

    loop {
        match (base_comps.next(), path_comps.next()) {
            (None, None) => break,
            (None, Some(path_comp)) => {
                comps.push(path_comp);
                comps.extend(path_comps.by_ref());
                break;
            }
            (_, None) => comps.push(Component::ParentDir),
            (Some(a), Some(b)) if comps.is_empty() && a == b => (),
            (Some(Component::CurDir), Some(b)) => comps.push(b),
            (Some(_), Some(b)) => {
                comps.push(Component::ParentDir);
                for _ in base_comps {
                    comps.push(Component::ParentDir);
                }
                comps.push(b);
                comps.extend(path_comps.by_ref());
                break;
            }
        }
    }

    comps.iter().collect()
}

// returns the parsed number and following character if there was one.
// initial must be a valid ascii digit
pub(crate) fn parse_num(initial: char, it: &mut Peekable<Chars<'_>>) -> usize {
    let mut s = String::from(initial);
    loop {
        match it.peek() {
            Some(ch) if ch.is_ascii_digit() => {
                s.push(it.next().unwrap());
            }
            _ => return s.parse().unwrap(),
        }
    }
}

pub(crate) fn normalize_line_endings(mut s: String) -> String {
    if !s.contains('\r') {
        return s;
    }

    warn!("normalizing \\r characters to \\n");
    s = s.replace("\r\n", "\n");
    s.replace("\r", "\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test_case("/home/bob/", "/home/bob/ad/src/lib.rs", "ad/src/lib.rs"; "child directory")]
    #[test_case("/home/bob/ad/src", "/home/bob/ad/README.md", "../README.md"; "relative directory")]
    #[test_case("/home/bob/ad", "/usr/local/bin/penrose", "../../../usr/local/bin/penrose"; "only root in common")]
    #[test]
    fn rel_path_works(base: &str, p: &str, expected: &str) {
        let base = PathBuf::from(base);
        let p = PathBuf::from(p);
        let rel = relative_path_from(&base, &p);

        assert_eq!(rel, PathBuf::from(expected));
    }
}
