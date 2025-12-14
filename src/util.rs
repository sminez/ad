//! Utility functions
use crate::{config::config_path, editor::built_in_commands, mode::keybindings};
use std::{
    fs,
    os::unix::fs::PermissionsExt,
    path::Path,
    sync::{Arc, LockResult, RwLock, RwLockReadGuard},
};
use tracing::warn;

/// A wrapper around an `Arc<RwLock<T>>` so that the owner is only
/// permitted read access to the underlying value.
#[derive(Debug, Default, Clone)]
pub struct ReadOnlyLock<T>(Arc<RwLock<T>>);

impl<T> ReadOnlyLock<T> {
    /// Construct a new ReadOnlyLock wrapping an inner `Arc<RwLock<T>>`
    pub fn new(inner: Arc<RwLock<T>>) -> Self {
        Self(inner)
    }

    /// Obtain a read guard from the underlying `RwLock`
    pub fn read(&self) -> LockResult<RwLockReadGuard<'_, T>> {
        self.0.read()
    }
}

/// Pull in data from the ad crate itself to auto-generate the docs on the functionality
/// available in the editor.
pub(crate) fn gen_help_docs() -> String {
    let help_template = include_str!("../data/help-template.txt");

    help_template
        .replace("{{KEY_BINDINGS}}", &keybindings_section())
        .replace("{{BUILT_IN_COMMANDS}}", &commands_section())
        .replace("{{CONFIG_PATH}}", &config_path())
}

fn keybindings_section() -> String {
    let raw = keybindings();
    let mut sections = Vec::with_capacity(raw.len());

    for (mode, bindings) in raw.into_iter() {
        let w_max = bindings.iter().map(|(s, _)| s.len()).max().unwrap();
        let mut section = format!("{mode} mode\n");

        for (keys, desc) in bindings.into_iter() {
            section.push_str(&format!("  {:width$} -- {desc}\n", keys, width = w_max));
        }

        sections.push(section);
    }

    sections.join("\n\n")
}

fn commands_section() -> String {
    let commands = built_in_commands();
    let mut buf = Vec::with_capacity(commands.len());

    for (cmds, desc) in commands.into_iter() {
        buf.push((cmds.join(" | "), desc));
    }

    let w_max = buf.iter().map(|(s, _)| s.len()).max().unwrap();
    let mut s = String::new();

    for (cmds, desc) in buf.into_iter() {
        s.push_str(&format!("{:width$} -- {desc}\n", cmds, width = w_max));
    }

    s
}

pub(crate) fn normalize_line_endings(mut s: String) -> String {
    if !s.contains('\r') {
        return s;
    }

    warn!("normalizing \\r characters to \\n");
    s = s.replace("\r\n", "\n");
    s.replace("\r", "\n")
}

/// Locate the first parent directory containing a target file
pub(crate) fn parent_dir_containing<'a>(initial: &'a Path, target: &str) -> Option<&'a Path> {
    initial
        .ancestors()
        .find(|&p| p.is_dir() && p.join(target).exists())
}

/// Check whether or not a given command can be found as an executable within the provided set of path directories
#[allow(dead_code)]
pub(crate) fn exists_on_path_as_executable(cmd: &str, cwd: &Path, path_str: &str) -> bool {
    let cwd_candidate = cwd.join(cmd);
    let candidates = path_str.split(':').map(|dir| Path::new(dir).join(cmd));

    for candidate in std::iter::once(cwd_candidate).chain(candidates) {
        if let Ok(meta) = fs::metadata(candidate)
            && meta.is_file()
            && meta.permissions().mode() & 0o111 != 0
        {
            return true;
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;
    use std::{env, path::PathBuf};

    #[test_case("cat", true; "cat exists")]
    #[test_case("dog", false; "dog does not exist")]
    #[test]
    fn executable_checking_works(cmd: &str, expected: bool) {
        let path = env::var("PATH").unwrap();
        let exists = exists_on_path_as_executable(cmd, &PathBuf::from("/tmp"), &path);

        assert_eq!(exists, expected);
    }
}
