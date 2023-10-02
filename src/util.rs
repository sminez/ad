use std::{
    io::{self, Write},
    path::{Component, Path, PathBuf},
    process::{Command, Stdio},
};

#[cfg(target_os = "linux")]
pub fn set_clipboard(s: &str) -> io::Result<()> {
    let mut child = Command::new("xclip")
        .args(["-selection", "clipboard", "-i"])
        .stdin(Stdio::piped())
        .spawn()?;

    child.stdin.take().unwrap().write_all(s.as_bytes())
}

#[cfg(target_os = "linux")]
pub fn read_clipboard() -> io::Result<String> {
    let output = Command::new("xclip")
        .args(["-selection", "clipboard", "-o"])
        .output()?;
    Ok(String::from_utf8(output.stdout).unwrap_or_default())
}

#[cfg(target_os = "macos")]
pub fn set_clipboard(s: &str) -> io::Result<()> {
    let mut child = Command::new("pbcopy").stdin(Stdio::piped()).spawn()?;
    child.stdin.take().unwrap().write_all(s.as_bytes())
}

#[cfg(target_os = "macos")]
pub fn read_clipboard() -> io::Result<String> {
    let output = Command::new("pbpaste").output()?;
    Ok(String::from_utf8(output.stdout).unwrap_or_default())
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
            (Some(a), Some(b)) if a == Component::CurDir => comps.push(b),
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
