use std::{fs::read_to_string, io};

#[derive(Default)]
pub struct Buffer {
    pub(crate) path: Option<String>,
    pub(crate) lines: Vec<String>,
}

impl Buffer {
    pub fn new_from_file(path: &str) -> io::Result<Self> {
        let raw = read_to_string(path)?;
        let lines: Vec<String> = raw.lines().map(String::from).collect();

        Ok(Self {
            path: Some(path.to_string()),
            lines,
        })
    }

    pub fn len(&self) -> usize {
        self.lines.len()
    }

    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }
}
