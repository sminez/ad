use crate::system::{DefaultSystem, System};
use std::{io, path::PathBuf};

pub trait Runner {
    fn run_shell_command(&mut self, cmd: &str, input: Option<&str>) -> io::Result<String>;
}

pub struct EditorRunner<'a, S>
where
    S: System,
{
    pub(crate) system: &'a mut S,
    pub(crate) dir: PathBuf,
    pub(crate) bufid: usize,
}

impl<'a, S> Runner for EditorRunner<'a, S>
where
    S: System,
{
    fn run_shell_command(&mut self, cmd: &str, input: Option<&str>) -> io::Result<String> {
        match input {
            Some(input) => self
                .system
                .pipe_through_command(cmd, input, &self.dir, self.bufid),
            None => self.system.run_command_blocking(cmd, &self.dir, self.bufid),
        }
    }
}

#[derive(Debug)]
pub struct SystemRunner {
    system: DefaultSystem,
    dir: PathBuf,
}

impl SystemRunner {
    pub fn new(dir: PathBuf) -> Self {
        Self {
            system: DefaultSystem::without_clipboard_provider(),
            dir,
        }
    }

    pub fn set_dir(&mut self, dir: impl Into<PathBuf>) {
        self.dir = dir.into();
    }
}

impl Runner for SystemRunner {
    fn run_shell_command(&mut self, cmd: &str, input: Option<&str>) -> io::Result<String> {
        match input {
            Some(input) => self.system.pipe_through_command(cmd, input, &self.dir, 0),
            None => self.system.run_command_blocking(cmd, &self.dir, 0),
        }
    }
}
