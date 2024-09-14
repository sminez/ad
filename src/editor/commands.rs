//! Command mode commands for ad
use crate::editor::{
    Action::*,
    Actions::{self, *},
    Editor, ViewPort,
};

impl Editor {
    pub(super) fn parse_command(&mut self, input: &str) -> Option<Actions> {
        if let Some(actions) = try_parse_single_char_command(input) {
            return Some(actions);
        }

        let input = input.trim_end();
        let (command, args) = if input.contains(' ') {
            input.split_once(' ')?
        } else {
            (input, "")
        };

        match command {
            "b" | "buffer" => match args.parse::<usize>() {
                Ok(id) => Some(Single(FocusBuffer { id })),
                Err(_) => {
                    self.set_status_message(&format!("'{args}' is not a valid buffer id"));
                    None
                }
            },
            "bn" | "buffer-next" => Some(Single(NextBuffer)),
            "bp" | "buffer-previous" => Some(Single(PreviousBuffer)),

            "cd" | "change-directory" => {
                if args.is_empty() {
                    Some(Single(ChangeDirectory { path: None }))
                } else {
                    Some(Single(ChangeDirectory {
                        path: Some(args.to_string()),
                    }))
                }
            }

            "db" | "delete-buffer" => Some(Single(DeleteBuffer { force: false })),
            "db!" | "delete-buffer!" => Some(Single(DeleteBuffer { force: true })),

            "echo" => {
                self.set_status_message(args);
                None
            }

            "E" | "Edit" => {
                if args.is_empty() {
                    self.set_status_message("No edit script provided");
                    None
                } else {
                    Some(Single(EditCommand {
                        cmd: args.to_string(),
                    }))
                }
            }

            "o" | "open" => {
                if args.is_empty() {
                    self.set_status_message("No filename provided");
                    None
                } else {
                    Some(Single(OpenFile {
                        path: args.to_string(),
                    }))
                }
            }

            "pwd" => {
                self.set_status_message(&self.cwd.display().to_string());
                None
            }

            "q" | "quit" => Some(Single(Exit { force: false })),
            "q!" | "quit!" => Some(Single(Exit { force: true })),

            "r" | "reload" => Some(Single(ReloadActiveBuffer)),
            "R" | "reload-config" => Some(Single(ReloadConfig)),
            "reload-buffer" => match args.parse::<usize>() {
                Ok(id) => Some(Single(ReloadBuffer { id })),
                Err(_) => {
                    self.set_status_message(&format!("'{args}' is not a valid buffer id"));
                    None
                }
            },

            "set" => Some(Single(SetConfigProp {
                input: args.to_string(),
            })),

            "vl" | "view-logs" => Some(Single(ViewLogs)),

            "w" | "write" => {
                if args.is_empty() {
                    Some(Single(SaveBuffer { force: false }))
                } else {
                    Some(Single(SaveBufferAs {
                        path: args.to_string(),
                        force: false,
                    }))
                }
            }
            "w!" | "write!" => {
                if args.is_empty() {
                    Some(Single(SaveBuffer { force: true }))
                } else {
                    Some(Single(SaveBufferAs {
                        path: args.to_string(),
                        force: true,
                    }))
                }
            }

            "wq" | "write-quit" => Some(Multi(vec![
                SaveBuffer { force: false },
                Exit { force: false },
            ])),
            "wq!" | "write-quit!" => Some(Multi(vec![
                SaveBuffer { force: true },
                Exit { force: true },
            ])),

            "zb" | "viewport-bottom" => Some(Single(SetViewPort(ViewPort::Bottom))),
            "zt" | "viewport-top" => Some(Single(SetViewPort(ViewPort::Top))),
            "zz" | "viewport-center" => Some(Single(SetViewPort(ViewPort::Center))),

            "" => None,

            _ => {
                self.set_status_message(&format!("Not an editor command: {command}"));
                None
            }
        }
    }
}

fn try_parse_single_char_command(input: &str) -> Option<Actions> {
    match input.chars().next() {
        Some('!') => Some(Single(ShellRun {
            cmd: input[1..].to_string(),
        })),
        Some('|') => Some(Single(ShellPipe {
            cmd: input[1..].to_string(),
        })),
        Some('<') => Some(Single(ShellReplace {
            cmd: input[1..].to_string(),
        })),
        Some('>') => Some(Single(ShellSend {
            cmd: input[1..].to_string(),
        })),

        _ => None,
    }
}
