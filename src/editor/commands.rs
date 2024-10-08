//! Command mode commands for ad
use crate::editor::{
    Action::*,
    Actions::{self, *},
    Editor, ViewPort,
};
use std::path::Path;

fn parse_command(input: &str, active_buffer_id: usize, cwd: &Path) -> Result<Actions, String> {
    if let Some(actions) = try_parse_single_char_command(input) {
        return Ok(actions);
    }

    let input = input.trim_end();
    let (command, args) = match input.split_once(' ') {
        Some((command, args)) => (command, args),
        None => (input, ""),
    };

    match command {
        "b" | "buffer" => match args.parse::<usize>() {
            Ok(id) => Ok(Single(FocusBuffer { id })),
            Err(_) => Err(format!("'{args}' is not a valid buffer id")),
        },
        "bn" | "buffer-next" => Ok(Single(NextBuffer)),
        "bp" | "buffer-prev" => Ok(Single(PreviousBuffer)),

        "cd" | "change-directory" => {
            if args.is_empty() {
                Ok(Single(ChangeDirectory { path: None }))
            } else {
                Ok(Single(ChangeDirectory {
                    path: Some(args.to_string()),
                }))
            }
        }

        "mark-clean" => {
            let bufid = if args.is_empty() {
                active_buffer_id
            } else {
                match args.parse::<usize>() {
                    Ok(bufid) => bufid,
                    Err(_) => return Err(format!("'{args}' is not a valid buffer id")),
                }
            };

            Ok(Single(MarkClean { bufid }))
        }

        "db" | "delete-buffer" => Ok(Single(DeleteBuffer { force: false })),
        "db!" | "delete-buffer!" => Ok(Single(DeleteBuffer { force: true })),

        "echo" => Ok(Single(SetStatusMessage {
            message: args.to_string(),
        })),

        "expand-dot" => Ok(Single(ExpandDot)),

        "E" | "Edit" => {
            if args.is_empty() {
                Err("No Edit script provided".to_string())
            } else {
                Ok(Single(EditCommand {
                    cmd: args.to_string(),
                }))
            }
        }

        "help" => Ok(Single(ShowHelp)),

        "o" | "open" => {
            if args.is_empty() {
                Err("No filename provided".to_string())
            } else {
                Ok(Single(OpenFile {
                    path: args.to_string(),
                }))
            }
        }

        "pwd" => Ok(Single(SetStatusMessage {
            message: cwd.display().to_string(),
        })),

        "q" | "quit" | "Exit" => Ok(Single(Exit { force: false })),
        "q!" | "quit!" | "Exit!" => Ok(Single(Exit { force: true })),

        "reload-config" => Ok(Single(ReloadConfig)),
        "reload-buffer" | "Get" => {
            if args.is_empty() {
                Ok(Single(ReloadActiveBuffer))
            } else {
                match args.parse::<usize>() {
                    Ok(id) => Ok(Single(ReloadBuffer { id })),
                    Err(_) => Err(format!("'{args}' is not a valid buffer id")),
                }
            }
        }

        "set" => Ok(Single(UpdateConfig {
            input: input.to_string(),
        })),

        "view-logs" => Ok(Single(ViewLogs)),

        "w" | "write" => {
            if args.is_empty() {
                Ok(Single(SaveBuffer { force: false }))
            } else {
                Ok(Single(SaveBufferAs {
                    path: args.to_string(),
                    force: false,
                }))
            }
        }
        "w!" | "write!" => {
            if args.is_empty() {
                Ok(Single(SaveBuffer { force: true }))
            } else {
                Ok(Single(SaveBufferAs {
                    path: args.to_string(),
                    force: true,
                }))
            }
        }

        "wq" | "write-quit" => Ok(Multi(vec![
            SaveBuffer { force: false },
            Exit { force: false },
        ])),

        "wq!" | "write-quit!" => Ok(Multi(vec![
            SaveBuffer { force: true },
            Exit { force: true },
        ])),

        "viewport-bottom" => Ok(Single(SetViewPort(ViewPort::Bottom))),
        "viewport-top" => Ok(Single(SetViewPort(ViewPort::Top))),
        "viewport-center" => Ok(Single(SetViewPort(ViewPort::Center))),

        "" => Err(String::new()),
        _ => Err(format!("Not an editor command: {command}")),
    }
}

impl Editor {
    pub(super) fn parse_command(&mut self, input: &str) -> Option<Actions> {
        match parse_command(input, self.active_buffer_id(), &self.cwd) {
            Ok(actions) => Some(actions),
            Err(msg) if msg.is_empty() => None,
            Err(msg) => {
                self.set_status_message(&msg);
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    /// The current command set accepted by the "Command" mode opened using ":".
    ///
    /// See the "known_commands_parse" test below which is used to ensure that the
    /// full command list is valid.
    const COMMANDS: [&str; 43] = [
        "b",
        "bn",
        "bp",
        "buffer",
        "buffer-next",
        "buffer-prev",
        "cd",
        "change-directory",
        "db!",
        "db",
        "delete-buffer!",
        "delete-buffer",
        "E",
        "echo",
        "Edit",
        "Exit!",
        "Exit",
        "expand-dot",
        "Get",
        "help",
        "mark-clean",
        "o",
        "open",
        "pwd",
        "q!",
        "q",
        "quit!",
        "quit",
        "reload-buffer",
        "reload-config",
        "set",
        "view-logs",
        "viewport-bottom",
        "viewport-center",
        "viewport-top",
        "w!",
        "w",
        "wq!",
        "wq",
        "write!",
        "write",
        "write-quit!",
        "write-quit",
    ];

    // The current behaviour of the command parser ignores additional input rather than erroring
    // which means we can always have the '1' argument here for all commands rather than needing
    // to pick out the buffer related commands that need an ID. If this behaviour changes the
    // test will need updating.
    #[test]
    fn known_commands_parse() {
        for raw_cmd in COMMANDS {
            let cmd = format!("{raw_cmd} 1");
            if let Err(msg) = parse_command(&cmd, 0, &PathBuf::new()) {
                panic!("{cmd:?} failed to parse: {msg:?}");
            }
        }

        for ch in "!<>|".chars() {
            let cmd = format!("{ch}some-shell-command");
            if let Err(msg) = parse_command(&cmd, 0, &PathBuf::new()) {
                panic!("{cmd:?} failed to parse: {msg:?}");
            }
        }
    }
}
