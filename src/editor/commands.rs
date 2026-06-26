//! Command mode commands for ad
use crate::{
    editor::{
        Actions::{self, *},
        BAction, EAction, Editor, UAction, ViewPort,
    },
    system::System,
};
use std::path::Path;

#[cfg(feature = "fuzz")]
pub fn parse_command_fuzz(input: &str) {
    _ = parse_command(input, 0, Path::new("/home/fuzz"));
}

fn parse_command(input: &str, active_buffer_id: usize, cwd: &Path) -> Result<Actions, String> {
    if let Some(actions) = try_parse_single_char_command(input, active_buffer_id) {
        return Ok(actions);
    }

    let input = input.trim_end();
    let (command, args) = input.split_once(' ').unwrap_or((input, ""));

    match command {
        "b" | "buffer" => match args.parse::<usize>() {
            Ok(id) => Ok(EAction::FocusBuffer { id }.into()),
            Err(_) => Err(format!("'{args}' is not a valid buffer id")),
        },
        "bn" | "next-buffer" => Ok(UAction::NextBuffer.into()),
        "bp" | "prev-buffer" => Ok(UAction::PreviousBuffer.into()),
        "next-column" => Ok(UAction::NextColumn.into()),
        "next-window" => Ok(UAction::NextWindowInColumn.into()),
        "prev-column" => Ok(UAction::PreviousColumn.into()),
        "prev-window" => Ok(UAction::PreviousWindowInColumn.into()),

        "balance-all" => Ok(UAction::BalanceAll.into()),
        "balance-column" => Ok(UAction::BalanceActiveColumn.into()),
        "balance-columns" => Ok(UAction::BalanceColumns.into()),
        "balance-windows" => Ok(UAction::BalanceWindows.into()),

        "cd" | "change-directory" => {
            if args.is_empty() {
                Ok(EAction::ChangeDirectory { path: None }.into())
            } else {
                Ok(EAction::ChangeDirectory {
                    path: Some(args.to_string()),
                }
                .into())
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

            Ok(BAction::MarkClean.for_buffer(bufid).into())
        }

        "db" | "delete-buffer" => Ok(EAction::DeleteBuffer {
            bufid: try_parse_bufid(args, active_buffer_id)?,
            force: false,
        }
        .into()),

        "db!" | "delete-buffer!" => Ok(EAction::DeleteBuffer {
            bufid: try_parse_bufid(args, active_buffer_id)?,
            force: true,
        }
        .into()),

        "dc" | "delete-column" => Ok(UAction::DeleteColumn { force: false }.into()),
        "dc!" | "delete-column!" => Ok(UAction::DeleteColumn { force: true }.into()),
        "dw" | "delete-window" => Ok(UAction::DeleteWindow { force: false }.into()),
        "dw!" | "delete-window!" => Ok(UAction::DeleteWindow { force: true }.into()),

        "echo" => Ok(EAction::SetStatusMessage {
            message: args.to_string(),
        }
        .into()),

        "expand-dot" => Ok(BAction::ExpandDot.for_active().into()),

        "E" | "Edit" => {
            if args.is_empty() {
                Err("No Edit script provided".to_string())
            } else {
                Ok(EAction::EditCommand {
                    bufid: Some(active_buffer_id),
                    cmd: args.to_string(),
                }
                .into())
            }
        }

        "execute" => Ok(EAction::ExecuteDot {
            bufid: Some(active_buffer_id),
        }
        .into()),
        "help" => Ok(EAction::ShowHelp.into()),
        "kill" => Ok(EAction::KillRunningChild { idx: None }.into()),
        "load" => Ok(EAction::LoadDot {
            bufid: Some(active_buffer_id),
            new_window: false,
        }
        .into()),
        "plumb" => Ok(EAction::Plumb {
            txt: args.to_string(),
            new_window: false,
        }
        .into()),

        "lsp-completion" => Ok(EAction::LspCompletion.into()),
        "lsp-find-references" => Ok(EAction::LspReferences.into()),
        "lsp-format" => Ok(EAction::LspFormat.into()),
        "lsp-goto-declaration" => Ok(EAction::LspGotoDeclaration.into()),
        "lsp-goto-definition" => Ok(EAction::LspGotoDefinition.into()),
        "lsp-goto-type-definition" => Ok(EAction::LspGotoTypeDefinition.into()),
        "lsp-hover" => Ok(EAction::LspHover.into()),
        "lsp-rename" => Ok(EAction::LspRenamePrepare.into()),
        "lsp-show-capabilities" => Ok(EAction::LspShowCapabilities.into()),
        "lsp-show-diagnostics" => Ok(EAction::LspShowDiagnostics.into()),
        "lsp-start" => Ok(EAction::LspStart.into()),
        "lsp-stop" => Ok(EAction::LspStop.into()),

        "o" | "open" => {
            if args.is_empty() {
                Err("No filename provided".to_string())
            } else {
                Ok(EAction::OpenFile {
                    path: args.to_string(),
                    new_window: false,
                }
                .into())
            }
        }

        "O" | "open-in-new-window" => {
            if args.is_empty() {
                Err("No filename provided".to_string())
            } else {
                Ok(EAction::OpenFile {
                    path: args.to_string(),
                    new_window: true,
                }
                .into())
            }
        }

        "open-virtual" => {
            if args.is_empty() {
                Err("No filename provided".to_string())
            } else {
                let (name, txt) = args.split_once(' ').unwrap_or((args, ""));
                Ok(EAction::OpenVirtualFile {
                    name: name.to_string(),
                    txt: txt.to_string(),
                    new_window: false,
                }
                .into())
            }
        }

        "open-virtual-in-new-window" => {
            if args.is_empty() {
                Err("No filename provided".to_string())
            } else {
                let (name, txt) = args.split_once(' ').unwrap_or((args, ""));
                Ok(EAction::OpenVirtualFile {
                    name: name.to_string(),
                    txt: txt.to_string(),
                    new_window: true,
                }
                .into())
            }
        }

        "new-column" => Ok(UAction::NewColumn.into()),
        "new-window" => Ok(UAction::NewWindow.into()),

        "pwd" => Ok(EAction::SetStatusMessage {
            message: cwd.display().to_string(),
        }
        .into()),

        "q" | "quit" | "Exit" => Ok(EAction::Exit { force: false }.into()),
        "q!" | "quit!" | "Exit!" => Ok(EAction::Exit { force: true }.into()),

        "reload-config" => Ok(EAction::ReloadConfig.into()),
        "reload-buffer" | "Get" => {
            if args.is_empty() {
                Ok(EAction::ReloadBuffer {
                    bufid: Some(active_buffer_id),
                }
                .into())
            } else {
                match args.parse::<usize>() {
                    Ok(id) => Ok(EAction::ReloadBuffer { bufid: Some(id) }.into()),
                    Err(_) => Err(format!("'{args}' is not a valid buffer id")),
                }
            }
        }

        "rename-buffer" => Ok(BAction::Rename {
            name: args.to_string(),
        }
        .for_buffer(active_buffer_id)
        .into()),

        "resize-column" => match args.parse::<i16>() {
            Ok(delta) => Ok(UAction::ResizeActiveColumn { delta }.into()),
            Err(_) => Err(format!("'{args}' is not a valid delta")),
        },
        "resize-window" => match args.parse::<i16>() {
            Ok(delta) => Ok(UAction::ResizeActiveWindow { delta }.into()),
            Err(_) => Err(format!("'{args}' is not a valid delta")),
        },

        "clear-scratch" => Ok(EAction::ClearScratch.into()),
        "toggle-scratch" => Ok(EAction::ToggleScratch.into()),

        "ts-show-tree" => Ok(EAction::TsShowTree.into()),
        "view-logs" => Ok(EAction::ViewLogs.into()),

        "w" | "write" => {
            if args.is_empty() {
                Ok(EAction::SaveBuffer { force: false }.into())
            } else {
                Ok(EAction::SaveBufferAs {
                    path: args.to_string(),
                    force: false,
                }
                .into())
            }
        }
        "w!" | "write!" => {
            if args.is_empty() {
                Ok(EAction::SaveBuffer { force: true }.into())
            } else {
                Ok(EAction::SaveBufferAs {
                    path: args.to_string(),
                    force: true,
                }
                .into())
            }
        }

        "wa" | "write-all" => Ok(EAction::SaveBufferAll { force: false }.into()),
        "wa!" | "write-all!" => Ok(EAction::SaveBufferAll { force: true }.into()),

        "wq" | "write-quit" => Ok(Multi(vec![
            EAction::SaveBuffer { force: false }.into(),
            EAction::Exit { force: false }.into(),
        ])),

        "wq!" | "write-quit!" => Ok(Multi(vec![
            EAction::SaveBuffer { force: true }.into(),
            EAction::Exit { force: true }.into(),
        ])),

        "viewport-bottom" => Ok(UAction::SetViewPort(ViewPort::Bottom).into()),
        "viewport-top" => Ok(UAction::SetViewPort(ViewPort::Top).into()),
        "viewport-center" => Ok(UAction::SetViewPort(ViewPort::Center).into()),

        "" => Err(String::new()),
        _ => Err(String::new()),
    }
}

impl<S> Editor<S>
where
    S: System,
{
    pub(super) fn parse_command(&mut self, bufid: usize, input: &str) -> Option<Actions> {
        match parse_command(input, bufid, &self.cwd) {
            Ok(actions) => Some(actions),
            Err(msg) if msg.is_empty() => None,
            Err(msg) => {
                self.set_status_message(&msg);
                None
            }
        }
    }
}

fn try_parse_bufid(args: &str, active_buffer_id: usize) -> Result<usize, String> {
    if args.is_empty() {
        Ok(active_buffer_id)
    } else {
        match args.parse::<usize>() {
            Ok(bufid) => Ok(bufid),
            Err(_) => Err(format!("'{args}' is not a valid buffer id")),
        }
    }
}

fn try_parse_single_char_command(input: &str, active_buffer_id: usize) -> Option<Actions> {
    match input.chars().next() {
        Some('!') => Some(
            EAction::ShellRun {
                bufid: Some(active_buffer_id),
                cmd: input[1..].to_string(),
            }
            .into(),
        ),
        Some('|') => Some(
            EAction::ShellPipe {
                bufid: Some(active_buffer_id),
                cmd: input[1..].to_string(),
            }
            .into(),
        ),
        Some('<') => Some(
            EAction::ShellReplace {
                bufid: Some(active_buffer_id),
                cmd: input[1..].to_string(),
            }
            .into(),
        ),
        Some('>') => Some(
            EAction::ShellSend {
                bufid: Some(active_buffer_id),
                cmd: input[1..].to_string(),
            }
            .into(),
        ),

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editor::built_in_commands::built_in_commands;
    use std::path::PathBuf;

    // The current behaviour of the command parser ignores additional input rather than erroring
    // which means we can always have the '1' argument here for all commands rather than needing
    // to pick out the buffer related commands that need an ID. If this behaviour changes the
    // test will need updating.
    #[test]
    fn known_commands_parse() {
        for (cmds, _) in built_in_commands().into_iter() {
            for raw_cmd in cmds.into_iter() {
                let cmd = format!("{raw_cmd} 1");
                if let Err(msg) = parse_command(&cmd, 0, &PathBuf::new()) {
                    panic!("{cmd:?} failed to parse: {msg:?}");
                }
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
