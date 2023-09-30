//! Command mode commands for ad
use crate::editor::{
    Action::{self, *},
    Editor,
};

// FIXME: Probably want / need the ability to have individual actions without the need to
// allocate a new vec?
impl Editor {
    pub(super) fn parse_command(&mut self, input: &str) -> Option<Vec<Action>> {
        let (command, args) = if input.contains(' ') {
            input.split_once(' ')?
        } else {
            (input, "")
        };

        match command {
            "bc" | "buffer-close" => Some(vec![CloseBuffer]),
            "bn" | "buffer-next" => Some(vec![NextBuffer]),
            "bp" | "buffer-previous" => Some(vec![PreviousBuffer]),

            "e" | "edit" => {
                if args.is_empty() {
                    self.set_status_message("No filename provided");
                    None
                } else {
                    Some(vec![OpenFile {
                        path: args.to_string(),
                    }])
                }
            }

            "q" | "quit" => Some(vec![Exit { force: false }]),
            "Q" | "quit-all" => Some(vec![Exit { force: true }]),

            "w" | "write" => {
                if args.is_empty() {
                    Some(vec![SaveBuffer])
                } else {
                    Some(vec![SaveBufferAs {
                        path: args.to_string(),
                    }])
                }
            }

            "" => None,

            _ => {
                self.set_status_message(&format!("Not an editor command: {input}"));
                None
            }
        }
    }
}
