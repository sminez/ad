//! Command mode commands for ad
use crate::editor::{
    Action::{self, *},
    Editor,
};

impl Editor {
    pub(super) fn parse_command(&mut self, input: &str) -> Option<Vec<Action>> {
        let (command, args) = if input.contains(' ') {
            input.split_once(' ')?
        } else {
            (input, "")
        };

        match command {
            "w" | "write" => {
                if args.is_empty() {
                    Some(vec![SaveBuffer])
                } else {
                    Some(vec![SaveBufferAs(args.to_string())])
                }
            }

            "q" | "quit" => Some(vec![Exit]),
            "Q" | "qall" | "quitall" => Some(vec![ForceExit]),

            "" => None,

            _ => {
                self.set_status_message(&format!("Not an editor command: {input}"));
                None
            }
        }
    }
}
