//! Utility functions
use crate::editor::built_in_commands;
use std::{
    iter::Peekable,
    str::Chars,
};
use tracing::warn;

/// Pull in data from the ad crate itself to auto-generate the docs on the functionality
/// available in the editor.
pub(crate) fn gen_help_docs() -> String {
    let help_template = include_str!("../data/help-template.txt");

    help_template.replace("{{BUILT_IN_COMMANDS}}", &commands_section())
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

