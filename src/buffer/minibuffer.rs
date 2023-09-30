//! A transient buffer for handling interactive input from the user without
//! modifying the current buffer state.
use crate::{
    buffer::{Buffer, BufferKind, Line},
    editor::Editor,
    key::{Arrow, Key},
    MINI_BUFFER_HEIGHT,
};
use std::cmp::min;

#[derive(Default)]
pub(crate) struct MiniBufferState<'a> {
    pub(crate) cx: usize,
    pub(crate) cy: usize,
    pub(crate) selected_line_idx: usize,
    pub(crate) prompt_line: &'a str,
    pub(crate) lines: &'a [Line],
}

pub(crate) enum MiniBufferSelection {
    Line(usize, String),
    UserInput(String),
    Cancelled,
}

/// A mini-buffer always has a single line prompt for accepting user input
/// with the rest of the buffer content not being directly editable.
///
/// Conceptually this is along the lines of the window tags in acme (a space
/// to scribble down additional useful information without affecting the
/// files you are editing) crossed with dmenu.
pub(crate) struct MiniBuffer {
    prompt: String,
    b: Buffer,
    max_height: usize,
}

impl MiniBuffer {
    pub fn new(prompt: String, lines: Vec<Line>, max_height: usize) -> Self {
        Self {
            prompt,
            b: Buffer {
                kind: BufferKind::MiniBuffer,
                lines,
                cx: 0,
                cy: 0,
                rx: 0,
                row_off: 0,
                col_off: 0,
                dirty: false,
            },
            max_height,
        }
    }

    fn handle_on_change<F: Fn(&str, &mut Vec<Line>)>(&mut self, input: &str, on_change: F) {
        (on_change)(input, &mut self.b.lines);

        self.b.cy = if self.b.lines.is_empty() {
            0
        } else {
            min(self.b.lines.len() - 1, self.b.cy)
        };
    }

    pub fn prompt_w_callback<F: Fn(&str, &mut Vec<Line>)>(
        prompt: &str,
        initial_lines: Vec<Line>,
        on_change: F,
        ed: &mut Editor,
    ) -> MiniBufferSelection {
        let mut mb = MiniBuffer::new(prompt.to_string(), initial_lines, MINI_BUFFER_HEIGHT);
        let mut input = String::new();
        let mut x = 0;
        let offset = prompt.len();
        let (screen_rows, _) = ed.screen_rowcol();

        loop {
            mb.prompt = format!("{prompt}{input}");
            let n_visible_lines = min(mb.b.lines.len(), mb.max_height);
            let max_line_idx = min(mb.b.lines.len(), mb.b.row_off + n_visible_lines);

            ed.refresh_screen_w_minibuffer(Some(MiniBufferState {
                cx: x + offset,
                cy: screen_rows + 1 + n_visible_lines,
                selected_line_idx: mb.b.cy,
                prompt_line: &mb.prompt,
                lines: &mb.b.lines[mb.b.row_off..max_line_idx],
            }));

            match ed.read_key() {
                Key::Char(c) => {
                    input.insert(x, c);
                    x += 1;
                    mb.handle_on_change(&input, &on_change);
                }
                Key::Ctrl('h') | Key::Backspace | Key::Del => {
                    if x <= input.len() {
                        input.remove(x - 1);
                        x = x.saturating_sub(1);
                        mb.handle_on_change(&input, &on_change);
                    }
                }

                Key::Esc => return MiniBufferSelection::Cancelled,
                Key::Return => {
                    return mb
                        .b
                        .lines
                        .get(mb.b.cy)
                        .map(|l| MiniBufferSelection::Line(mb.b.cy, l.raw.clone()))
                        .unwrap_or_else(|| MiniBufferSelection::UserInput(input.to_string()))
                }

                Key::Arrow(Arrow::Right) => x = min(x + 1, input.len()),
                Key::Arrow(Arrow::Left) => x = x.saturating_sub(1),
                Key::Ctrl('k') | Key::Arrow(Arrow::Up) => {
                    mb.b.move_cursor(Arrow::Up, 1);
                }
                Key::Ctrl('j') | Key::Arrow(Arrow::Down) => {
                    mb.b.move_cursor(Arrow::Down, 1);
                }

                _ => (),
            }
        }
    }

    pub fn prompt(prompt: &str, ed: &mut Editor) -> Option<String> {
        match MiniBuffer::prompt_w_callback(prompt, vec![], |_, _| (), ed) {
            MiniBufferSelection::UserInput(s) => Some(s),
            _ => None,
        }
    }

    // pub fn select_from(
    //     prompt: &str,
    //     initial_lines: Vec<Line>,
    //     ed: &mut Editor,
    // ) -> MiniBufferSelection {
    //     MiniBuffer::prompt_w_callback(
    //         prompt,
    //         initial_lines.clone(),
    //         |input, lines| {
    //             let mut filtered_lines = initial_lines.clone();
    //             filtered_lines.retain(|l| l.raw.contains(input));
    //             *lines = filtered_lines;
    //         },
    //         ed,
    //     )
    // }
}
