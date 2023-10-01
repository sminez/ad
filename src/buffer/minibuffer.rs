//! A transient buffer for handling interactive input from the user without
//! modifying the current buffer state.
use crate::{
    buffer::{
        dot::{Cur, Dot, UpdateDot},
        Buffer, BufferKind, Line,
    },
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

// TODO: remove this allow when line selection fields are in use
#[allow(dead_code)]
pub(crate) enum MiniBufferSelection {
    Line {
        cy: usize,
        line: String,
        input: String,
    },
    UserInput {
        input: String,
    },
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
    initial_lines: Vec<Line>,
    b: Buffer,
    max_height: usize,
}

impl MiniBuffer {
    pub fn new(prompt: String, lines: Vec<Line>, max_height: usize) -> Self {
        Self {
            prompt,
            initial_lines: lines,
            b: Buffer {
                id: usize::MAX,
                kind: BufferKind::MiniBuffer,
                dot: Default::default(),
                lines: vec![],
                rx: 0,
                row_off: 0,
                col_off: 0,
                dirty: false,
            },
            max_height,
        }
    }

    /// Force the cursor to be a single Cur and ensure that its y offset is in bounds
    fn handle_on_change<F: Fn(&str) -> Option<Vec<Line>>>(&mut self, input: &str, on_change: F) {
        if let Some(lines) = (on_change)(input) {
            self.b.lines = lines;
        };

        let c = Cur {
            x: 0,
            y: if self.b.lines.is_empty() {
                0
            } else {
                min(self.b.lines.len() - 1, self.b.dot.first_cur().y)
            },
        };

        self.b.dot = Dot::Cur { c };
    }

    pub fn prompt_w_callback<F: Fn(&str) -> Option<Vec<Line>>>(
        prompt: &str,
        initial_lines: Vec<Line>,
        on_change: F,
        ed: &mut Editor,
    ) -> MiniBufferSelection {
        let offset = prompt.len();
        let (screen_rows, screen_cols) = ed.screen_rowcol();
        let mut mb = MiniBuffer::new(prompt.to_string(), initial_lines, MINI_BUFFER_HEIGHT);
        let mut input = String::new();
        let mut x = 0;
        let mut line_indices: Vec<usize> = vec![];

        loop {
            mb.prompt = format!("{prompt}{input}");
            mb.b.lines.clear();
            line_indices.clear();

            for (i, line) in mb.initial_lines.iter().enumerate() {
                if line.raw.contains(&input) {
                    mb.b.lines.push(line.clone());
                    line_indices.push(i);
                }
            }

            let n_visible_lines = min(mb.b.lines.len(), mb.max_height);
            mb.b.clamp_scroll(n_visible_lines, screen_cols);
            let Cur { y, .. } = mb.b.dot.first_cur();

            let (selected_line_idx, lines): (usize, &[Line]) = if n_visible_lines == 0 {
                (0, &[])
            } else if y >= n_visible_lines {
                let lower = y.saturating_sub(n_visible_lines) + 1;
                (n_visible_lines - 1, &mb.b.lines[lower..(y + 1)])
            } else {
                (y, &mb.b.lines[0..n_visible_lines])
            };

            ed.refresh_screen_w_minibuffer(Some(MiniBufferState {
                cx: x + offset,
                cy: screen_rows + 1 + n_visible_lines,
                prompt_line: &mb.prompt,
                selected_line_idx,
                lines,
            }));

            match ed.block_for_key() {
                Key::Char(c) => {
                    input.insert(x, c);
                    x += 1;
                    mb.handle_on_change(&input, &on_change);
                }
                Key::Ctrl('h') | Key::Backspace | Key::Del => {
                    if x > 0 && x <= input.len() {
                        input.remove(x - 1);
                        x = x.saturating_sub(1);
                        mb.handle_on_change(&input, &on_change);
                    }
                }

                Key::Esc => return MiniBufferSelection::Cancelled,
                Key::Return => {
                    return match mb.b.lines.get(y) {
                        Some(l) => MiniBufferSelection::Line {
                            cy: line_indices[y],
                            line: l.raw.clone(),
                            input,
                        },
                        None => MiniBufferSelection::UserInput { input },
                    };
                }

                Key::Arrow(Arrow::Right) => x = min(x + 1, input.len()),
                Key::Arrow(Arrow::Left) => x = x.saturating_sub(1),
                Key::Alt('k') | Key::Arrow(Arrow::Up) => {
                    Arrow::Up.set_dot(&mb.b);
                }
                Key::Alt('j') | Key::Arrow(Arrow::Down) => {
                    Arrow::Down.set_dot(&mb.b);
                }

                _ => (),
            }
        }
    }

    pub fn prompt(prompt: &str, ed: &mut Editor) -> Option<String> {
        match MiniBuffer::prompt_w_callback(prompt, vec![], |_| None, ed) {
            MiniBufferSelection::UserInput { input } => Some(input),
            _ => None,
        }
    }

    pub fn select_from(
        prompt: &str,
        initial_lines: Vec<Line>,
        ed: &mut Editor,
    ) -> MiniBufferSelection {
        MiniBuffer::prompt_w_callback(prompt, initial_lines, |_| None, ed)
    }
}
