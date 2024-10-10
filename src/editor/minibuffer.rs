//! A transient buffer for handling interactive input from the user without
//! modifying the current buffer state.
//!
//! Conceptually this is operates as an embedded dmenu.
use crate::{
    buffer::{Buffer, GapBuffer},
    config_handle,
    dot::TextObject,
    editor::Editor,
    key::{Arrow, Input},
    util::run_command_blocking,
};
use std::{cmp::min, ffi::OsStr, fmt, path::Path};
use tracing::trace;

#[derive(Debug, Default)]
pub(crate) struct MiniBufferState<'a> {
    pub(crate) cx: usize,
    pub(crate) cy: usize,
    pub(crate) selected_line_idx: usize,
    pub(crate) prompt: &'a str,
    pub(crate) input: &'a str,
    pub(crate) b: Option<&'a Buffer>,
    pub(crate) top: usize,
    pub(crate) bottom: usize,
}

pub(crate) enum MiniBufferSelection {
    Line { cy: usize, line: String },
    UserInput { input: String },
    Cancelled,
}

/// A mini-buffer always has a single line prompt for accepting user input
/// with the rest of the buffer content not being directly editable.
///
/// Conceptually this is operates as an embedded dmenu.
pub(crate) struct MiniBuffer<F>
where
    F: Fn(&str) -> Option<Vec<String>>,
{
    on_change: F,
    prompt: String,
    input: String,
    initial_lines: Vec<String>,
    line_indices: Vec<usize>,
    b: Buffer,
    max_height: usize,
    x: usize,
    y: usize,
    selected_line_idx: usize,
    n_visible_lines: usize,
    top: usize,
    bottom: usize,
    show_buffer_content: bool,
}

impl<F> fmt::Debug for MiniBuffer<F>
where
    F: Fn(&str) -> Option<Vec<String>>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MiniBuffer")
            .field("prompt", &self.prompt)
            .field("input", &self.input)
            .finish()
    }
}

impl<F> MiniBuffer<F>
where
    F: Fn(&str) -> Option<Vec<String>>,
{
    pub fn new(prompt: String, lines: Vec<String>, max_height: usize, on_change: F) -> Self {
        let line_indices = Vec::with_capacity(lines.len());

        Self {
            on_change,
            prompt,
            input: String::new(),
            initial_lines: lines,
            line_indices,
            b: Buffer::new_minibuffer(),
            max_height,
            x: 0,
            y: 0,
            selected_line_idx: 0,
            n_visible_lines: 0,
            top: 0,
            bottom: 0,
            show_buffer_content: true,
        }
    }

    /// Force the cursor to be a single Cur and ensure that its y offset is in bounds
    #[inline]
    fn handle_on_change(&mut self) {
        if let Some(lines) = (self.on_change)(&self.input) {
            self.b.txt = GapBuffer::from(lines.join("\n"));
            self.b.dot.clamp_idx(self.b.txt.len_chars());
        };
    }

    #[inline]
    fn update_state(&mut self) {
        self.b.txt.clear();
        self.line_indices.clear();

        let input_fragments: Vec<&str> = self.input.split_whitespace().collect();
        let mut visible_lines = vec![];

        for (i, line) in self.initial_lines.iter().enumerate() {
            let matching = input_fragments.iter().all(|f| {
                if f.chars().all(|c| c.is_lowercase()) {
                    line.to_lowercase().contains(f)
                } else {
                    line.contains(f)
                }
            });

            if matching {
                visible_lines.push(line.clone());
                self.line_indices.push(i);
            }
        }

        self.b.txt = GapBuffer::from(visible_lines.join("\n"));
        self.b.dot.clamp_idx(self.b.txt.len_chars());

        let n_visible_lines = min(visible_lines.len(), self.max_height);
        let (y, _) = self.b.dot.active_cur().as_yx(&self.b);

        let (selected_line_idx, top, bottom, show_buffer_content) = if n_visible_lines == 0 {
            (0, 0, 0, false)
        } else if y >= n_visible_lines {
            let lower = y.saturating_sub(n_visible_lines) + 1;
            (y, lower, y, true)
        } else {
            (y, 0, n_visible_lines - 1, true)
        };

        self.show_buffer_content = show_buffer_content;
        self.selected_line_idx = selected_line_idx;
        self.n_visible_lines = n_visible_lines;
        self.y = y;
        self.top = top;
        self.bottom = bottom;
    }

    #[inline]
    fn current_state(&self, screen_rows: usize) -> MiniBufferState<'_> {
        MiniBufferState {
            cx: self.x + self.prompt.len(),
            cy: screen_rows + self.n_visible_lines + 1,
            prompt: &self.prompt,
            input: &self.input,
            selected_line_idx: self.selected_line_idx,
            b: if self.show_buffer_content {
                Some(&self.b)
            } else {
                None
            },
            top: self.top,
            bottom: self.bottom,
        }
    }

    #[inline]
    fn handle_input(&mut self, input: Input) -> Option<MiniBufferSelection> {
        match input {
            Input::Char(c) => {
                self.input.insert(self.x, c);
                self.x += 1;
                self.handle_on_change();
            }
            Input::Ctrl('h') | Input::Backspace | Input::Del => {
                if self.x > 0 && self.x <= self.input.len() {
                    self.input.remove(self.x - 1);
                    self.x = self.x.saturating_sub(1);
                    self.handle_on_change();
                }
            }

            Input::Esc => return Some(MiniBufferSelection::Cancelled),
            Input::Return => {
                let selection = match self.b.line(self.y) {
                    Some(_) if self.line_indices.is_empty() => MiniBufferSelection::UserInput {
                        input: self.input.clone(),
                    },
                    Some(l) => MiniBufferSelection::Line {
                        cy: self.line_indices[self.y],
                        line: l.to_string(),
                    },
                    None => MiniBufferSelection::UserInput {
                        input: self.input.clone(),
                    },
                };
                return Some(selection);
            }

            Input::Alt('h') | Input::Arrow(Arrow::Left) => self.x = self.x.saturating_sub(1),
            Input::Alt('l') | Input::Arrow(Arrow::Right) => {
                self.x = min(self.x + 1, self.input.len())
            }
            Input::Alt('k') | Input::Arrow(Arrow::Up) => {
                if self.selected_line_idx == 0 {
                    self.b.set_dot(TextObject::BufferEnd, 1);
                } else {
                    self.b.set_dot(TextObject::Arr(Arrow::Up), 1);
                }
            }
            Input::Alt('j') | Input::Arrow(Arrow::Down) => {
                if self.selected_line_idx == self.b.len_lines() - 1 {
                    self.b.set_dot(TextObject::BufferStart, 1);
                } else {
                    self.b.set_dot(TextObject::Arr(Arrow::Down), 1);
                }
            }

            _ => (),
        }

        None
    }
}

impl Editor {
    fn prompt_w_callback<F: Fn(&str) -> Option<Vec<String>>>(
        &mut self,
        prompt: &str,
        initial_lines: Vec<String>,
        on_change: F,
    ) -> MiniBufferSelection {
        let mut mb = MiniBuffer::new(
            prompt.to_string(),
            initial_lines,
            config_handle!().minibuffer_lines,
            on_change,
        );

        loop {
            mb.update_state();
            self.refresh_screen_w_minibuffer(Some(mb.current_state(self.screen_rows)));
            let input = self.block_for_input();
            if let Some(selection) = mb.handle_input(input) {
                return selection;
            }
        }
    }

    /// Use the minibuffer to prompt for user input
    pub(crate) fn minibuffer_prompt(&mut self, prompt: &str) -> Option<String> {
        trace!(%prompt, "opening mini-buffer");
        match self.prompt_w_callback(prompt, vec![], |_| None) {
            MiniBufferSelection::UserInput { input } => Some(input),
            _ => None,
        }
    }

    /// Append ", continue? [y/n]: " to the prompt and return true if the user enters one of
    /// y, Y, yes, YES, Yes (otherwise return false)
    pub(crate) fn minibuffer_confirm(&mut self, prompt: &str) -> bool {
        let resp = self.minibuffer_prompt(&format!("{prompt}, continue? [y/n]: "));

        matches!(resp.as_deref(), Some("y" | "Y" | "yes"))
    }

    /// Use a [MiniBuffer] to select from a list of strings.
    pub(crate) fn minibuffer_select_from(
        &mut self,
        prompt: &str,
        initial_lines: Vec<String>,
    ) -> MiniBufferSelection {
        self.prompt_w_callback(prompt, initial_lines, |_| None)
    }

    /// Use a [MiniBuffer] to select from the newline delimited output of running a shell command.
    pub(crate) fn minibuffer_select_from_command_output<S, I>(
        &mut self,
        prompt: &str,
        cmd: &str,
        args: I,
        dir: &Path,
    ) -> MiniBufferSelection
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let initial_lines = match run_command_blocking(cmd, args, dir, self.active_buffer_id()) {
            Ok(s) => s.lines().map(String::from).collect(),
            Err(e) => {
                self.set_status_message(&format!("unable to get minibuffer input: {e}"));
                return MiniBufferSelection::Cancelled;
            }
        };

        self.prompt_w_callback(prompt, initial_lines, |_| None)
    }
}
