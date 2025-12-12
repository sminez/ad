//! A transient buffer for handling interactive input from the user without
//! modifying the current buffer state.
//!
//! Conceptually this is operates as an embedded dmenu.
use crate::{
    Config,
    buffer::{Buffer, Buffers, GapBuffer, Slice},
    config_handle,
    dot::TextObject,
    editor::{Action, Actions, Editor},
    key::{Arrow, Input},
    system::System,
};
use ad_event::Source;
use std::{
    cmp::{self, min},
    fmt,
    path::Path,
    sync::{Arc, RwLock},
};
use tracing::trace;

const MINIBUFFER_ID: usize = usize::MAX - 1;

#[derive(Debug, Default)]
pub struct MiniBufferState<'a> {
    pub(crate) cx: usize,
    pub(crate) n_visible_lines: usize,
    pub(crate) selected_line_idx: usize,
    pub(crate) prompt: &'a str,
    pub(crate) input: Slice<'a>,
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
    F: Fn(&GapBuffer) -> Option<Vec<String>>,
{
    on_change: F,
    prompt: String,
    n_prompt_chars: usize,
    input: Buffer,
    initial_lines: Vec<String>,
    line_indices: Vec<usize>,
    b: Buffer,
    max_height: usize,
    y: usize,
    selected_line_idx: usize,
    n_visible_lines: usize,
    top: usize,
    bottom: usize,
    show_buffer_content: bool,
}

impl<F> fmt::Debug for MiniBuffer<F>
where
    F: Fn(&GapBuffer) -> Option<Vec<String>>,
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
    F: Fn(&GapBuffer) -> Option<Vec<String>>,
{
    pub fn new(
        prompt: String,
        lines: Vec<String>,
        max_height: usize,
        on_change: F,
        config: Arc<RwLock<Config>>,
    ) -> Self {
        let line_indices = Vec::with_capacity(lines.len());
        let n_prompt_chars = prompt.chars().count();

        Self {
            on_change,
            prompt,
            n_prompt_chars,
            input: Buffer::new_unnamed(MINIBUFFER_ID, "", config.clone()),
            initial_lines: lines,
            line_indices,
            b: Buffer::new_minibuffer(config),
            max_height,
            y: 0,
            selected_line_idx: 0,
            n_visible_lines: 0,
            top: 0,
            bottom: 0,
            show_buffer_content: true,
        }
    }

    #[inline]
    fn handle_on_change(&mut self) {
        if let Some(lines) = (self.on_change)(&self.input.txt) {
            self.b.txt = GapBuffer::from(lines.join("\n"));
            self.b.dot.clamp_idx(self.b.txt.len_chars());
        };
    }

    #[inline]
    fn update_state(&mut self) {
        self.b.txt.clear();
        self.line_indices.clear();

        let input_fragments: Vec<&str> = self.input.txt.as_str().split_whitespace().collect();
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
    fn current_state(&self) -> MiniBufferState<'_> {
        MiniBufferState {
            cx: self.input.dot.active_cur().idx + self.n_prompt_chars,
            n_visible_lines: self.n_visible_lines,
            prompt: &self.prompt,
            input: self.input.txt.as_slice(),
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
                self.input
                    .handle_action(Action::InsertChar { c }, Source::Keyboard);
                self.handle_on_change();
            }
            Input::Ctrl('h') | Input::Backspace | Input::Del => {
                self.input.handle_action(
                    Action::DotSet(TextObject::Arr(Arrow::Left), 1),
                    Source::Keyboard,
                );
                self.input.handle_action(Action::Delete, Source::Keyboard);
                self.handle_on_change();
            }

            Input::Esc => return Some(MiniBufferSelection::Cancelled),
            Input::Return => {
                let selection = match self.b.line(self.y) {
                    Some(_) if self.line_indices.is_empty() => MiniBufferSelection::UserInput {
                        input: self.input.txt.to_string(),
                    },
                    Some(l) => MiniBufferSelection::Line {
                        cy: self.line_indices[self.y],
                        line: l.to_string(),
                    },
                    None => MiniBufferSelection::UserInput {
                        input: self.input.txt.to_string(),
                    },
                };
                return Some(selection);
            }

            Input::Alt('h') | Input::Arrow(Arrow::Left) => {
                self.input.handle_action(
                    Action::DotSet(TextObject::Arr(Arrow::Left), 1),
                    Source::Keyboard,
                );
            }
            Input::Alt('l') | Input::Arrow(Arrow::Right) => {
                self.input.handle_action(
                    Action::DotSet(TextObject::Arr(Arrow::Right), 1),
                    Source::Keyboard,
                );
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

impl<S> Editor<S>
where
    S: System,
{
    fn prompt_w_callback<F: Fn(&GapBuffer) -> Option<Vec<String>>>(
        &mut self,
        prompt: &str,
        initial_lines: Vec<String>,
        initial_input: Option<String>,
        on_change: F,
    ) -> MiniBufferSelection {
        let mut mb = MiniBuffer::new(
            prompt.to_string(),
            initial_lines,
            config_handle!(self).minibuffer_lines,
            on_change,
            self.config.clone(),
        );

        if let Some(s) = initial_input {
            mb.input
                .handle_action(Action::InsertString { s }, Source::Fsys);
        }

        while self.running {
            mb.update_state();
            self.refresh_screen_w_minibuffer(Some(mb.current_state()));
            let inputs = self.block_for_input();
            for input in inputs.into_iter() {
                if let Some(selection) = mb.handle_input(input) {
                    return selection;
                }
            }
        }

        MiniBufferSelection::Cancelled
    }

    /// Use the minibuffer to prompt for user input
    pub(crate) fn minibuffer_prompt(&mut self, prompt: &str) -> Option<String> {
        trace!(%prompt, "opening mini-buffer");
        match self.prompt_w_callback(prompt, vec![], None, |_| None) {
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
        self.prompt_w_callback(prompt, initial_lines, None, |_| None)
    }

    /// Use a [MiniBuffer] to select from the newline delimited output of running a shell command.
    pub(crate) fn minibuffer_select_from_command_output(
        &mut self,
        prompt: &str,
        cmd: &str,
        dir: &Path,
    ) -> MiniBufferSelection {
        let initial_lines =
            match self
                .system
                .run_command_blocking(cmd, dir, self.active_buffer_id())
            {
                Ok(s) => s.lines().map(String::from).collect(),
                Err(e) => {
                    self.set_status_message(format!("unable to get minibuffer input: {e}"));
                    return MiniBufferSelection::Cancelled;
                }
            };

        self.prompt_w_callback(prompt, initial_lines, None, |_| None)
    }
}

/// Something that can be used to open a minibuffer and run subsequent actions based on
/// a selection.
pub(crate) trait MbSelect: Send + Sync {
    fn clone_selector(&self) -> MbSelector;
    fn prompt_and_options(&self, buffers: &Buffers) -> (String, Vec<String>);
    fn selected_actions(&self, sel: MiniBufferSelection) -> Option<Actions>;

    #[allow(unused_variables)]
    fn initial_input(&self, buffers: &Buffers) -> Option<String> {
        None
    }

    fn into_selector(self) -> MbSelector
    where
        Self: Sized + 'static,
    {
        MbSelector(Box::new(self))
    }
}

pub struct MbSelector(Box<dyn MbSelect>);

impl fmt::Debug for MbSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MbSelector").finish()
    }
}

impl Clone for MbSelector {
    fn clone(&self) -> Self {
        self.0.clone_selector()
    }
}

impl cmp::Eq for MbSelector {}
impl cmp::PartialEq for MbSelector {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl MbSelector {
    pub(crate) fn run<S>(&self, ed: &mut Editor<S>)
    where
        S: System,
    {
        let (prompt, options) = self.0.prompt_and_options(ed.layout.buffers());
        let initial_input = self.0.initial_input(ed.layout.buffers());
        let selection = ed.prompt_w_callback(&prompt, options, initial_input, |_| None);
        if let Some(actions) = self.0.selected_actions(selection) {
            ed.handle_actions(actions, Source::Fsys);
        }
    }
}
