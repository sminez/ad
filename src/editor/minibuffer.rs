//! A transient buffer for handling interactive input from the user without
//! modifying the current buffer state.
//!
//! Conceptually this is operates as an embedded dmenu.
use crate::{
    Config,
    buffer::{Buffer, Buffers, GapBuffer, Slice},
    dot::TextObject,
    editor::{Action, Actions, Editor},
    input::Event,
    key::{Arrow, Input},
    system::System,
};
use ad_event::Source;
use std::{
    cmp::{self, min},
    fmt,
    ops::ControlFlow,
    sync::{Arc, Mutex, RwLock},
};

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

impl MiniBufferSelection {
    /// Disgard any information around which line was selected by the user and only
    /// return the content of the selected line itself.
    pub(crate) fn into_content(self) -> Option<String> {
        match self {
            Self::Line { line, .. } => Some(line),
            Self::UserInput { input } => Some(input),
            Self::Cancelled => None,
        }
    }
}

/// A mini-buffer always has a single line prompt for accepting user input
/// with the rest of the buffer content not being directly editable.
///
/// Conceptually this is operates as an embedded dmenu.
pub(crate) struct MiniBuffer {
    sel: MbSelector,
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

impl fmt::Debug for MiniBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MiniBuffer")
            .field("prompt", &self.prompt)
            .field("input", &self.input)
            .finish()
    }
}

impl MiniBuffer {
    pub fn new(sel: MbSelector, config: Arc<RwLock<Config>>, buffers: &Buffers) -> Self {
        let (prompt, options) = sel.0.prompt_and_options(buffers);
        let initial_input = sel.0.initial_input(buffers).unwrap_or_default();

        let line_indices = Vec::with_capacity(options.len());
        let n_prompt_chars = prompt.chars().count();
        let max_height = config.read().unwrap().minibuffer_lines;

        Self {
            sel,
            prompt,
            n_prompt_chars,
            input: Buffer::new_unnamed(MINIBUFFER_ID, initial_input, config.clone()),
            initial_lines: options,
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

    pub fn handle_input(&mut self, inputs: Vec<Input>) -> ControlFlow<Option<Actions>> {
        for input in inputs.into_iter() {
            if let Some(selection) = self.handle_input_one(input) {
                return ControlFlow::Break(self.sel.0.selected_actions(selection));
            }
        }

        ControlFlow::Continue(())
    }

    pub fn updated_render_state(&mut self) -> MiniBufferState<'_> {
        self.update_state();

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

    fn handle_input_one(&mut self, input: Input) -> Option<MiniBufferSelection> {
        match input {
            Input::Char(c) => {
                self.input
                    .handle_action(Action::InsertChar { c }, Source::Keyboard);
            }
            Input::Ctrl('h') | Input::Backspace | Input::Del => {
                for action in [
                    Action::DotSet(TextObject::Arr(Arrow::Left), 1),
                    Action::Delete,
                ] {
                    self.input.handle_action(action, Source::Keyboard);
                }
            }

            // Readline style bindings
            Input::Ctrl('a') => {
                self.input
                    .handle_action(Action::DotSet(TextObject::LineStart, 1), Source::Keyboard);
            }
            Input::Ctrl('e') => {
                self.input
                    .handle_action(Action::DotSet(TextObject::LineEnd, 1), Source::Keyboard);
            }
            Input::Ctrl('w') => {
                for action in [
                    Action::DotSet(TextObject::Arr(Arrow::Left), 1),
                    Action::DotExtendBackward(TextObject::Word, 1),
                    Action::Delete,
                ] {
                    self.input.handle_action(action, Source::Keyboard);
                }
            }

            // Esc / Enter to cancel and accept
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

            // Alt-hjkl and arrows navigate the options
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
    /// Push a new minibuffer onto the minibuffer stack.
    ///
    /// This minibuffer will be responsible for processing input events until it returns a
    /// selection, at which point the minibuffer below it will resume processing.
    pub(crate) fn push_minibuffer(&mut self, sel: MbSelector) {
        let mb = MiniBuffer::new(sel, self.config.clone(), self.layout.buffers());
        self.mb_stack.push(mb);
    }

    /// If we have an open minibuffer then it is responsible for handling input and bracketed paste
    /// events. All other events are handled by the main event loop even when a minibuffer is open.
    pub(crate) fn try_handle_event_with_minibuffer(&mut self, event: Event) -> Option<Event> {
        if self.mb_stack.is_empty() {
            return Some(event);
        }

        let inputs = match event {
            Event::Input(i) => vec![i],
            Event::BracketedPaste(s) => s.chars().map(Input::Char).collect(),
            event => return Some(event),
        };

        let mb = self.mb_stack.last_mut().expect("checked non-empty above");
        if let ControlFlow::Break(maybe_actions) = mb.handle_input(inputs) {
            self.mb_stack.pop();
            self.handle_actions(maybe_actions?, Source::Fsys);
        }

        None
    }
}

/// Something that can be used to open a minibuffer and run subsequent actions based on
/// a selection.
pub(crate) trait MbSelect: Send + Sync {
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
        MbSelector(Arc::new(self))
    }
}

#[derive(Clone)]
pub struct MbSelector(Arc<dyn MbSelect>);

impl fmt::Debug for MbSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MbSelector").finish_non_exhaustive()
    }
}

impl cmp::Eq for MbSelector {}
impl cmp::PartialEq for MbSelector {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

pub(crate) struct SimpleMbSelect {
    prompt: String,
    lines: Vec<String>,
    selected_actions: Mutex<Box<dyn FnMut(MiniBufferSelection) -> Option<Actions> + Send + Sync>>,
}

impl fmt::Debug for SimpleMbSelect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SimpleMbSelect")
            .field("prompt", &self.prompt)
            .field("lines", &self.lines)
            .finish_non_exhaustive()
    }
}

impl SimpleMbSelect {
    pub fn new<F>(prompt: impl Into<String>, lines: Vec<String>, selected_actions: F) -> Self
    where
        F: FnMut(MiniBufferSelection) -> Option<Actions> + Send + Sync + 'static,
    {
        Self {
            prompt: prompt.into(),
            lines,
            selected_actions: Mutex::new(Box::new(selected_actions)),
        }
    }
}

impl MbSelect for SimpleMbSelect {
    fn prompt_and_options(&self, _: &Buffers) -> (String, Vec<String>) {
        (self.prompt.clone(), self.lines.clone())
    }

    fn selected_actions(&self, selection: MiniBufferSelection) -> Option<Actions> {
        let mut f = self.selected_actions.lock().unwrap();

        (f)(selection)
    }
}
