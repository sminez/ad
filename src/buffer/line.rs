use crate::TAB_STOP;

fn as_render_line(line: &str) -> String {
    line.replace('\t', &" ".repeat(TAB_STOP))
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Line {
    // The raw characters as they will be stored on disk
    pub(crate) raw: String,
    // A cache of the rendered string content for the terminal
    pub(crate) render: String,
}

impl Line {
    pub(crate) fn new(raw: String) -> Self {
        let render = as_render_line(&raw);
        Self { raw, render }
    }

    pub(super) fn update_render(&mut self) {
        self.render = as_render_line(&self.raw);
    }

    pub(super) fn modify<F: Fn(&mut String)>(&mut self, f: F) {
        (f)(&mut self.raw);
        self.update_render();
    }

    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn len(&self) -> usize {
        self.raw.len()
    }
}
