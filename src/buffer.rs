use crate::{
    key::{Arrow, Key},
    TAB_STOP,
};
use std::{cmp::Ordering, fs::read_to_string, io};

#[derive(Default)]
pub struct Buffer {
    pub(crate) path: Option<String>,
    pub(crate) lines: Vec<String>,
    pub(crate) render_lines: Vec<String>,
    pub(crate) cx: usize,
    pub(crate) cy: usize,
    pub(crate) rx: usize,
    pub(crate) row_off: usize,
    pub(crate) col_off: usize,
}

fn as_render_lines(lines: &[String]) -> Vec<String> {
    lines
        .iter()
        .map(|line| line.replace('\t', &" ".repeat(TAB_STOP)))
        .collect()
}

impl Buffer {
    pub fn new_from_file(path: &str) -> io::Result<Self> {
        let raw = read_to_string(path)?;
        let lines: Vec<String> = raw.lines().map(String::from).collect();
        let render_lines = as_render_lines(&lines);

        Ok(Self {
            path: Some(path.to_string()),
            lines,
            render_lines,
            cx: 0,
            cy: 0,
            rx: 0,
            row_off: 0,
            col_off: 0,
        })
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.lines.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }

    pub fn clamp_scroll(&mut self, screen_rows: usize, screen_cols: usize) {
        self.rx = 0;
        if self.cy < self.lines.len() {
            self.update_rx();
        }

        if self.cy < self.row_off {
            self.row_off = self.cy;
        }

        if self.cy >= self.row_off + screen_rows {
            self.row_off = self.cy - screen_rows + 1;
        }

        if self.rx < self.col_off {
            self.col_off = self.rx;
        }

        if self.rx >= self.col_off + screen_cols {
            self.col_off = self.rx - screen_cols + 1;
        }
    }

    fn update_rx(&mut self) {
        let mut rx = 0;

        for c in self.lines[self.cy].chars().take(self.cx) {
            if c == '\t' {
                rx += (TAB_STOP - 1) - (rx % TAB_STOP);
            }
            rx += 1;
        }

        self.rx = rx;
    }

    pub fn clamp_cx(&mut self) {
        let len = if self.cy >= self.len() {
            0
        } else {
            self.lines[self.cy].len()
        };

        if self.cx > len {
            self.cx = len;
        }
    }

    pub fn current_line(&self) -> Option<&str> {
        if self.cy >= self.len() {
            None
        } else {
            Some(&self.lines[self.cy])
        }
    }

    pub fn handle_keypress(
        &mut self,
        k: Key,
        screen_rows: usize,
        screen_cols: usize,
    ) -> io::Result<()> {
        match k {
            Key::Arrow(arr) => self.move_cursor(arr),
            Key::Home => self.cx = 0,
            Key::End => {
                self.cx = screen_cols - 1;
                self.clamp_cx();
            }
            Key::PageUp | Key::PageDown => {
                let arr = if k == Key::PageUp {
                    Arrow::Up
                } else {
                    Arrow::Down
                };

                for _ in 0..screen_rows {
                    self.move_cursor(arr);
                }
            }

            _ => (),
        }

        Ok(())
    }

    fn move_cursor(&mut self, arr: Arrow) {
        match arr {
            Arrow::Up => {
                if self.cy != 0 {
                    self.cy -= 1;
                }
            }
            Arrow::Down => {
                if self.cy < self.lines.len() {
                    self.cy += 1;
                }
            }
            Arrow::Left => {
                if self.cx != 0 {
                    self.cx -= 1;
                } else if self.cy > 0 {
                    // Allow <- to move to the end of the previous line
                    self.cy -= 1;
                    self.cx = self.lines[self.cy].len();
                }
            }
            Arrow::Right => {
                if let Some(line) = self.current_line() {
                    match self.cx.cmp(&line.len()) {
                        Ordering::Less => self.cx += 1,
                        Ordering::Equal => {
                            // Allow -> to move to the start of the next line
                            self.cy += 1;
                            self.cx = 0;
                        }
                        _ => (),
                    }
                }
            }
        }

        self.clamp_cx();
    }
}
