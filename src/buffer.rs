use crate::key::{Arrow, Key};
use std::{fs::read_to_string, io};

#[derive(Default)]
pub struct Buffer {
    pub(crate) path: Option<String>,
    pub(crate) lines: Vec<String>,
    pub(crate) cx: usize,
    pub(crate) cy: usize,
    pub(crate) row_off: usize,
    pub(crate) col_off: usize,
}

impl Buffer {
    pub fn new_from_file(path: &str) -> io::Result<Self> {
        let raw = read_to_string(path)?;
        let lines: Vec<String> = raw.lines().map(String::from).collect();

        Ok(Self {
            path: Some(path.to_string()),
            lines,
            cx: 0,
            cy: 0,
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
        if self.cy < self.row_off {
            self.row_off = self.cy;
        }

        if self.cy >= self.row_off + screen_rows {
            self.row_off = self.cy - screen_rows + 1;
        }

        if self.cx < self.col_off {
            self.col_off = self.cx;
        }

        if self.cx >= self.col_off + screen_cols {
            self.col_off = self.cx - screen_cols + 1;
        }
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
            Key::Arrow(arr) => self.move_cursor(arr, screen_rows, screen_cols),
            Key::Home => self.cx = 0,
            Key::End => self.cx = screen_cols - 1,
            Key::PageUp | Key::PageDown => {
                let arr = if k == Key::PageUp {
                    Arrow::Up
                } else {
                    Arrow::Down
                };

                for _ in 0..screen_rows {
                    self.move_cursor(arr, screen_rows, screen_cols);
                }
            }

            _ => (),
        }

        Ok(())
    }

    fn move_cursor(&mut self, arr: Arrow, screen_rows: usize, screen_cols: usize) {
        match arr {
            Arrow::Up => {
                if self.cy != 0 {
                    self.cy -= 1;
                }
            }
            Arrow::Down => {
                if self.cy != screen_rows - 1 {
                    self.cy += 1;
                }
            }
            Arrow::Left => {
                if self.cx != 0 {
                    self.cx -= 1;
                }
            }
            Arrow::Right => {
                if self.cx != screen_cols - 1 {
                    self.cx += 1;
                }
            }
        }
    }
}
