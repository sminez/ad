use crate::{
    buffer::{Buffer, Line},
    key::Arrow,
};
use std::cmp::Ordering;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cur {
    pub y: usize,
    pub x: usize,
}

impl Cur {
    pub fn buffer_start() -> Self {
        Cur { y: 0, x: 0 }
    }

    pub fn buffer_end(b: &Buffer) -> Self {
        Cur {
            y: b.lines.len(),
            x: 0,
        }
    }

    pub(super) fn arr_w_count(&self, arr: Arrow, count: usize, b: &Buffer) -> Self {
        let mut cur = *self;

        for _ in 0..count {
            cur = cur.arr(arr, b);
        }

        cur.clamp_x(b);
        cur
    }

    #[must_use]
    pub(super) fn move_to_line_start(mut self) -> Self {
        self.x = 0;
        self
    }

    #[must_use]
    pub(super) fn move_to_line_end(mut self, b: &Buffer) -> Self {
        self.x = b.lines.get(self.y).map(|l| l.len()).unwrap_or_default();
        self
    }

    /// Move forward until cond returns an x position in the given line or we bottom out at the end of the buffer
    #[must_use]
    pub(super) fn move_to(mut self, b: &Buffer, cond: fn(&Line) -> Option<usize>) -> Self {
        for line in b.lines.iter().skip(self.y + 1) {
            self.y += 1;
            if let Some(x) = (cond)(line) {
                self.x = x;
                return self;
            }
        }
        self.move_to_line_end(b)
    }

    /// Move back until cond returns an x position in the given line or we bottom out at the start of the buffer
    #[must_use]
    pub(super) fn move_back_to(mut self, b: &Buffer, cond: fn(&Line) -> Option<usize>) -> Self {
        for line in b.lines.iter().take(self.y).rev() {
            self.y -= 1;
            if let Some(x) = (cond)(line) {
                self.x = x;
                return self;
            }
        }
        self.move_to_line_start()
    }

    fn arr(&self, arr: Arrow, b: &Buffer) -> Self {
        let mut cur = *self;

        match arr {
            Arrow::Up => {
                if cur.y != 0 {
                    cur.y -= 1;
                    cur.set_x_from_buffer_rx(b);
                }
            }
            Arrow::Down => {
                if !b.lines.is_empty() && cur.y < b.lines.len() - 1 {
                    cur.y += 1;
                    cur.set_x_from_buffer_rx(b);
                }
            }
            Arrow::Left => {
                if cur.x != 0 {
                    cur.x -= 1;
                } else if cur.y > 0 {
                    // Allow <- to move to the end of the previous line
                    cur.y -= 1;
                    cur.x = b.lines[cur.y].len();
                }
            }
            Arrow::Right => {
                if let Some(line) = b.line(cur.y) {
                    match cur.x.cmp(&line.len()) {
                        Ordering::Less => cur.x += 1,
                        Ordering::Equal => {
                            // Allow -> to move to the start of the next line
                            cur.y += 1;
                            cur.x = 0;
                        }
                        _ => (),
                    }
                }
            }
        }

        cur
    }

    fn clamp_x(&mut self, b: &Buffer) {
        let len = if self.y >= b.len_lines() {
            0
        } else {
            b.lines[self.y].len()
        };

        if self.x > len {
            self.x = len;
        }
    }

    fn set_x_from_buffer_rx(&mut self, b: &Buffer) {
        self.x = b.x_from_rx(self.y);
    }
}
