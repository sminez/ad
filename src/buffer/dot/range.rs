use crate::buffer::dot::Cur;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range {
    pub start: Cur,
    pub end: Cur,
}

impl Range {
    /// Construct a new range from two cursors, ensuring that their are
    /// correctly ordered.
    pub fn from_cursors(c1: Cur, c2: Cur) -> Self {
        let (start, end) = if c1 < c2 { (c1, c2) } else { (c2, c1) };

        Self { start, end }
    }

    pub(crate) fn line_ranges(&self) -> Vec<LineRange> {
        let n_lines = self.start.y - self.end.y + 1;
        if n_lines == 1 {
            vec![LineRange::Partial {
                y: self.start.y,
                start: self.start.x,
                end: self.end.x,
            }]
        } else {
            let mut lrs = Vec::with_capacity(n_lines);
            lrs.push(LineRange::ToEnd {
                y: self.start.y,
                start: self.start.x,
            });

            for y in (self.start.y + 1)..self.end.y {
                lrs.push(LineRange::Full { y });
            }

            lrs.push(LineRange::FromStart {
                y: self.end.y,
                end: self.end.x,
            });

            lrs
        }
    }
}

/// A selection of characters within a single line
pub(crate) enum LineRange {
    Full { y: usize },
    ToEnd { y: usize, start: usize },
    FromStart { y: usize, end: usize },
    Partial { y: usize, start: usize, end: usize },
}
