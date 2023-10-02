use crate::buffer::dot::Cur;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range {
    pub start: Cur,
    pub end: Cur,
    pub(super) start_active: bool,
}

impl Range {
    pub fn from_cursors(c1: Cur, c2: Cur, c1_was_active: bool) -> Self {
        let (start, end, start_active) = if c1 <= c2 {
            (c1, c2, c1_was_active)
        } else if c1_was_active {
            (c2, c1, false)
        } else {
            (c2, c1, true)
        };

        Self {
            start,
            end,
            start_active,
        }
    }

    pub fn flip(&mut self) {
        self.start_active = !self.start_active;
    }

    pub fn active_cursor(&self) -> Cur {
        if self.start_active {
            self.start
        } else {
            self.end
        }
    }

    pub(crate) fn line_range(&self, y: usize) -> Option<LineRange> {
        if y == self.start.y {
            if self.start.y == self.end.y {
                Some(LineRange::Partial {
                    y: self.start.y,
                    start: self.start.x,
                    end: self.end.x,
                })
            } else {
                Some(LineRange::ToEnd {
                    y: self.start.y,
                    start: self.start.x,
                })
            }
        } else if y > self.start.y && y < self.end.y {
            Some(LineRange::Full { y })
        } else if y == self.end.y {
            Some(LineRange::FromStart {
                y: self.end.y,
                end: self.end.x,
            })
        } else {
            None
        }
    }

    pub(crate) fn line_ranges(&self) -> Vec<LineRange> {
        let n_lines = self.end.y - self.start.y + 1;
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LineRange {
    Full { y: usize },
    ToEnd { y: usize, start: usize },
    FromStart { y: usize, end: usize },
    Partial { y: usize, start: usize, end: usize },
}

#[cfg(test)]
mod tests {
    use super::{LineRange::*, *};
    use simple_test_case::test_case;

    #[test_case(0, None; "before range")]
    #[test_case(1, Some(LineRange::ToEnd { y: 1, start: 10 }); "first line")]
    #[test_case(2, Some(LineRange::Full { y: 2 }); "full line")]
    #[test_case(3, Some(LineRange::FromStart { y: 3, end: 5 }); "last line")]
    #[test_case(5, None; "after range")]
    #[test]
    fn line_range_works_for_multi_line_range(y: usize, expected: Option<LineRange>) {
        let r = Range {
            start: Cur { y: 1, x: 10 },
            end: Cur { y: 3, x: 5 },
            start_active: true,
        };

        assert_eq!(r.line_range(y), expected);
    }

    #[test_case(0, None; "before range")]
    #[test_case(1, Some(LineRange::Partial { y: 1, start: 5, end: 10 }); "partial line")]
    #[test_case(2, None; "after range")]
    #[test]
    fn line_range_works_for_single_line_range(y: usize, expected: Option<LineRange>) {
        let r = Range {
            start: Cur { y: 1, x: 5 },
            end: Cur { y: 1, x: 10 },
            start_active: true,
        };

        assert_eq!(r.line_range(y), expected);
    }

    #[test_case(
        Range { start: Cur { y: 1, x: 5 }, end: Cur { y: 1, x: 10 }, start_active: true },
        vec![Partial { y: 1, start: 5, end: 10 }];
        "single-line"
    )]
    #[test_case(
        Range { start: Cur { y: 1, x: 10 }, end: Cur { y: 4, x: 5 }, start_active: true },
        vec![ToEnd { y: 1, start: 10 }, Full { y: 2 }, Full { y: 3 }, FromStart { y: 4, end: 5 }];
        "multi-line"
    )]
    #[test]
    fn line_ranges_works(r: Range, expected: Vec<LineRange>) {
        assert_eq!(r.line_ranges(), expected);
    }
}
