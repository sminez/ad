//! Vim style text objects
use crate::{
    buffer::Buffer,
    dot::{
        find::{find_backward_start, find_forward_end, Find},
        Cur, Dot, Range,
    },
    key::Arrow,
};

/// A vim-like text object which can be used to manipulate the current Dot in a Buffer
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum TextObject {
    Arr(Arrow),
    BufferEnd,
    BufferStart,
    Character,
    FindChar(char),
    Delimited(char, char),
    Line,
    LineEnd,
    LineStart,
    Paragraph,
    Word,
}

impl TextObject {
    pub fn set_dot(&self, b: &mut Buffer) {
        use TextObject::*;

        let dot = match self {
            Arr(arr) => b.dot.active_cur().arr(*arr, b).into(),
            BufferEnd => Cur::buffer_end(b).into(),
            BufferStart => Cur::buffer_start().into(),
            Character => b.dot.active_cur().arr(Arrow::Right, b).into(),
            FindChar(ch) => find_forward_end(ch, b.dot.active_cur(), b).into(),
            Delimited(l, r) => FindDelimited::new(*l, *r).expand(b.dot, b),
            LineEnd => b.dot.active_cur().move_to_line_end(b).into(),
            LineStart => b.dot.active_cur().move_to_line_start(b).into(),
            Line => Dot::from(
                b.dot
                    .as_range()
                    .extend_to_line_start(b)
                    .extend_to_line_end(b),
            )
            .collapse_null_range(),
            Paragraph => FindParagraph::Fwd.expand(b.dot, b),
            Word => FindWord::Fwd.expand(b.dot, b),
        };

        b.dot = dot;
    }

    pub fn extend_dot_forward(&self, b: &mut Buffer) {
        use TextObject::*;

        let Range {
            mut start,
            mut end,
            start_active,
        } = b.dot.as_range();

        (start, end) = match (self, start_active) {
            (Arr(arr), _) => (start, end.arr(*arr, b)),
            (BufferEnd, true) => (end, Cur::buffer_end(b)),
            (BufferEnd, false) => (start, Cur::buffer_end(b)),
            (Character, true) => (start.arr_w_count(Arrow::Right, 1, b), end),
            (Character, false) => (start, end.arr_w_count(Arrow::Right, 1, b)),
            (FindChar(ch), true) => (find_forward_end(ch, start, b), end),
            (FindChar(ch), false) => (start, find_forward_end(ch, end, b)),
            (Line, true) => (start.arr_w_count(Arrow::Down, 1, b), end),
            (Line, false) => (start, end.arr_w_count(Arrow::Down, 1, b)),
            (LineEnd, true) => (start.move_to_line_end(b), end),
            (LineEnd, false) => (start, end.move_to_line_end(b)),
            (LineStart, true) => (start.move_to_line_start(b), end),
            (LineStart, false) => (start, end.move_to_line_start(b)),
            (Paragraph, true) => (find_forward_end(&FindParagraph::Fwd, start, b), end),
            (Paragraph, false) => (start, find_forward_end(&FindParagraph::Fwd, end, b)),
            (Word, true) => (find_forward_end(&FindWord::Fwd, start, b), end),
            (Word, false) => (start, find_forward_end(&FindWord::Fwd, end, b)),
            // Can't move forward to the buffer start or move forward between delimiters
            (BufferStart | Delimited(_, _), _) => return,
        };

        b.dot = Dot::from(Range::from_cursors(start, end, start_active)).collapse_null_range();
    }

    pub fn extend_dot_backward(&self, b: &mut Buffer) {
        use TextObject::*;

        let Range {
            mut start,
            mut end,
            start_active,
        } = b.dot.as_range();

        (start, end) = match (self, start_active) {
            (Arr(arr), _) => (start.arr(arr.flip(), b), end),
            (BufferStart, true) => (Cur::buffer_start(), end),
            (BufferStart, false) => (Cur::buffer_start(), start),
            (Character, true) => (start.arr_w_count(Arrow::Left, 1, b), end),
            (Character, false) => (start, end.arr_w_count(Arrow::Left, 1, b)),
            (FindChar(ch), true) => (find_backward_start(ch, start, b), end),
            (FindChar(ch), false) => (start, find_backward_start(ch, end, b)),
            (Line, true) => (start.arr_w_count(Arrow::Up, 1, b), end),
            (Line, false) => (start, end.arr_w_count(Arrow::Up, 1, b)),
            (LineEnd, true) => (start.move_to_line_end(b), end),
            (LineEnd, false) => (start, end.move_to_line_end(b)),
            (LineStart, true) => (start.move_to_line_start(b), end),
            (LineStart, false) => (start, end.move_to_line_start(b)),
            (Paragraph, true) => (find_backward_start(&FindParagraph::Fwd, start, b), end),
            (Paragraph, false) => (start, find_backward_start(&FindParagraph::Fwd, end, b)),
            (Word, true) => (find_backward_start(&FindWord::Fwd, start, b), end),
            (Word, false) => (start, find_backward_start(&FindWord::Fwd, end, b)),
            // Can't move back to the buffer end or move back between delimiters
            (BufferEnd | Delimited(_, _), _) => return,
        };

        b.dot = Dot::from(Range::from_cursors(start, end, start_active)).collapse_null_range();
    }
}

pub struct FindDelimited {
    l: String,
    r: String,
    rev: bool,
}

impl FindDelimited {
    pub fn new(l: impl Into<String>, r: impl Into<String>) -> Self {
        Self {
            l: l.into(),
            r: r.into(),
            rev: false,
        }
    }
}

impl Find for FindDelimited {
    type Reversed = FindDelimited;

    fn reversed(&self) -> Self::Reversed {
        Self {
            l: self.l.clone(),
            r: self.r.clone(),
            rev: !self.rev,
        }
    }

    fn try_find<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>,
    {
        let (target, other) = if self.rev {
            (&self.l, &self.r)
        } else {
            (&self.r, &self.l)
        };
        let mut skips = 0;

        for (i, ch) in it {
            if other.contains(ch) && target != other {
                skips += 1;
            } else if skips == 0 && target.contains(ch) {
                let ix = if self.rev { i + 1 } else { i - 1 };
                return Some((ix, ix));
            } else if target.contains(ch) {
                skips -= 1;
            }
        }

        None
    }
}

enum FindParagraph {
    Fwd,
    Bck,
}

impl Find for FindParagraph {
    type Reversed = FindParagraph;

    fn reversed(&self) -> Self::Reversed {
        match self {
            Self::Fwd => Self::Bck,
            Self::Bck => Self::Fwd,
        }
    }

    fn try_find<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>,
    {
        let mut prev_was_newline = false;
        let mut pos = 0;

        for (i, ch) in it {
            match ch {
                '\n' if prev_was_newline => {
                    return match self {
                        Self::Fwd => Some((i, i)),
                        Self::Bck => Some((i + 1, i + 1)),
                    }
                }
                '\n' => prev_was_newline = true,
                _ => prev_was_newline = false,
            }
            pos = i;
        }

        Some((pos, pos))
    }
}

enum FindWord {
    Fwd,
    Bck,
}

impl Find for FindWord {
    type Reversed = FindWord;

    fn reversed(&self) -> Self::Reversed {
        match self {
            Self::Fwd => Self::Bck,
            Self::Bck => Self::Fwd,
        }
    }

    fn try_find<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>,
    {
        use CharKind::*;

        let mut it = it.peekable();
        let mut prev = CharKind::from(it.peek()?.1);

        // If we are searching forward and are not currently sat on whitespace then we could be
        // on the end of a word which would cause us to stick in place, so we advance a single
        // character and start the search from there.
        if matches!((prev, self), (Word | Punctuation, FindWord::Fwd)) {
            it.next();
            prev = CharKind::from(it.peek()?.1);
        }

        for (i, ch) in it {
            let kind = CharKind::from(ch);
            match (prev, kind) {
                (Word, Punctuation) | (Punctuation, Word) | (Word | Punctuation, Whitespace) => {
                    return match self {
                        Self::Fwd => Some((i - 1, i - 1)),
                        Self::Bck => Some((i + 1, i + 1)),
                    }
                }
                _ => prev = kind,
            }
        }

        None
    }

    fn expand(&self, dot: Dot, b: &Buffer) -> Dot {
        use CharKind::*;

        let Range {
            mut start,
            mut end,
            start_active,
        } = dot.as_range();
        let max_idx = b.txt.len_chars() - 1;

        if start.idx > 0 {
            let current = CharKind::from(b.txt.char(start.idx));
            let prev = CharKind::from(b.txt.char(start.idx - 1));

            match (prev, current) {
                // We're at the start of the current word so start.idx is correct
                (Whitespace, Word | Punctuation) => (),

                // We're in whitespace so advance until we hit a word or the end of the buffer
                (_, Whitespace) if start.idx < max_idx => {
                    while matches!(CharKind::from(b.txt.char(start.idx)), Whitespace) {
                        start.idx += 1;
                        if start.idx == max_idx {
                            end.idx = max_idx;
                            break;
                        }
                    }
                }

                // We're in a word so search back to find the start
                _ => start = find_backward_start(self, start, b),
            }
        }

        if end.idx < max_idx {
            let current = CharKind::from(b.txt.char(end.idx));
            let next = CharKind::from(b.txt.char(end.idx + 1));

            match (current, next) {
                // We're at the end of a word so end.idx is correct
                (Word | Punctuation, Whitespace) => (),

                // We're within a word or in whitespace so advance to find the end of the word
                _ => end = find_forward_end(self, end, b),
            }
        }

        Dot::from(Range::from_cursors(start, end, start_active)).collapse_null_range()
    }
}

#[derive(Debug, Clone, Copy)]
enum CharKind {
    Word,
    Punctuation,
    Whitespace,
}

impl From<char> for CharKind {
    fn from(ch: char) -> Self {
        if ch.is_alphanumeric() || ch == '_' {
            CharKind::Word
        } else if ch.is_whitespace() {
            CharKind::Whitespace
        } else {
            CharKind::Punctuation
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test_case(FindWord::Fwd, 0, "this"; "forward start of buffer")]
    #[test_case(FindWord::Fwd, 3, "this"; "forward end of first word")]
    #[test_case(FindWord::Fwd, 4, "is"; "forward after first word")]
    #[test_case(FindWord::Fwd, 5, "is"; "forward start of second word")]
    #[test_case(FindWord::Fwd, 6, "is"; "forward end of second word")]
    #[test_case(FindWord::Fwd, 9, "test"; "forward before last word")]
    #[test_case(FindWord::Fwd, 13, "test"; "forward end of buffer")]
    #[test]
    fn expand_word(fw: FindWord, idx: usize, expected: &str) {
        let b = Buffer::new_virtual(0, "test", "this is a test");
        let dot = Dot::Cur { c: Cur { idx } };
        let expanded = fw.expand(dot, &b);
        let content = expanded.content(&b);

        assert_eq!(content, expected);
    }

    #[test]
    fn expand_word_for_buffer_with_trailing_spaces() {
        let b = Buffer::new_virtual(0, "test", "this is a test   ");
        let dot = Dot::Cur { c: Cur { idx: 14 } };
        let expanded = FindWord::Fwd.expand(dot, &b);
        let content = expanded.content(&b);

        // Should have advanced to the end of the buffer and selected that final space character
        assert!(matches!(expanded, Dot::Cur { c: Cur { idx: 16 } }));
        assert_eq!(content, " ");
    }
}
