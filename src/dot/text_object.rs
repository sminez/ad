//! Vim style text objects
use crate::{
    buffer::{Buffer, CharIdx},
    dot::{
        Cur, Dot, Range,
        find::{Find, find_backward, find_backward_start, find_forward, find_forward_end},
    },
    key::Arrow,
};
use std::cmp::min;

/// A vim-like text object which can be used to manipulate the current Dot in a Buffer
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TextObject {
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
    pub fn as_dot(&self, b: &Buffer) -> Dot {
        use TextObject::*;

        match self {
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
        }
    }

    pub fn set_dot(&self, b: &mut Buffer) {
        b.dot = self.as_dot(b);
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
    l: char,
    r: char,
    rev: bool,
}

impl FindDelimited {
    pub fn new(l: char, r: char) -> Self {
        Self { l, r, rev: false }
    }

    fn expand(&self, dot: Dot, b: &Buffer) -> Dot
    where
        Self: Sized,
    {
        let Range {
            mut start,
            mut end,
            start_active,
        } = dot.as_range();

        // Special case starting on either delimiter as we know we are only one character away
        let new_start = if dot.collapse_to_first_cur().content(b).contains(self.l) {
            start.idx = start.idx.uadd(1);
            Some(start)
        } else {
            find_backward(self, end, b).map(|d| d.last_cur())
        };

        let new_end = if dot.collapse_to_last_cur().content(b).contains(self.r) {
            end.idx = end.idx.usub(1);
            Some(end)
        } else {
            find_forward(self, start, b).map(|d| d.first_cur())
        };

        // If either search failed to find the delimiter it needed we leave dot unchanged
        if let (Some(new_start), Some(new_end)) = (new_start, new_end) {
            Dot::from(Range::from_cursors(new_start, new_end, start_active)).collapse_null_range()
        } else {
            dot
        }
    }
}

impl Find for FindDelimited {
    type Reversed = FindDelimited;

    fn reversed(&self) -> Self::Reversed {
        Self {
            l: self.l,
            r: self.r,
            rev: !self.rev,
        }
    }

    fn try_find<I>(&self, it: I) -> Option<(CharIdx, CharIdx)>
    where
        I: Iterator<Item = (CharIdx, char)>,
    {
        let (target, other) = if self.rev {
            (self.l, self.r)
        } else {
            (self.r, self.l)
        };
        let mut skips = 0;

        for (i, ch) in it {
            if other == ch && target != other {
                skips += 1;
            } else if skips == 0 && target == ch {
                let ix = if self.rev { i.uadd(1) } else { i.usub(1) };
                return Some((ix, ix));
            } else if target == ch {
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

impl FindParagraph {
    fn expand(&self, dot: Dot, b: &Buffer) -> Dot
    where
        Self: Sized,
    {
        let Range {
            start,
            end,
            start_active,
        } = dot.as_range();

        let new_start = find_backward(self, start, b).map(|d| d.last_cur());
        let new_end = find_forward(self, end, b).map(|d| d.first_cur());

        // If either search failed to find the delimiter it needed we leave dot unchanged
        if let (Some(new_start), Some(new_end)) = (new_start, new_end) {
            Dot::from(Range::from_cursors(new_start, new_end, start_active)).collapse_null_range()
        } else {
            dot
        }
    }
}

impl Find for FindParagraph {
    type Reversed = FindParagraph;

    fn reversed(&self) -> Self::Reversed {
        match self {
            Self::Fwd => Self::Bck,
            Self::Bck => Self::Fwd,
        }
    }

    fn try_find<I>(&self, mut it: I) -> Option<(CharIdx, CharIdx)>
    where
        I: Iterator<Item = (CharIdx, char)>,
    {
        let mut prev_was_newline = it.next()?.1 == '\n';

        for (i, ch) in it {
            match ch {
                '\n' if prev_was_newline => {
                    return match self {
                        Self::Fwd => Some((i, i)),
                        Self::Bck => Some((i.uadd(1), i.uadd(1))),
                    };
                }
                '\n' => prev_was_newline = true,
                _ => prev_was_newline = false,
            }
        }

        None
    }
}

enum FindWord {
    Fwd,
    Bck,
}

impl FindWord {
    fn expand(&self, dot: Dot, b: &Buffer) -> Dot {
        use CharKind::*;

        if b.is_empty() {
            return dot;
        }

        let Range {
            mut start,
            mut end,
            start_active,
        } = dot.as_range();

        let max_idx = CharIdx(b.txt.len_chars() - 1);
        start.idx = min(start.idx, max_idx);
        end.idx = min(end.idx, max_idx);

        if start.idx > CharIdx(0) {
            let current = CharKind::from(b.txt.char(start.idx));
            let prev = CharKind::from(b.txt.char(start.idx.usub(1)));

            match (prev, current) {
                // We're at the start of the current word so start.idx is correct
                (Whitespace, Word | Punctuation) => (),

                // We're in whitespace so advance until we hit a word or the end of the buffer
                (_, Whitespace) if start.idx < max_idx => {
                    while matches!(CharKind::from(b.txt.char(start.idx)), Whitespace) {
                        start.idx = start.idx.uadd(1);
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
            let next = CharKind::from(b.txt.char(end.idx.uadd(1)));

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

impl Find for FindWord {
    type Reversed = FindWord;

    fn reversed(&self) -> Self::Reversed {
        match self {
            Self::Fwd => Self::Bck,
            Self::Bck => Self::Fwd,
        }
    }

    fn try_find<I>(&self, it: I) -> Option<(CharIdx, CharIdx)>
    where
        I: Iterator<Item = (CharIdx, char)>,
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
                        Self::Fwd => Some((i.usub(1), i.usub(1))),
                        Self::Bck => Some((i.uadd(1), i.uadd(1))),
                    };
                }
                _ => prev = kind,
            }
        }

        None
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
    use std::assert_matches;

    #[test_case(FindWord::Fwd, 0, "this"; "forward start of buffer")]
    #[test_case(FindWord::Fwd, 3, "this"; "forward end of first word")]
    #[test_case(FindWord::Fwd, 4, "is"; "forward after first word")]
    #[test_case(FindWord::Fwd, 5, "is"; "forward start of second word")]
    #[test_case(FindWord::Fwd, 6, "is"; "forward end of second word")]
    #[test_case(FindWord::Fwd, 9, "test"; "forward before last word")]
    #[test_case(FindWord::Fwd, 13, "test"; "forward end of buffer")]
    #[test]
    fn expand_word(fw: FindWord, idx: usize, expected: &str) {
        let b = Buffer::new_virtual(0, "test", "this is a test", Default::default());
        let dot = Dot::Cur {
            c: Cur { idx: CharIdx(idx) },
        };
        let expanded = fw.expand(dot, &b);
        let content = expanded.content(&b);

        assert_eq!(content, expected);
    }

    #[test]
    fn expand_word_for_buffer_with_trailing_spaces() {
        let b = Buffer::new_virtual(0, "test", "this is a test   ", Default::default());
        let dot = Dot::Cur {
            c: Cur { idx: CharIdx(14) },
        };
        let expanded = FindWord::Fwd.expand(dot, &b);
        let content = expanded.content(&b);

        // Should have advanced to the end of the buffer and selected that final space character
        assert_matches!(
            expanded,
            Dot::Cur {
                c: Cur { idx: CharIdx(16) }
            }
        );
        assert_eq!(content, " ");
    }

    #[test_case(CharIdx(2), "(", true; "on left delimiter")]
    #[test_case(CharIdx(3), "E", true; "first char after left delimiter")]
    #[test_case(CharIdx(41), "9", true; "first char before right delimiter")]
    #[test_case(CharIdx(42), ")", true; "on right delimiter")]
    #[test_case(CharIdx(1), " ", false; "before left delimiter")]
    #[test_case(CharIdx(43), " ", false; "after right delimiter")]
    #[test]
    fn find_delimited(idx: CharIdx, initial: &str, should_expand: bool) {
        let b = Buffer::new_virtual(
            0,
            "test",
            "│ (EX:40727d6d-32a6-4ebc-ae4f-4d92e326a8c9) │",
            Default::default(),
        );

        let dot = Dot::Cur { c: Cur { idx } };
        assert_eq!(dot.content(&b), initial);

        let expanded = FindDelimited::new('(', ')').expand(dot, &b);

        let expected = if should_expand {
            "EX:40727d6d-32a6-4ebc-ae4f-4d92e326a8c9"
        } else {
            initial
        };

        assert_eq!(expanded.content(&b), expected);
    }

    #[test]
    fn find_paragraph_at_eof_is_noop() {
        let b = Buffer::new_virtual(0, "test", "abc\n\ndef\n\n\n", Default::default());
        let dot = Dot::Cur {
            c: Cur { idx: CharIdx(9) },
        };
        assert_eq!(dot.content(&b), "\n");

        let new_dot = FindParagraph::Fwd.expand(dot, &b);
        assert_eq!(new_dot, dot);
    }

    #[test]
    fn find_word_expand_doesnt_panic_on_empty_buffer() {
        let b = Buffer::new_virtual(0, "test", "", Default::default());
        let dot = Dot::Cur {
            c: Cur { idx: CharIdx(0) },
        };

        let new_dot = FindWord::Fwd.expand(dot, &b);
        assert_eq!(new_dot, dot);
    }
}
