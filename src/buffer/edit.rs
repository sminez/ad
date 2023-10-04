use crate::buffer::Cur;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Kind {
    Insert,
    Delete,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Txt {
    Char(char),
    String(String),
}

impl Txt {
    fn append(&mut self, t: Txt) {
        let push = |buf: &mut String| match t {
            Txt::Char(c) => buf.push(c),
            Txt::String(s) => buf.push_str(&s),
        };

        match self {
            Txt::Char(c) => {
                let mut buf = c.to_string();
                push(&mut buf);
                *self = Txt::String(buf);
            }
            Txt::String(s) => push(s),
        };
    }

    fn prepend(&mut self, t: Txt) {
        let insert = |buf: &mut String| match t {
            Txt::Char(c) => buf.insert(0, c),
            Txt::String(s) => buf.insert_str(0, &s),
        };

        match self {
            Txt::Char(c) => {
                let mut buf = c.to_string();
                insert(&mut buf);
                *self = Txt::String(buf);
            }
            Txt::String(s) => insert(s),
        };
    }
}

/// An Edit represents an atomic change to the state of a Buffer that can be rolled
/// back if needed. Sequential edits to the Buffer are compressed from char based
/// to String based where possible in order to simplify undo state.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Edit {
    kind: Kind,
    cur: Cur,
    txt: Txt,
}

impl Edit {
    fn try_combine(&mut self, e: Edit) -> Option<Edit> {
        match (self.kind, e.kind) {
            // Adding more text to the existing insert
            (Kind::Insert, Kind::Insert) => match self.valid_insert_cur(e.cur) {
                Some(should_append) => {
                    if should_append {
                        self.txt.append(e.txt);
                    } else {
                        self.txt.prepend(e.txt);
                    }
                    None
                }
                None => Some(e),
            },

            // Removing more text to the existing delete
            (Kind::Delete, Kind::Delete) => match self.valid_delete_cur(&e) {
                Some(should_append) => {
                    if should_append {
                        self.txt.append(e.txt);
                    } else {
                        self.txt.prepend(e.txt);
                    }
                    None
                }
                None => Some(e),
            },

            // There are other cases that _could_ be handled here where the kind is still matching
            // and the characters being inserted/deleted are still part of a continuous region of
            // the buffer, but for now this is sufficent for the common case of the user typing
            // without explicitly moving the cursor.
            _ => Some(e),
        }
    }

    fn valid_insert_cur(&self, cur: Cur) -> Option<bool> {
        if cur == self.cur {
            Some(false)
        } else {
            match &self.txt {
                Txt::Char(_) if cur.x == self.cur.x + 1 => Some(true),
                Txt::String(s) if cur.x == self.cur.x + s.len() => Some(true),
                _ => None,
            }
        }
    }

    fn valid_delete_cur(&self, e: &Edit) -> Option<bool> {
        if e.cur == self.cur {
            Some(true)
        } else {
            match &e.txt {
                Txt::Char(_) if e.cur.x + 1 == self.cur.x => Some(false),
                Txt::String(s) if e.cur.x + s.len() == self.cur.x => Some(false),
                _ => None,
            }
        }
    }
}

/// An edit log represents the currently undo-able state changes made to a Buffer.
///
/// The log can be unwound, restoring the buffer to a previous state, and rewound as long
/// as no new edits have been made to the buffer (i.e. it is a flat timeline not a tree).
#[derive(Default, Debug, Clone)]
pub struct EditLog {
    edits: Vec<Edit>,
}

impl EditLog {
    /// Record a single character being inserted at the given cursor position
    pub fn insert_char(&mut self, cur: Cur, c: char) {
        self.push(Edit {
            kind: Kind::Insert,
            cur,
            txt: Txt::Char(c),
        });
    }

    /// Record a string being inserted, starting at the given cursor position
    pub fn insert_string(&mut self, cur: Cur, s: String) {
        self.push(Edit {
            kind: Kind::Insert,
            cur,
            txt: Txt::String(s),
        });
    }

    /// Record a single character being deleted from the given cursor position
    pub fn delete_char(&mut self, cur: Cur, c: char) {
        self.push(Edit {
            kind: Kind::Delete,
            cur,
            txt: Txt::Char(c),
        });
    }

    /// Record a string being deleted starting at the given cursor position
    pub fn delete_string(&mut self, cur: Cur, s: String) {
        self.push(Edit {
            kind: Kind::Delete,
            cur,
            txt: Txt::String(s),
        });
    }

    fn push(&mut self, e: Edit) {
        if self.edits.is_empty() {
            self.edits.push(e);
            return;
        }

        // So long as we have at least one existing edit we can try to extend it
        // by combining it with this new one. If that fails we simply store the
        // new edit as provided.
        if let Some(e) = self.edits.last_mut().unwrap().try_combine(e) {
            self.edits.push(e);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    fn in_c(y: usize, x: usize, c: char) -> Edit {
        Edit {
            kind: Kind::Insert,
            cur: Cur { y, x },
            txt: Txt::Char(c),
        }
    }

    fn in_s(y: usize, x: usize, s: &str) -> Edit {
        Edit {
            kind: Kind::Insert,
            cur: Cur { y, x },
            txt: Txt::String(s.to_string()),
        }
    }

    fn del_c(y: usize, x: usize, c: char) -> Edit {
        Edit {
            kind: Kind::Delete,
            cur: Cur { y, x },
            txt: Txt::Char(c),
        }
    }

    fn del_s(y: usize, x: usize, s: &str) -> Edit {
        Edit {
            kind: Kind::Delete,
            cur: Cur { y, x },
            txt: Txt::String(s.to_string()),
        }
    }

    #[test_case(
        vec![in_c(0, 0, 'a'), in_c(0, 1, 'b')],
        &[in_s(0, 0, "ab")];
        "run of characters"
    )]
    #[test_case(
        vec![in_c(0, 0, 'a'), in_c(0, 0, 'b')],
        &[in_s(0, 0, "ba")];
        "run of characters at same cursor"
    )]
    #[test_case(
        vec![in_c(0, 0, 'a'), in_s(0, 1, "bcd")],
        &[in_s(0, 0, "abcd")];
        "char then string"
    )]
    #[test_case(
        vec![in_c(0, 0, 'a'), in_s(0, 0, "bcd")],
        &[in_s(0, 0, "bcda")];
        "char then string at same cursor"
    )]
    #[test_case(
        vec![in_s(0, 0, "ab"), in_s(0, 2, "cd")],
        &[in_s(0, 0, "abcd")];
        "run of strings"
    )]
    #[test_case(
        vec![in_s(0, 0, "ab"), in_s(0, 0, "cd")],
        &[in_s(0, 0, "cdab")];
        "run of strings at same cursor"
    )]
    #[test_case(
        vec![in_s(0, 0, "abc"), in_c(0, 3, 'd')],
        &[in_s(0, 0, "abcd")];
        "string then char"
    )]
    #[test_case(
        vec![in_s(0, 0, "abc"), in_c(0, 0, 'd')],
        &[in_s(0, 0, "dabc")];
        "string then char at same cursor"
    )]
    #[test]
    fn inserts_work(edits: Vec<Edit>, expected: &[Edit]) {
        let mut log = EditLog::default();
        for e in edits {
            log.push(e);
        }

        assert_eq!(&log.edits, expected);
    }

    #[test_case(
        vec![del_c(0, 1, 'b'), del_c(0, 0, 'a')],
        &[del_s(0, 1, "ab")];
        "run of chars"
    )]
    #[test_case(
        vec![del_c(0, 0, 'a'), del_c(0, 0, 'b')],
        &[del_s(0, 0, "ab")];
        "run of characters at same cursor"
    )]
    #[test_case(
        vec![del_c(0, 3, 'd'), del_s(0, 0, "abc")],
        &[del_s(0, 3, "abcd")];
        "char then string"
    )]
    #[test_case(
        vec![del_c(0, 0, 'a'), del_s(0, 0, "bcd")],
        &[del_s(0, 0, "abcd")];
        "char then string at same cursor"
    )]
    #[test]
    fn delete_work(edits: Vec<Edit>, expected: &[Edit]) {
        let mut log = EditLog::default();
        for e in edits {
            log.push(e);
        }

        assert_eq!(&log.edits, expected);
    }
}
