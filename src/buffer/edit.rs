use crate::buffer::{Buffer, CharOffset, Cur};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(super) enum Kind {
    Insert,
    Delete,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Txt {
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
pub(crate) struct Edit {
    pub(super) kind: Kind,
    pub(super) cur: Cur,
    pub(super) txt: Txt,
}

impl Edit {
    fn into_undo(mut self) -> Self {
        self.kind = match self.kind {
            Kind::Insert => Kind::Delete,
            Kind::Delete => Kind::Insert,
        };

        self
    }

    pub fn string_repr(&self, b: &Buffer) -> String {
        let indicator = if self.kind == Kind::Insert { "I" } else { "D" };
        let addr = self.cur.as_string_addr(b);

        match &self.txt {
            Txt::Char(c) => format!("{indicator} {addr} '{}'", c),
            Txt::String(s) => format!("{indicator} {addr} '{}'", s.replace('\n', "\\n")),
        }
    }

    fn try_combine(&mut self, e: Edit) -> Option<Edit> {
        match (self.kind, e.kind) {
            (Kind::Insert, Kind::Insert) => self.try_extend_insert(e),
            (Kind::Delete, Kind::Delete) => self.try_extend_delete(e),

            // There are other cases that _could_ be handled here where the kind is still matching
            // and the characters being inserted/deleted are still part of a continuous region of
            // the buffer, but for now this is sufficient for the common case of the user typing
            // without explicitly moving the cursor.
            _ => Some(e),
        }
    }

    fn try_extend_insert(&mut self, e: Edit) -> Option<Edit> {
        if e.cur == self.cur {
            self.txt.prepend(e.txt);
            None
        } else {
            match &self.txt {
                Txt::Char(_) if e.cur.idx == self.cur.idx.uadd(1) => {
                    self.txt.append(e.txt);
                    None
                }
                Txt::String(s) if e.cur.idx == self.cur.idx + CharOffset::from(s.as_str()) => {
                    self.txt.append(e.txt);
                    None
                }
                _ => Some(e),
            }
        }
    }

    fn try_extend_delete(&mut self, e: Edit) -> Option<Edit> {
        if e.cur == self.cur {
            self.txt.append(e.txt);
            None
        } else {
            match &e.txt {
                Txt::Char(_) if e.cur.idx.uadd(1) == self.cur.idx => {
                    self.txt.prepend(e.txt);
                    self.cur = e.cur;
                    None
                }
                Txt::String(s) if e.cur.idx + CharOffset::from(s.as_str()) == self.cur.idx => {
                    self.txt.prepend(e.txt);
                    self.cur = e.cur;
                    None
                }
                _ => Some(e),
            }
        }
    }
}

pub type Transaction = Vec<Edit>;

/// An edit log represents the currently undo-able state changes made to a Buffer.
///
/// The log can be unwound, restoring the buffer to a previous state, and rewound as long
/// as no new edits have been made to the buffer (i.e. it is a flat timeline not a tree).
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct EditLog {
    pub(super) edits: Vec<Transaction>,
    pub(super) undone_edits: Vec<Transaction>,
    pub(super) paused: bool,
}

impl EditLog {
    pub(crate) fn clear(&mut self) {
        self.edits.clear();
        self.undone_edits.clear();
        self.paused = false;
    }

    pub(crate) fn debug_edits(&self, b: &Buffer) -> Vec<String> {
        self.edits
            .iter()
            .flat_map(|t| t.iter().map(|e| e.string_repr(b)))
            .collect()
    }

    pub(crate) fn undo(&mut self) -> Option<Transaction> {
        let mut trans = self.edits.pop()?;
        while trans.is_empty() {
            trans = self.edits.pop()?;
        }

        self.undone_edits.push(trans.clone());
        trans.reverse();

        Some(trans.into_iter().map(|e| e.into_undo()).collect())
    }

    pub(crate) fn redo(&mut self) -> Option<Transaction> {
        let mut trans = self.undone_edits.pop()?;
        while trans.is_empty() {
            trans = self.undone_edits.pop()?;
        }

        self.edits.push(trans.clone());

        Some(trans)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.edits.is_empty()
    }

    /// Record a single character being inserted at the given cursor position
    pub(crate) fn insert_char(&mut self, cur: Cur, ch: char) {
        if !self.paused {
            self.push(Edit {
                kind: Kind::Insert,
                cur,
                txt: Txt::Char(ch),
            });
        }
    }

    /// Record a string being inserted, starting at the given cursor position
    pub(crate) fn insert_string(&mut self, cur: Cur, s: String) {
        if !self.paused {
            self.push(Edit {
                kind: Kind::Insert,
                cur,
                txt: Txt::String(s),
            });
        }
    }

    /// Record a single character being deleted from the given cursor position
    pub(crate) fn delete_char(&mut self, cur: Cur, ch: char) {
        if !self.paused {
            self.push(Edit {
                kind: Kind::Delete,
                cur,
                txt: Txt::Char(ch),
            });
        }
    }

    /// Record a string being deleted starting at the given cursor position
    pub(crate) fn delete_string(&mut self, cur: Cur, s: String) {
        if !self.paused {
            self.push(Edit {
                kind: Kind::Delete,
                cur,
                txt: Txt::String(s),
            });
        }
    }

    pub(crate) fn new_transaction(&mut self) {
        match self.edits.last() {
            Some(t) if t.is_empty() => return,
            _ => (),
        }
        self.edits.push(Vec::new());
    }

    // So long as we have at least one existing edit in an existing transaction we can try to
    // extend it by combining it with this new one. If that fails we simply store the new edit as
    // it was provided. If we had no previous transaction at all we create a new one containing the
    // new edit.
    fn push(&mut self, e: Edit) {
        self.undone_edits.clear();

        match self.edits.last_mut() {
            Some(trans) => match trans.last_mut() {
                Some(prev) => {
                    // On success we combined with prev and we're done. If combining failed then
                    // the returned value here is simply the edit we passed in so we push it onto
                    // the transaction directly.
                    if let Some(e) = prev.try_combine(e) {
                        trans.push(e);
                    }
                }
                None => trans.push(e),
            },

            None => self.edits.push(vec![e]),
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::buffer::CharIdx;
    use simple_test_case::test_case;

    pub(crate) fn in_c(idx: usize, c: char) -> Edit {
        Edit {
            kind: Kind::Insert,
            cur: Cur { idx: CharIdx(idx) },
            txt: Txt::Char(c),
        }
    }

    pub(crate) fn in_s(idx: usize, s: &str) -> Edit {
        Edit {
            kind: Kind::Insert,
            cur: Cur { idx: CharIdx(idx) },
            txt: Txt::String(s.to_string()),
        }
    }

    pub(crate) fn del_c(idx: usize, c: char) -> Edit {
        Edit {
            kind: Kind::Delete,
            cur: Cur { idx: CharIdx(idx) },
            txt: Txt::Char(c),
        }
    }

    pub(crate) fn del_s(idx: usize, s: &str) -> Edit {
        Edit {
            kind: Kind::Delete,
            cur: Cur { idx: CharIdx(idx) },
            txt: Txt::String(s.to_string()),
        }
    }

    // insert appending to previous
    #[test_case(in_c(0, 'a'), in_c(1, 'b'), Some(in_s(0, "ab")); "insert append char char")]
    #[test_case(in_c(0, 'a'), in_s(1, "bc"), Some(in_s(0, "abc")); "insert append char string")]
    #[test_case(in_s(0, "ab"), in_s(2, "cd"), Some(in_s(0, "abcd")); "insert append string string")]
    #[test_case(in_s(0, "ab"), in_c(2, 'c'), Some(in_s(0, "abc")); "insert append string char")]
    // insert with non-moving cursor
    #[test_case(in_c(0, 'a'), in_c(0, 'b'), Some(in_s(0, "ba")); "insert same cur char char")] // typos:ignore
    #[test_case(in_c(0, 'a'), in_s(0, "bc"), Some(in_s(0, "bca")); "insert same cur char string")]
    #[test_case(in_s(0, "ab"), in_s(0, "cd"), Some(in_s(0, "cdab")); "insert same cur string string")]
    #[test_case(in_s(0, "ab"), in_c(0, 'c'), Some(in_s(0, "cab")); "insert same cur string char")]
    // delete appending to previous
    #[test_case(del_c(1, 'a'), del_c(0, 'b'), Some(del_s(0, "ba")); "delete append char char")] // typos:ignore
    #[test_case(del_c(2, 'a'), del_s(0, "bc"), Some(del_s(0, "bca")); "delete append char string")]
    #[test_case(del_s(2, "ab"), del_s(0, "cd"), Some(del_s(0, "cdab")); "delete append string string")]
    #[test_case(del_s(1, "ab"), del_c(0, 'c'), Some(del_s(0, "cab")); "delete append string char")]
    // delete with non-moving cursor
    #[test_case(del_c(0, 'a'), del_c(0, 'b'), Some(del_s(0, "ab")); "delete same cur char char")]
    #[test_case(del_c(0, 'a'), del_s(0, "b"), Some(del_s(0, "ab")); "delete same cur char string")]
    #[test_case(del_s(0, "a"), del_s(0, "b"), Some(del_s(0, "ab")); "delete same cur string string")]
    #[test_case(del_s(0, "a"), del_c(0, 'b'), Some(del_s(0, "ab")); "delete same cur string char")]
    // disjoint inserts
    #[test_case(in_c(0, 'a'), in_c(9, 'b'), None; "insert disjoint char char")]
    #[test_case(in_c(0, 'a'), in_s(9, "bc"), None; "insert disjoint char string")]
    #[test_case(in_s(0, "ab"), in_s(9, "cd"), None; "insert disjoint string string")]
    #[test_case(in_s(0, "ab"), in_c(9, 'c'), None; "insert disjoint string char")]
    // disjoint deletes
    #[test_case(del_c(0, 'a'), del_c(9, 'b'), None; "delete disjoint char char")]
    #[test_case(del_c(0, 'a'), del_s(9, "b"), None; "delete disjoint char string")]
    #[test_case(del_s(0, "a"), del_s(9, "b"), None; "delete disjoint string string")]
    #[test_case(del_s(0, "a"), del_c(9, 'b'), None; "delete disjoint string char")]
    // mixed insert and delete (modified from the otherwise successful cases above)
    #[test_case(in_c(0, 'a'), del_c(1, 'b'), None; "insert delete char char")]
    #[test_case(in_c(0, 'a'), del_s(1, "bc"), None; "insert delete char string")]
    #[test_case(in_s(0, "ab"), del_s(2, "cd"), None; "insert delete string string")]
    #[test_case(in_s(0, "ab"), del_c(2, 'c'), None; "insert delete string char")]
    #[test_case(del_c(0, 'a'), in_c(0, 'b'), None; "delete insert char char")]
    #[test_case(del_c(0, 'a'), in_s(0, "bc"), None; "delete insert char string")]
    #[test_case(del_s(0, "ab"), in_s(0, "cd"), None; "delete insert string string")]
    #[test_case(del_s(0, "ab"), in_c(0, 'c'), None; "delete insert string char")]
    #[test]
    fn edit_try_combine_works(mut e1: Edit, e2: Edit, expected: Option<Edit>) {
        let res = e1.try_combine(e2.clone());

        match (res, expected) {
            (Some(rejected), None) => assert_eq!(rejected, e2),
            (None, Some(combined)) => assert_eq!(combined, e1),
            (Some(res), Some(_)) => panic!("expected to combine but got {res:?}"),
            (None, None) => panic!("expected to reject but merged e1 is {e1:?}"),
        }
    }

    fn transaction_vec(raw: &[&[Edit]]) -> Vec<Transaction> {
        raw.iter().map(|edits| edits.to_vec()).collect()
    }

    fn cur(i: usize) -> Cur {
        Cur { idx: CharIdx(i) }
    }

    #[test]
    fn inserts_combine() {
        let mut log = EditLog::default();

        // cursor moves with the text being inserted here
        log.insert_char(cur(0), 'a'); // start with single char
        log.insert_string(cur(1), "bc".into()); // combine to string
        log.insert_string(cur(3), "de".into()); // extend with string
        log.insert_char(cur(5), 'f'); // extend with char

        assert_eq!(log.edits, transaction_vec(&[&[in_s(0, "abcdef")]]));
        assert_eq!(log.undone_edits, transaction_vec(&[]));
    }

    #[test]
    fn deletes_combine() {
        let mut log = EditLog::default();

        // unlike insert, the cursor stays fixed here
        log.delete_char(cur(0), 'a'); // start with single char
        log.delete_string(cur(0), "bc".into()); // combine to string
        log.delete_string(cur(0), "de".into()); // extend with string
        log.delete_char(cur(0), 'f'); // extend with char

        assert_eq!(log.edits, transaction_vec(&[&[del_s(0, "abcdef")]]));
        assert_eq!(log.undone_edits, transaction_vec(&[]));
    }

    #[test]
    fn creating_a_new_transaction_prevents_combining() {
        let mut log = EditLog::default();

        log.insert_char(cur(0), 'a');
        log.insert_string(cur(1), "bc".into());

        log.new_transaction();

        log.insert_string(cur(3), "de".into());
        log.insert_char(cur(5), 'f');

        assert_eq!(
            log.edits,
            transaction_vec(&[&[in_s(0, "abc")], &[in_s(3, "def")]])
        );
        assert_eq!(log.undone_edits, transaction_vec(&[]));
    }

    #[test]
    fn new_transaction_is_noop_for_empty_current_transaction() {
        let initial_edits = transaction_vec(&[&[in_c(0, 'a')], &[]]);

        let mut log = EditLog {
            edits: initial_edits.clone(),
            undone_edits: vec![],
            paused: false,
        };

        log.new_transaction();

        assert_eq!(log.edits, initial_edits);
        assert_eq!(log.undone_edits, transaction_vec(&[]));
    }

    #[test_case(
        transaction_vec(&[&[in_c(0, 'a')], &[in_c(1, 'b'), del_c(2, 'c')]]),
        Some(vec![in_c(2, 'c'), del_c(1, 'b')]),
        transaction_vec(&[&[in_c(0, 'a')]]),
        transaction_vec(&[&[in_c(1, 'b'), del_c(2, 'c')]]);
        "with historic edits"
    )]
    #[test_case(
        vec![vec![]],
        None,
        vec![],
        vec![];
        "only empty transaction is None"
    )]
    #[test_case(
        transaction_vec(&[&[in_c(0, 'a')], &[in_c(1, 'b'), del_c(2, 'c')], &[]]),
        Some(vec![in_c(2, 'c'), del_c(1, 'b')]),
        transaction_vec(&[&[in_c(0, 'a')]]),
        transaction_vec(&[&[in_c(1, 'b'), del_c(2, 'c')]]);
        "an empty transaction is skipped"
    )]
    #[test]
    fn undo_works(
        edits: Vec<Transaction>,
        res: Option<Transaction>,
        expected_edits: Vec<Transaction>,
        expected_undone: Vec<Transaction>,
    ) {
        let mut log = EditLog {
            edits,
            undone_edits: vec![],
            paused: false,
        };

        assert_eq!(log.undo(), res);
        assert_eq!(log.edits, expected_edits);
        assert_eq!(log.undone_edits, expected_undone);
    }

    #[test_case(
        transaction_vec(&[&[in_c(0, 'a')]]),
        transaction_vec(&[&[in_c(1, 'b'), del_c(2, 'c')]]),
        Some(vec![in_c(1, 'b'), del_c(2, 'c')]),
        transaction_vec(&[&[in_c(0, 'a')], &[in_c(1, 'b'), del_c(2, 'c')]]),
        transaction_vec(&[]);
        "with historic edits"
    )]
    #[test_case(
        vec![],
        vec![vec![]],
        None,
        vec![],
        vec![];
        "only empty transaction is None"
    )]
    #[test_case(
        transaction_vec(&[&[in_c(0, 'a')]]),
        transaction_vec(&[&[in_c(1, 'b'), del_c(2, 'c')], &[]]),
        Some(vec![in_c(1, 'b'), del_c(2, 'c')]),
        transaction_vec(&[&[in_c(0, 'a')], &[in_c(1, 'b'), del_c(2, 'c')]]),
        transaction_vec(&[]);
        "an empty transaction is skipped"
    )]
    #[test]
    fn redo_works(
        edits: Vec<Transaction>,
        undone_edits: Vec<Transaction>,
        res: Option<Transaction>,
        expected_edits: Vec<Transaction>,
        expected_undone: Vec<Transaction>,
    ) {
        let mut log = EditLog {
            edits,
            undone_edits,
            paused: false,
        };

        assert_eq!(log.redo(), res);
        assert_eq!(log.edits, expected_edits);
        assert_eq!(log.undone_edits, expected_undone);
    }

    #[test]
    fn push_clears_undone_edit_state() {
        let mut log = EditLog {
            edits: vec![],
            undone_edits: transaction_vec(&[&[in_c(1, 'a')]]),
            paused: false,
        };

        log.push(in_c(100, 'b'));

        assert_eq!(log.edits, transaction_vec(&[&[in_c(100, 'b')]]));
        assert!(log.undone_edits.is_empty(), "{log:?}");
    }
}
