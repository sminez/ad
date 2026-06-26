//! Reference editing tests for the ad Buffer implementation using
//! <https://github.com/josephg/editing-traces>
use ad_editor::{Source, buffer::Buffer, editor::BAction};
use libflate::gzip::Decoder;
use serde::Deserialize;
use std::{
    fs::File,
    io::{self, BufReader, Read},
};

/// A sequential editing trace.
///
/// See [here][0] for details on the format.
///
/// [0]: https://github.com/josephg/editing-traces/blob/master/sequential_traces/README.md
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TestData {
    pub start_content: String,
    pub end_content: String,
    #[serde(rename = "txns")]
    pub transactions: Vec<TestTransaction>,
}

impl TestData {
    pub fn try_load_from_file(fname: &str) -> io::Result<Self> {
        let file = File::open(fname)?;
        let input = BufReader::new(file);
        let mut decoder = Decoder::new(input)?;
        let mut raw_json = vec![];
        decoder.read_to_end(&mut raw_json)?;

        serde_json::from_reader(raw_json.as_slice()).map_err(io::Error::other)
    }

    pub fn apply_all(&mut self, b: &mut Buffer) {
        for tx in self.transactions.drain(..) {
            for patch in tx.patches.into_iter() {
                patch.apply(b);
            }
        }
    }

    /// Apply n transactions only
    pub fn apply_n(&mut self, b: &mut Buffer, n: usize) {
        for tx in self.transactions.drain(..).take(n) {
            for patch in tx.patches.into_iter() {
                patch.apply(b);
            }
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct TestTransaction {
    pub time: String,
    pub patches: Vec<TestPatch>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(from = "(usize, usize, String)")]
pub struct TestPatch {
    pub position: usize,
    pub delete_len: usize,
    pub insert_content: String,
}

impl TestPatch {
    // https://github.com/josephg/editing-traces/blob/master/sequential_traces/README.md#data-format
    pub fn apply(self, b: &mut Buffer) {
        if self.delete_len > 0 {
            b.set_dot_from_range(self.position, self.position + self.delete_len - 1);
            b.handle_action(BAction::Delete, Source::Fsys);
        }

        b.set_dot_from_cursor(self.position);
        b.handle_action(
            BAction::InsertString {
                s: self.insert_content,
            },
            Source::Fsys,
        );
    }
}

impl From<(usize, usize, String)> for TestPatch {
    fn from((position, delete_len, insert_content): (usize, usize, String)) -> Self {
        Self {
            position,
            delete_len,
            insert_content,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ad_editor::Config;
    use parking_lot::RwLock;
    use pretty_assertions::assert_eq;
    use std::sync::Arc;

    fn run_reference_test(name: &str) {
        let path = format!("{}/data/{name}.json.gz", env!("CARGO_MANIFEST_DIR"),);
        let mut test_data = TestData::try_load_from_file(&path).unwrap();
        let mut cfg = Config::default();
        cfg.editor.expand_tab = false;
        cfg.editor.match_indent = false;

        let mut b = Buffer::new_unnamed(0, &test_data.start_content, Arc::new(RwLock::new(cfg)));

        test_data.apply_all(&mut b);

        assert_eq!(b.str_contents(), test_data.end_content);
    }

    #[test]
    fn automerge_paper() {
        run_reference_test("automerge-paper");
    }

    #[test]
    fn clownschool_flat() {
        run_reference_test("clownschool_flat");
    }

    #[test]
    fn friends_forever_flat() {
        run_reference_test("friendsforever_flat");
    }

    #[test]
    fn json_crdt_blog_post() {
        run_reference_test("json-crdt-blog-post");
    }

    #[test]
    fn json_crdt_patch() {
        run_reference_test("json-crdt-patch");
    }

    #[test]
    fn rustcode() {
        run_reference_test("rustcode");
    }

    #[test]
    fn seph_blog() {
        run_reference_test("seph-blog1");
    }

    #[test]
    fn sveltecomponent() {
        run_reference_test("sveltecomponent");
    }
}
