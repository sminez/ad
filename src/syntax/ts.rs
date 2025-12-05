//! Support for tree-sitter incremental parsing, querying and highlighting of Buffers
//!
//! For a given language the user needs to provide a .so file containing the compiled
//! tree-sitter parser and a highlights .scm file for driving the highlighting.
use crate::{
    buffer::{GapBuffer, SliceIter},
    dot::Range,
    syntax::{ByteRange, LineIter, SyntaxRange},
};
use libloading::{Library, Symbol};
use std::{
    cmp::{max, min},
    collections::HashSet,
    fmt, fs,
    iter::repeat_n,
    ops::{Deref, DerefMut},
    path::Path,
};
use tracing::{error, info};
use tree_sitter::{self as ts, StreamingIterator, ffi::TSLanguage};

pub const SUPPORTED_PREDICATES: [&str; 0] = [];

impl From<ts::Range> for ByteRange {
    fn from(r: ts::Range) -> Self {
        Self {
            from: r.start_byte,
            to: r.end_byte,
        }
    }
}

/// Buffer level tree-sitter state for parsing and highlighting
#[derive(Debug)]
pub struct TsState {
    tree: ts::Tree,
    p: Parser,
    t: Tokenizer,
}

impl TsState {
    pub fn try_new(
        lang: &str,
        so_dir: &str,
        query_dir: &str,
        gb: &GapBuffer,
    ) -> Result<Self, String> {
        let query_path = Path::new(query_dir).join(lang).join("highlights.scm");
        let query = match fs::read_to_string(query_path) {
            Ok(s) => s,
            Err(e) => return Err(format!("unable to read tree-sitter query file: {e}")),
        };

        let p = Parser::try_new(so_dir, lang)?;

        Self::try_new_explicit(p, &query, gb)
    }

    #[cfg(test)]
    pub(crate) fn try_new_from_language(
        lang_name: &str,
        lang: ts::Language,
        query: &str,
        gb: &GapBuffer,
    ) -> Result<Self, String> {
        let p = Parser::try_new_from_language(lang_name, lang)?;

        Self::try_new_explicit(p, query, gb)
    }

    fn try_new_explicit(mut p: Parser, query: &str, gb: &GapBuffer) -> Result<Self, String> {
        let tree = p.parse_with_options(
            &mut |byte_offset, _| gb.maximal_slice_from_offset(byte_offset),
            None,
            None,
        );

        match tree {
            Some(tree) => {
                let t = p.new_tokenizer(query)?;
                info!("TS loaded for {}", p.lang_name);

                Ok(Self { p, t, tree })
            }
            None => Err("failed to parse file".to_owned()),
        }
    }

    /// Apply a previously prepared edit to the tree-sitter state.
    ///
    /// The byte offset parameters must have been computed BEFORE the buffer is modified:
    /// - `start_byte`: byte offset where the edit starts (same in old and new)
    /// - `old_end_byte`: byte offset where the edit ends in the ORIGINAL buffer
    /// - `new_end_byte`: byte offset where the edit ends in the MODIFIED buffer
    pub(super) fn apply_prepared_edit(
        &mut self,
        start_byte: usize,
        old_end_byte: usize,
        new_end_byte: usize,
        gb: &GapBuffer,
    ) {
        self.tree.edit(&ts::InputEdit {
            start_byte,
            old_end_byte,
            new_end_byte,
            // See https://github.com/tree-sitter/tree-sitter/discussions/1793 for why this OK
            start_position: ts::Point::new(0, 0),
            old_end_position: ts::Point::new(0, 0),
            new_end_position: ts::Point::new(0, 0),
        });

        let new_tree = self.p.parse_with_options(
            &mut |byte_offset, _| gb.maximal_slice_from_offset(byte_offset),
            Some(&self.tree),
            None,
        );

        if let Some(tree) = new_tree {
            // TODO: it might be looking at self.tree.changed_ranges(&tree) to optimise being able
            // to only clear regions that are now invalid
            self.tree = tree;
        }

        self.t.clear();
    }

    pub(super) fn prepare_insert_char(
        &self,
        ch_idx: usize,
        ch: char,
        gb: &GapBuffer,
    ) -> (usize, usize, usize) {
        let start_byte = gb.char_to_byte(ch_idx);

        (start_byte, start_byte, start_byte + ch.len_utf8())
    }

    pub(super) fn prepare_insert_string(
        &self,
        ch_idx: usize,
        s: &str,
        gb: &GapBuffer,
    ) -> (usize, usize, usize) {
        let start_byte = gb.char_to_byte(ch_idx);

        (start_byte, start_byte, start_byte + s.len())
    }

    pub(super) fn prepare_delete_char(
        &self,
        ch_idx: usize,
        gb: &GapBuffer,
    ) -> (usize, usize, usize) {
        let (start_byte, old_end_byte) = gb.char_range_to_byte_range(ch_idx, ch_idx + 1);

        (start_byte, old_end_byte, start_byte)
    }

    pub(super) fn prepare_delete_range(
        &self,
        ch_from: usize,
        ch_to: usize,
        gb: &GapBuffer,
    ) -> (usize, usize, usize) {
        let (start_byte, old_end_byte) = gb.char_range_to_byte_range(ch_from, ch_to);

        (start_byte, old_end_byte, start_byte)
    }

    pub fn update(&mut self, gb: &GapBuffer, from_row: usize, n_rows: usize) {
        let raw_from = gb.line_to_byte(from_row);
        let raw_to = if from_row + n_rows + 1 < gb.len_lines() {
            gb.line_to_byte(from_row + n_rows + 1)
        } else {
            gb.len()
        };

        if let Some((a, b)) = self.t.missing_region(raw_from, raw_to) {
            // To avoid spinning on calling back to the tree-sitter API for individual lines, we
            // pre-emptively grab a larger block of tokens from the region ahead or behind of the
            // requested one if we have missing tokens in that direction.
            const PADDING: usize = 512;
            let byte_from = if b < raw_to {
                a.saturating_sub(PADDING)
            } else {
                a
            };
            let byte_to = if a > raw_from {
                min(b + PADDING, gb.len())
            } else {
                b
            };

            self.t.update(self.tree.root_node(), gb, byte_from, byte_to);
        }
    }

    #[inline]
    pub fn iter_tokenized_lines_from<'a>(
        &'a self,
        line: usize,
        gb: &'a GapBuffer,
        dot_range: Range,
        load_exec_range: Option<(bool, Range)>,
    ) -> LineIter<'a> {
        self.t
            .iter_tokenized_lines_from(line, gb, dot_range, load_exec_range)
    }

    pub fn pretty_print_tree(&self) -> String {
        let sexp = self.tree.root_node().to_sexp();
        let mut buf = String::with_capacity(sexp.len()); // better starting point than default
        let mut has_field = false;
        let mut indent = 0;

        for s in sexp.split([' ', ')']) {
            if s.is_empty() {
                indent -= 1;
                buf.push(')');
            } else if s.starts_with('(') {
                if has_field {
                    has_field = false;
                } else {
                    if indent > 0 {
                        buf.push('\n');
                        buf.extend(repeat_n(' ', indent * 2));
                    }
                    indent += 1;
                }

                buf.push_str(s); // "(node_name"
            } else if s.ends_with(':') {
                buf.push('\n');
                buf.extend(repeat_n(' ', indent * 2));
                buf.push_str(s); // "field:"
                buf.push(' ');
                has_field = true;
                indent += 1;
            }
        }

        buf
    }
}

// Required for us to be able to pass GapBuffers to the tree-sitter API
impl<'a> ts::TextProvider<&'a [u8]> for &'a GapBuffer {
    type I = SliceIter<'a>;

    fn text(&mut self, node: ts::Node<'_>) -> Self::I {
        let ts::Range {
            start_byte,
            end_byte,
            ..
        } = node.range();

        self.slice_from_byte_offsets(start_byte, end_byte)
            .slice_iter()
    }
}

/// A dynamically loaded tree-sitter parser backed by an on disk .so file
pub struct Parser {
    lang_name: String,
    inner: ts::Parser,
    lang: ts::Language,
    // Need to prevent drop while the parser is in use
    // Stored as an Option to allow for crate-based parsers that are not backed by a .so file
    _lib: Option<Library>,
}

impl Deref for Parser {
    type Target = ts::Parser;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Parser {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl fmt::Debug for Parser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parser({})", self.lang_name)
    }
}

impl Parser {
    /// Error values returned by this function are intended as status messages to be
    /// presented to the user.
    pub fn try_new<P: AsRef<Path>>(so_dir: P, lang_name: &str) -> Result<Self, String> {
        let p = so_dir.as_ref().join(format!("{lang_name}.so"));
        let lang_fn = format!("tree_sitter_{lang_name}");

        // SAFETY: if the library loads and contains the target symbol we expect the
        //         given .so file to be a valid tree-sitter parser
        unsafe {
            let lib = Library::new(p).map_err(|e| e.to_string())?;
            let func: Symbol<'_, unsafe extern "C" fn() -> *const TSLanguage> =
                lib.get(lang_fn.as_bytes()).map_err(|e| e.to_string())?;

            let lang = ts::Language::from_raw(func());
            if lang.abi_version() < ts::MIN_COMPATIBLE_LANGUAGE_VERSION {
                return Err(format!(
                    "incompatible .so tree-sitter parser version: {} < {}",
                    lang.abi_version(),
                    ts::MIN_COMPATIBLE_LANGUAGE_VERSION
                ));
            }

            let mut inner = ts::Parser::new();
            inner.set_language(&lang).map_err(|e| e.to_string())?;

            Ok(Self {
                lang_name: lang_name.to_owned(),
                inner,
                lang,
                _lib: Some(lib),
            })
        }
    }

    /// Construct a new tokenizer directly from a ts::Language provided by a crate
    #[cfg(test)]
    fn try_new_from_language(lang_name: &str, lang: ts::Language) -> Result<Self, String> {
        let mut inner = ts::Parser::new();
        inner.set_language(&lang).map_err(|e| e.to_string())?;

        Ok(Self {
            lang_name: lang_name.to_owned(),
            inner,
            lang,
            _lib: None,
        })
    }

    pub fn new_tokenizer(&self, query: &str) -> Result<Tokenizer, String> {
        let q = ts::Query::new(&self.lang, query).map_err(|e| format!("{e:?}"))?;
        let cur = ts::QueryCursor::new();

        // If a query has been copied from another text editor then there is a chance that
        // it makes use of custom predicates that we don't know how to handle. The highlights
        // as a whole won't behave as the user expects in this instance so we error out the
        // setup of syntax-highlighting as a whole in this case and log an error
        let mut unsupported_predicates = HashSet::new();
        for i in 0..q.pattern_count() {
            for p in q.general_predicates(i) {
                if !SUPPORTED_PREDICATES.contains(&p.operator.as_ref()) {
                    unsupported_predicates.insert(p.operator.clone());
                }
            }
        }

        if !unsupported_predicates.is_empty() {
            error!("Unsupported custom tree-sitter predicates found: {unsupported_predicates:?}");
            info!("Supported custom tree-sitter predicates: {SUPPORTED_PREDICATES:?}");
            info!("Please modify the highlights.scm file to remove the unsupported predicates");

            return Err(format!(
                "{} highlights query contained unsupported custom predicates",
                self.lang_name
            ));
        }

        let names = q.capture_names().iter().map(|s| s.to_string()).collect();

        Ok(Tokenizer {
            q,
            cur,
            names,
            ranges: Vec::new(),
            tokenized_regions: Vec::new(),
        })
    }
}

pub struct Tokenizer {
    // Tree-sitter state
    q: ts::Query,
    cur: ts::QueryCursor,
    names: Vec<String>,
    // Cache of computed syntax tokens for passing to LineIter
    ranges: Vec<SyntaxRange>,
    // The regions of the file that we currently have tokens for
    tokenized_regions: Vec<ByteRange>,
}

impl fmt::Debug for Tokenizer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tokenizer")
    }
}

#[inline]
fn mark_region(regions: &mut Vec<ByteRange>, from: usize, to: usize) {
    regions.push(ByteRange { from, to });
    if regions.len() == 1 {
        return;
    }

    regions.sort_unstable();

    let mut idx = 0;
    for i in 1..regions.len() {
        if regions[idx].to >= regions[i].from {
            // Merge overlapping regions
            regions[idx].to = max(regions[idx].to, regions[i].to);
        } else {
            // Move to the next region to check for overlaps
            idx += 1;
            regions.swap(idx, i);
        }
    }

    // If we performed any merges then there will be unused regions at the end of the
    // Vec now that we need to drop
    regions.truncate(idx + 1);
}

/// This is not attempting to be maximally efficient in returning a set of missing regions as the
/// goal is simply to minimise the amount of retokenization we do via tree-sitter where possible.
#[inline]
fn missing_region(regions: &[ByteRange], from: usize, to: usize) -> Option<(usize, usize)> {
    let mut it = regions.iter();
    while let Some(r) = it.next() {
        if to < r.from {
            // before this region and not in the previous so all missing
            break;
        } else if from < r.from {
            // runs up to the start of this region or over this region
            let end = if r.to > to { r.from } else { to };
            return Some((from, end));
        } else if r.contains(from, to) {
            // contained entirely within this region so nothing missing
            return None;
        } else if from < r.to && to > r.to {
            // from inside this region out to the next or past it
            let end = match it.next() {
                Some(r) if r.from < to => r.from,
                _ => to,
            };
            return Some((r.to, end));
        }
    }

    Some((from, to))
}

impl Tokenizer {
    fn clear(&mut self) {
        self.ranges.clear();
        self.tokenized_regions.clear();
    }

    fn missing_region(&self, from: usize, to: usize) -> Option<(usize, usize)> {
        missing_region(&self.tokenized_regions, from, to)
    }

    fn mark_region(&mut self, from: usize, to: usize) {
        mark_region(&mut self.tokenized_regions, from, to);
    }

    pub fn update(&mut self, root: ts::Node<'_>, gb: &GapBuffer, from: usize, to: usize) {
        self.cur.set_byte_range(from..to);

        // This is a streaming-iterator not an iterator, hence the odd while-let that follows
        let mut it = self.cur.captures(&self.q, root, gb);

        while let Some((m, idx)) = it.next() {
            let cap = m.captures[*idx];
            let r = ByteRange::from(cap.node.range());
            if let Some(prev) = self.ranges.last_mut() {
                if r == prev.r {
                    // preferring the the last capture found so that precedence ordering
                    // in query files matches Neovim & the treesitter-cli
                    prev.cap_idx = Some(cap.index as usize);
                    continue;
                } else if r.from < prev.r.to && prev.r.from < r.to {
                    continue;
                }
            }
            self.ranges.push(SyntaxRange {
                r,
                cap_idx: Some(cap.index as usize),
            });
        }

        self.ranges.sort_unstable();
        self.ranges.dedup();
        self.mark_region(from, to);
    }

    #[inline]
    pub fn iter_tokenized_lines_from<'a>(
        &'a self,
        line: usize,
        gb: &'a GapBuffer,
        dot_range: Range,
        load_exec_range: Option<(bool, Range)>,
    ) -> LineIter<'a> {
        LineIter::new(
            line,
            gb,
            dot_range,
            load_exec_range,
            &self.names,
            &self.ranges,
        )
    }

    #[cfg(test)]
    fn range_tokens(&self) -> Vec<crate::syntax::RangeToken<'_>> {
        use crate::syntax::{RangeToken, TK_DEFAULT};

        let names = self.q.capture_names();

        self.ranges
            .iter()
            .map(|sr| RangeToken {
                tag: sr.cap_idx.map(|i| names[i]).unwrap_or(TK_DEFAULT),
                r: sr.r,
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        buffer::Buffer,
        dot::{Cur, Dot},
        editor::Action,
        syntax::{RangeToken, SyntaxState, SyntaxStateInner},
    };
    use ad_event::Source;
    use simple_test_case::test_case;

    fn rt(tag: &str, from: usize, to: usize) -> RangeToken<'_> {
        RangeToken {
            tag,
            r: ByteRange { from, to },
        }
    }

    #[test]
    fn char_delete_correctly_update_state() {
        // minimal query for the fn keyword and parens
        let query = r#"
"fn" @keyword

[ "(" ")" "{" "}" ] @punctuation"#;

        let s = "fn main() {}";
        let mut b = Buffer::new_unnamed(0, s, Default::default());
        let gb = &b.txt;
        let mut ts =
            TsState::try_new_from_language("rust", tree_sitter_rust::LANGUAGE.into(), query, gb)
                .unwrap();
        ts.update(gb, 0, gb.len());
        b.syntax_state = Some(SyntaxState::ts(ts));

        assert_eq!(b.str_contents(), "fn main() {}");

        let ranges = match b.syntax_state.as_ref() {
            Some(SyntaxState {
                inner: SyntaxStateInner::Ts(ts),
                ..
            }) => ts.t.range_tokens(),
            _ => panic!("no ts state"),
        };
        assert_eq!(
            ranges,
            vec![
                rt("keyword", 0, 2),       // fn
                rt("punctuation", 7, 8),   // (
                rt("punctuation", 8, 9),   // )
                rt("punctuation", 10, 11), // {
                rt("punctuation", 11, 12), // }
            ]
        );

        b.dot = Dot::Cur { c: Cur { idx: 9 } };
        b.handle_action(Action::Delete, Source::Fsys);
        b.syntax_state
            .as_mut()
            .unwrap()
            .update(&b.txt, 0, usize::MAX - 1);
        let ranges = match b.syntax_state.as_ref() {
            Some(SyntaxState {
                inner: SyntaxStateInner::Ts(ts),
                ..
            }) => ts.t.range_tokens(),
            _ => panic!("no ts state"),
        };

        assert_eq!(b.str_contents(), "fn main(){}");
        assert_eq!(ranges.len(), 5);

        // these two should have moved left one character
        assert_eq!(ranges[3], rt("punctuation", 9, 10), "opening curly");
        assert_eq!(ranges[4], rt("punctuation", 10, 11), "closing curly");
    }

    #[test]
    fn overlapping_tokens_prefer_previous_matches() {
        // Minimal query extracted from the full query in gh#88 that resulted in
        // overlapping tokens being produced
        let query = r#"
(identifier) @variable

(import_statement
  name: (dotted_name
    (identifier) @module))

(import_statement
  name: (aliased_import
    name: (dotted_name
      (identifier) @module)
    alias: (identifier) @module))

(import_from_statement
  module_name: (dotted_name
    (identifier) @module))"#;

        let s = "import builtins as _builtins";
        let b = Buffer::new_unnamed(0, s, Default::default());
        let gb = &b.txt;
        let mut ts = TsState::try_new_from_language(
            "python",
            tree_sitter_python::LANGUAGE.into(),
            query,
            gb,
        )
        .unwrap();
        ts.update(gb, 0, gb.len());

        assert_eq!(
            ts.t.range_tokens(),
            vec![
                rt("module", 7, 15),  // builtins
                rt("module", 19, 28)  // _builtins
            ]
        );
    }

    #[test]
    fn built_in_predicates_work() {
        let query = r#"
(identifier) @variable

; Assume all-caps names are constants
((identifier) @constant
  (#match? @constant "^[A-Z][A-Z%d_]*$"))

((identifier) @constant.builtin
  (#any-of? @constant.builtin "Some" "None" "Ok" "Err"))

[ "(" ")" "{" "}" ] @punctuation"#;

        let s = "Ok(Some(42)) foo BAR";
        let b = Buffer::new_unnamed(0, s, Default::default());
        let gb = &b.txt;
        let mut ts =
            TsState::try_new_from_language("rust", tree_sitter_rust::LANGUAGE.into(), query, gb)
                .unwrap();
        ts.update(gb, 0, gb.len());

        assert_eq!(
            ts.t.range_tokens(),
            vec![
                rt("constant.builtin", 0, 2), // Ok
                rt("punctuation", 2, 3),      // (
                rt("constant.builtin", 3, 7), // Some
                rt("punctuation", 7, 8),      // (
                rt("punctuation", 10, 11),    // )
                rt("punctuation", 11, 12),    // )
                rt("variable", 13, 16),       // foo
                rt("constant", 17, 20),       // BAR
            ]
        );
    }

    fn br(from: usize, to: usize) -> ByteRange {
        ByteRange { from, to }
    }

    #[test_case(vec![], 0, 5, vec![br(0, 5)]; "no initial regions")]
    #[test_case(vec![br(0, 5)], 0, 5, vec![br(0, 5)]; "existing region idempotent")]
    #[test_case(vec![br(9, 15)], 0, 5, vec![br(0, 5), br(9, 15)]; "disjoint regions")]
    #[test_case(vec![br(0, 5)], 3, 5, vec![br(0, 5)]; "existing region contains new")]
    #[test_case(vec![br(0, 5)], 3, 9, vec![br(0, 9)]; "existing region extending past current end")]
    #[test_case(vec![br(3, 5)], 0, 3, vec![br(0, 5)]; "existing region extending before current start")]
    #[test_case(vec![br(3, 5)], 0, 9, vec![br(0, 9)]; "existing region contained within new")]
    #[test_case(vec![br(0, 5), br(7, 15)], 4, 9, vec![br(0, 15)]; "new region joins multiple existing")]
    #[test]
    fn mark_region_works(
        mut regions: Vec<ByteRange>,
        from: usize,
        to: usize,
        expected: Vec<ByteRange>,
    ) {
        mark_region(&mut regions, from, to);
        assert_eq!(regions, expected);
    }

    #[test_case(vec![br(0, 100)], 5, 20, None; "contained")]
    #[test_case(vec![br(0, 1366)], 89, 1385, Some((1366, 1385)); "scroll down")]
    #[test_case(vec![br(100, 1366)], 0, 255, Some((0, 100)); "scroll up")]
    #[test_case(vec![br(100, 1366)], 0, 80, Some((0, 80)); "before")]
    #[test_case(vec![br(100, 1366)], 1400, 1500, Some((1400, 1500)); "after")]
    #[test_case(vec![br(0, 100), br(200, 300)], 150, 180, Some((150, 180)); "in between regions")]
    #[test_case(vec![br(0, 100), br(200, 300)], 50, 180, Some((100, 180)); "from one range into gap")]
    #[test_case(vec![br(0, 100), br(200, 300)], 150, 280, Some((150, 200)); "from gap into region")]
    #[test_case(vec![br(0, 100), br(200, 300)], 50, 280, Some((100, 200)); "from one region into another")]
    #[test_case(vec![br(50, 100), br(200, 300)], 0, 150, Some((0, 150)); "around an existing region")]
    #[test]
    fn missing_region_works(
        regions: Vec<ByteRange>,
        from: usize,
        to: usize,
        expected: Option<(usize, usize)>,
    ) {
        let res = missing_region(&regions, from, to);
        assert_eq!(res, expected);
    }
}
