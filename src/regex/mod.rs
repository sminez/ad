use crate::buffer::ByteIdx;
use regex_automata::{
    Anchored, Input, MatchKind, PatternID,
    hybrid::{self, dfa::DFA},
    meta,
    nfa::thompson::{self, Compiler, Config, WhichCaptures},
    util::captures::Captures,
};
use regex_syntax::{
    ast::{
        Ast, Flag, Flags, FlagsItem, FlagsItemKind, Group, GroupKind, LiteralKind, parse::Parser,
    },
    hir::{Hir, translate::TranslatorBuilder},
};
use std::{fmt, sync::Arc};

mod haystack;
mod matches;

pub use haystack::Haystack;
pub use matches::{Match, MatchIter};

/// An error that can occur when building a [Regex] or [RevRegex].
#[derive(Debug, Clone)]
pub enum Error {
    Syntax(Box<regex_syntax::Error>),
    Build(Box<meta::BuildError>),
    NfaBuild(Box<thompson::BuildError>),
    DfaBuild(Box<hybrid::BuildError>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Syntax(inner) => inner.fmt(f),
            Self::Build(inner) => inner.fmt(f),
            Self::NfaBuild(inner) => inner.fmt(f),
            Self::DfaBuild(inner) => inner.fmt(f),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Syntax(inner) => Some(inner),
            Self::Build(inner) => Some(inner),
            Self::NfaBuild(inner) => Some(inner),
            Self::DfaBuild(inner) => Some(inner),
        }
    }
}

impl From<regex_syntax::Error> for Error {
    fn from(err: regex_syntax::Error) -> Self {
        Self::Syntax(Box::new(err))
    }
}

impl From<meta::BuildError> for Error {
    fn from(err: meta::BuildError) -> Self {
        Self::Build(Box::new(err))
    }
}

impl From<thompson::BuildError> for Error {
    fn from(err: thompson::BuildError) -> Self {
        Self::NfaBuild(Box::new(err))
    }
}

impl From<hybrid::BuildError> for Error {
    fn from(err: hybrid::BuildError) -> Self {
        Self::DfaBuild(Box::new(err))
    }
}

/// A regex matcher primarily intended for use inside of the `ad` text editor.
///
/// This matcher provides a somewhat limited public API that only extends to cover the use cases
/// within `ad`.
#[derive(Debug, Clone)]
pub struct Regex {
    pattern: Arc<str>,
    inner: meta::Regex,
    submatch_names: Arc<[Option<Arc<str>>]>,
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern
    }
}

impl Eq for Regex {}

impl fmt::Display for Regex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pattern)
    }
}

impl Regex {
    pub fn compile(pattern: impl AsRef<str>) -> Result<Self, Error> {
        let hir = build_hir(pattern.as_ref())?;
        let inner = meta::Regex::builder().build_from_hir(&hir)?;

        let submatch_names: Vec<Option<Arc<str>>> = inner
            .group_info()
            .pattern_names(PatternID::must(0))
            .map(|name| name.map(Arc::from))
            .collect();

        Ok(Self {
            pattern: Arc::from(pattern.as_ref()),
            inner,
            submatch_names: Arc::from(submatch_names.into_boxed_slice()),
        })
    }

    /// Determine whether or not this Regex matches the haystack without searching for the
    /// leftmost-longest match and associated submatch boundaries.
    pub fn matches<H>(&self, haystack: &H) -> bool
    where
        H: Haystack,
    {
        self.matches_from(haystack, ByteIdx(0))
    }

    /// Determine whether or not this Regex matches the [Haystack] from the given offset without
    /// searching for the leftmost-longest match and associated submatch boundaries.
    pub fn matches_from<H>(&self, haystack: &H, offset: ByteIdx) -> bool
    where
        H: Haystack,
    {
        let cow = haystack.substr_from(ByteIdx(0)).unwrap_or_default();
        let input = Input::new(cow.as_ref()).range(offset.0..);

        self.inner.is_match(input)
    }

    /// Determine whether or not this Regex matches the [Haystack] between the given offsets
    /// without searching for the leftmost-longest match and associated submatch boundaries.
    pub fn matches_between<H>(&self, haystack: &H, from: ByteIdx, to: ByteIdx) -> bool
    where
        H: Haystack,
    {
        let cow = haystack.substr_from(ByteIdx(0)).unwrap_or_default();
        let input = Input::new(cow.as_ref()).range(from.0..to.0);

        self.inner.is_match(input)
    }

    fn build_match(&self, caps: Captures) -> Match {
        Match {
            n_submatches: caps.group_len(),
            sub_matches: caps
                .iter()
                .flat_map(|maybe_cap| match maybe_cap {
                    Some(cap) => [ByteIdx(cap.start), ByteIdx(cap.end)],
                    None => [ByteIdx(0), ByteIdx(0)],
                })
                .collect(),
            submatch_names: self.submatch_names.clone(),
        }
    }

    /// Search the given [Haystack] for the leftmost longest match of this [Regex] and return the
    /// match position along with all submatches.
    pub fn find<H>(&self, haystack: &H) -> Option<Match>
    where
        H: Haystack,
    {
        self.find_from(haystack, ByteIdx(0))
    }

    /// Search the given [Haystack] for the leftmost longest match of this [Regex] starting from
    /// the provided byte offset rather than the beginning of the haystack, returning
    /// the match position along with all submatches.
    pub fn find_from<H>(&self, haystack: &H, offset: ByteIdx) -> Option<Match>
    where
        H: Haystack,
    {
        let cow = haystack.substr_from(ByteIdx(0)).unwrap_or_default();
        let input = Input::new(cow.as_ref()).range(offset.0..);

        let mut caps = self.inner.create_captures();
        self.inner.captures(input, &mut caps);

        if caps.is_match() {
            Some(self.build_match(caps))
        } else {
            None
        }
    }

    /// Search the given [Haystack] for the leftmost longest match of this [Regex] starting from
    /// the provided `from` byte offset rather than the beginning of the haystack, and ending
    /// before the provided `to` byte offset, returning the match position along with all
    /// submatches.
    pub fn find_between<H>(&self, haystack: &H, from: ByteIdx, to: ByteIdx) -> Option<Match>
    where
        H: Haystack,
    {
        let cow = haystack.substr_from(ByteIdx(0)).unwrap_or_default();
        let input = Input::new(cow.as_ref()).range(from.0..to.0);
        let mut caps = self.inner.create_captures();
        self.inner.captures(input, &mut caps);

        if caps.is_match() {
            Some(self.build_match(caps))
        } else {
            None
        }
    }

    pub fn find_iter<'a, H>(&'a mut self, haystack: &'a H) -> MatchIter<'a, H>
    where
        H: Haystack,
    {
        MatchIter {
            haystack,
            re: self,
            from: ByteIdx(0),
        }
    }
}

/// A reverse regex matcher primarily intended for use inside of the `ad` text editor.
///
/// This matcher provides a somewhat limited public API that only extends to cover the use cases
/// within `ad`.
#[derive(Debug, Clone)]
pub struct RevRegex {
    pattern: Arc<str>,
    rev_dfa: Arc<DFA>,
    fwd_dfa: Arc<DFA>,
}

impl PartialEq for RevRegex {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern
    }
}

impl Eq for RevRegex {}

impl RevRegex {
    pub fn compile(pattern: impl AsRef<str>) -> Result<Self, Error> {
        let hir = build_hir(pattern.as_ref())?;
        let rev_nfa = Compiler::new()
            .configure(
                Config::new()
                    .reverse(true)
                    .which_captures(WhichCaptures::None),
            )
            .build_from_hir(&hir)?;
        let rev_dfa = DFA::builder()
            .configure(DFA::config().unicode_word_boundary(true))
            .build_from_nfa(rev_nfa)?;

        let fwd_nfa = Compiler::new().build_from_hir(&hir)?;
        let fwd_dfa = DFA::builder()
            .configure(
                DFA::config()
                    .match_kind(MatchKind::All)
                    .unicode_word_boundary(true),
            )
            .build_from_nfa(fwd_nfa)?;

        Ok(Self {
            pattern: Arc::from(pattern.as_ref()),
            rev_dfa: Arc::new(rev_dfa),
            fwd_dfa: Arc::new(fwd_dfa),
        })
    }

    /// Executes a reverse search returning the leftmost match that is found. If no match exists,
    /// then `None` is returned.
    pub fn find_rev_from<H>(&self, haystack: &H, offset: ByteIdx) -> Option<Match>
    where
        H: Haystack,
    {
        let cow = haystack.substr_from(ByteIdx(0))?;
        let mut input = Input::new(cow.as_ref()).range(..offset.0);
        let mut cache = self.rev_dfa.create_cache();

        // Run the reverse DFA over the haystack up until the given offset to locate the start of
        // match ending nearest offset.
        let start = self.rev_dfa.try_search_rev(&mut cache, &input).ok()??;
        input.set_start(start.offset());
        input.set_anchored(Anchored::Yes);

        cache = self.fwd_dfa.create_cache();

        // With the start position found, run the forward DFA from that starting point to locate
        // the end of the match.
        let end = self.fwd_dfa.try_search_fwd(&mut cache, &input).ok()??;

        Some(Match::synthetic(
            ByteIdx(start.offset()),
            ByteIdx(end.offset()),
        ))
    }
}

#[expect(clippy::result_large_err)]
fn build_hir(pattern: &str) -> Result<Hir, regex_syntax::Error> {
    let mut ast = Parser::new().parse(pattern)?;
    rewrite_at_meta_char(&mut ast);
    let hir = TranslatorBuilder::new()
        .multi_line(true)
        .build()
        .translate(pattern, &ast)?;

    Ok(hir)
}

/// Replace every unescaped `@` (outside a bracketed class) with `(?s:.)`.
fn rewrite_at_meta_char(ast: &mut Ast) {
    match ast {
        Ast::Literal(lit) if lit.c == '@' && lit.kind == LiteralKind::Verbatim => {
            let span = lit.span;
            *ast = Ast::Group(Box::new(Group {
                span,
                kind: GroupKind::NonCapturing(Flags {
                    span,
                    items: vec![FlagsItem {
                        span,
                        kind: FlagsItemKind::Flag(Flag::DotMatchesNewLine),
                    }],
                }),
                ast: Box::new(Ast::dot(span)),
            }));
        }
        Ast::Repetition(r) => rewrite_at_meta_char(&mut r.ast),
        Ast::Group(g) => rewrite_at_meta_char(&mut g.ast),
        Ast::Alternation(alt) => alt.asts.iter_mut().for_each(rewrite_at_meta_char),
        Ast::Concat(cat) => cat.asts.iter_mut().for_each(rewrite_at_meta_char),
        _ => {} // classes (incl. [@]), dot, assertions, flags, etc
    }
}

mod impl_structex {
    use super::*;
    use crate::buffer::{ByteIdx, View};
    use std::{io, ops::Range};
    use structex::re::{Haystack, RawCaptures, RegexEngine, Sliceable, Writable};

    impl RegexEngine for Regex {
        type CompileError = Error;

        fn compile(re: &str) -> Result<Self, Self::CompileError> {
            Regex::compile(re)
        }
    }

    impl Haystack<Regex> for str {
        fn is_match_between(&self, re: &Regex, from: usize, to: usize) -> bool {
            re.matches_between(&self, ByteIdx(from), ByteIdx(to))
        }

        fn captures_between(&self, re: &Regex, from: usize, to: usize) -> Option<RawCaptures> {
            let m = re.find_between(&self, ByteIdx(from), ByteIdx(to))?;

            Some(RawCaptures::new(m.iter_raw_locs()))
        }
    }

    impl<'a> Haystack<Regex> for View<'a> {
        fn is_match_between(&self, re: &Regex, from: usize, to: usize) -> bool {
            re.matches_between(self, ByteIdx(from), ByteIdx(to))
        }

        fn captures_between(&self, re: &Regex, from: usize, to: usize) -> Option<RawCaptures> {
            let m = re.find_between(self, ByteIdx(from), ByteIdx(to))?;

            Some(RawCaptures::new(m.iter_raw_locs()))
        }
    }

    impl<'a> Sliceable for View<'a> {
        type Slice<'h>
            = &'h str
        where
            Self: 'h;

        fn char_at(&self, byte_offset: usize) -> Option<char> {
            self.gb().get_char_at(ByteIdx(byte_offset))
        }

        fn slice(&self, range: Range<usize>) -> Self::Slice<'_> {
            &self.as_str()[range]
        }

        fn max_len(&self) -> usize {
            self.len()
        }
    }

    impl<'a> Writable for View<'a> {
        fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
        where
            W: io::Write,
        {
            w.write_all(self.as_bytes()).map(|_| self.len())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;
    use simple_test_case::test_case;

    // typos:off
    #[test_case("foo", "foo", Some("foo"); "literal full string")]
    #[test_case("ba*", "baaaaa", Some("baaaaa"); "zero or more present")]
    #[test_case("ba*", "b", Some("b"); "zero or more not present")]
    #[test_case("ba+", "baaaaa", Some("baaaaa"); "one or more present")]
    #[test_case("ba+", "b", None; "one or more not present")]
    #[test_case("b?a", "ba", Some("ba"); "optional present")]
    #[test_case("b?a", "a", Some("a"); "optional not present")]
    #[test_case("a(bb)+a", "abbbba", Some("abbbba"); "article example matching")]
    #[test_case("a(bb)+a", "abbba", None; "article example non matching")]
    #[test_case(".*b", "123b", Some("123b"); "dot star prefix")]
    #[test_case("1.*", "123b", Some("123b"); "dot star suffix")]
    #[test_case("1.*b", "123b", Some("123b"); "dot star inner")]
    #[test_case("(c|C)ase matters", "case matters", Some("case matters"); "alternation first")]
    #[test_case("(c|C)ase matters", "Case matters", Some("Case matters"); "alternation second")]
    #[test_case("(aa|bbb|c|dd)", "c", Some("c"); "chained alternation")]
    #[test_case("this@*works", "this contains\nbut still works", Some("this contains\nbut still works"); "true any")]
    #[test_case(r"literal\?", "literal?", Some("literal?"); "escape special char")]
    #[test_case(r"literal\t", "literal\t", Some("literal\t"); "escape sequence")]
    #[test_case("[abc] happy cow", "a happy cow", Some("a happy cow"); "character class")]
    #[test_case("[^abc] happy cow", "a happy cow", None; "negated character class")]
    #[test_case("[a-zA-Z]*", "camelCaseFtw", Some("camelCaseFtw"); "char class ranges matching")]
    #[test_case("[a-zA-Z]*1", "kebab-case-not-so-much", None; "char class ranges non matching")]
    #[test_case("[a-zA-Z ]*", "this should work", Some("this should work"); "char class mixed")]
    #[test_case("[\\]5]*", "5]]5555]]", Some("5]]5555]]"); "char class escaped bracket")]
    #[test_case("[0-9]+", "0123", Some("0123"); "digit range")]
    #[test_case("[0-9]+", "0", Some("0"); "digit range range start only")]
    #[test_case("25[0-5]", "255", Some("255"); "ipv4 element one")]
    #[test_case("2[0-4][0-9]", "231", Some("231"); "ipv4 element two")]
    #[test_case("1?[0-9]?[0-9]", "155", Some("155"); "ipv4 element three three digit")]
    #[test_case("1?[0-9]?[0-9]", "72", Some("72"); "ipv4 element three two digit")]
    #[test_case("1?[0-9]?[0-9]", "8", Some("8"); "ipv4 element three one digit")]
    #[test_case("1?[0-9]?[0-9]", "0", Some("0"); "ipv4 element three zero")]
    #[test_case("(25[0-5]|2[0-4][0-9])", "255", Some("255"); "ipv4 elements one and two matching one")]
    #[test_case("(25[0-5]|2[0-4][0-9])", "219", Some("219"); "ipv4 elements one and two matching two")]
    #[test_case("(25[0-5]|2[0-4][0-9])", "42", None; "ipv4 elements one and two not matching")]
    #[test_case("(2[0-4][0-9]|1?[0-9]?[0-9])", "237", Some("237"); "ipv4 elements two and three matching two")]
    #[test_case("(2[0-4][0-9]|1?[0-9]?[0-9])", "142", Some("142"); "ipv4 elements two and three matching three")]
    #[test_case("(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])", "251", Some("251"); "ipv4 all elements matching one")]
    #[test_case("(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])", "237", Some("237"); "ipv4 all elements matching two")]
    #[test_case("(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])", "142", Some("142"); "ipv4 all elements matching three")]
    #[test_case(
        r"(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])",
        "127.0.0.1 ",
        Some("127.0.0.1");
        "ipv4 full"
    )]
    #[test_case("^foo", "foo at the start", Some("foo"); "SOL holding")]
    #[test_case("^foo", "bar\nfoo at the start", Some("foo"); "SOL holding after newline")]
    #[test_case("^foo", "we have foo but not at the start", None; "SOL not holding")]
    #[test_case("foo$", "a line that ends with foo", Some("foo"); "BOL holding")]
    #[test_case("foo$", "a line that ends with foo\nnow bar", Some("foo"); "BOL holding before newline")]
    #[test_case("foo$", "a line with foo in the middle", None; "BOL not holding")]
    #[test_case("foo", "│foo", Some("foo"); "after a multibyte char")]
    #[test_case("a{3}", "aaa", Some("aaa"); "counted repetition")]
    #[test_case("a{3}", "aa", None; "counted repetition non matching")]
    #[test_case("a{3,}", "aaaaaa", Some("aaaaaa"); "counted repetition at least")]
    #[test_case("a{3,}", "aa", None; "counted repetition at least non matching")]
    #[test_case("a{3,5}", "aaa", Some("aaa"); "counted repetition between lower")]
    #[test_case("a{3,5}", "aaaaa", Some("aaaaa"); "counted repetition between upper")]
    #[test_case("a{3,5}", "aaaa", Some("aaaa"); "counted repetition in range")]
    #[test_case("a{3,5}", "aa", None; "counted repetition less")]
    #[test_case("^a{3,5}$", "aaaaaa", None; "counted repetition more")]
    #[test_case("\\b\\w+\\b", "foo", Some("foo"); "word boundary at end of input")]
    #[test_case("\\bfor\\b", "forward", None; "word boundary for match at start of word")]
    #[test_case("\\bfor\\b", "for ward", Some("for"); "word boundary for match not inside word")]
    #[test_case("\\bfor\\b", "bob for", Some("for"); "word boundary match not at BOF")]
    #[test_case("\\bfor\\b", "bob for bob", Some("for"); "word boundary match not at BOF or EOF")]
    #[test_case("\\bin\\b", "min", None; "word boundary for match at end of word")]
    #[test_case("\\b(in)\\b", "min", None; "word boundary for sub expression match at end of word")]
    #[test_case("\\b(in|for)\\b", "min", None; "word boundary for alt match at end of word")]
    #[test_case("\\b(in|for)\\b", "bob for", Some("for"); "word boundary for alt match not at BOF")]
    #[test_case("[a-zA-Z0-9_\\-./@]+\\.jpe?g", "glenda_space_medium.jpg", Some("glenda_space_medium.jpg"); "complex group")]
    #[test_case("[a-zA-Z¡-￿0-9_\\-./@]+", "foo-bar_99.pdf", Some("foo-bar_99.pdf"); "multibyte group")]
    // typos:on
    #[test]
    fn find_works(pattern: &str, s: &str, expected: Option<&str>) {
        let re = Regex::compile(pattern).unwrap();
        let m = re.find(&s).map(|m| m.match_text(&s));
        assert_eq!(m.as_deref(), expected);
    }

    #[test_case("foo", "foo", Some("foo"); "literal full string")]
    #[test_case("ba*", " baaaaa foo", Some("baaaaa"); "zero or more present")] // typos:ignore
    #[test_case("ba*", "b foo", Some("b"); "zero or more not present")] // typos:ignore
    #[test_case("foo$", "a line that ends with foo\nnow bar", Some("foo"); "EOL holding before newline")]
    #[test_case("\\b\\w+\\b", "foo", Some("foo"); "word boundary at end of input")]
    #[test_case(
        r"(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])",
        "127.0.0.1 ",
        Some("127.0.0.1");
        "ipv4 full"
    )]
    #[test_case(
        "his",
        "this is a line\nand another\n- [ ] something to do\n",
        Some("his");
        "multiline input"
    )]
    #[test]
    fn find_rev_works(pattern: &str, s: &str, expected: Option<&str>) {
        let re = RevRegex::compile(pattern).unwrap();
        let b = Buffer::new_unnamed(0, s, Default::default());
        let m = re
            .find_rev_from(&b, ByteIdx(s.len()))
            .map(|m| m.match_text(&b));

        assert_eq!(m.as_deref(), expected);
    }

    #[test_case("[0-9]+", "|42 3 127 9991|", &["42", "3", "127", "9991"]; "integers")]
    #[test_case("[0-9]+", "|42 3 127 9991", &["42", "3", "127", "9991"]; "integers to EOF")]
    #[test_case("[0-9]+", "42 3 127 9991|", &["42", "3", "127", "9991"]; "integers from BOF")]
    #[test_case("[0-9]+", "42 3 127 9991", &["42", "3", "127", "9991"]; "integers full input")]
    #[test_case("foo|bar|baz", "baz bar foo bar", &["baz", "bar", "foo", "bar"]; "alts spaced in s")]
    #[test_case("foo|bar|baz", "bazbarfoobar", &["baz", "bar", "foo", "bar"]; "alts back to back in s")]
    #[test_case("(foo|bar|baz)", "foo foobar barfoo baz", &["foo", "foo", "bar", "bar", "foo", "baz"]; "alts in parens")]
    #[test_case("\\b(foo|bar|baz)\\b", "foo foobar barfoo baz", &["foo", "baz"]; "alts with word boundaries")]
    #[test_case("\\b", "foo bar", &["", "", "", ""]; "zero-length word boundary")]
    #[test]
    fn find_iter_works(pattern: &str, s: &str, expected: &[&str]) {
        let mut re = Regex::compile(pattern).unwrap();
        let matches: Vec<String> = re
            .find_iter(&s)
            .map(|m| m.match_text(&s).into_owned())
            .collect();

        assert_eq!(&matches, expected);
    }

    #[test]
    fn match_extraction_works() {
        let re = "([0-9]+)-([0-9]+)-([0-9]+)";
        let r = Regex::compile(re).unwrap();
        let s = "this should work 123-456-789 other stuff";
        let m = r.find(&s).unwrap();

        assert_eq!(m.match_text(&s), "123-456-789");
        assert_eq!(m.submatch_text(1, &s).as_deref(), Some("123"));
        assert_eq!(m.submatch_text(2, &s).as_deref(), Some("456"));
        assert_eq!(m.submatch_text(3, &s).as_deref(), Some("789"));
    }

    #[test_case("(?<xy>X|Y)", "xy", "X"; "named match on its own")]
    #[test_case("(?<xy>X|Y)(a|b)", "xy", "X"; "named match before unnamed")]
    #[test_case("(e| )(?<xy>X|Y)", "xy", "X"; "named match after unnamed")]
    #[test_case("(e| )(?<xy>X|Y)(a|b)", "xy", "X"; "named match inbetween unnamed")]
    #[test]
    fn named_submatch_works(re: &str, name: &str, expected: &str) {
        let r = Regex::compile(re).unwrap();
        let s = "text before Xanadu";
        let m = r.find(&s).unwrap();

        assert_eq!(m.submatch_text_by_name(name, &s).as_deref(), Some(expected));
    }

    #[test]
    fn match_extraction_works_when_multibyte_characters_are_present() {
        let s: &str = "const VLINE: char = '│';

impl Editor {
";

        let re = r"impl (\w+) \{";
        let r = Regex::compile(re).unwrap();
        let m = r.find(&s).unwrap();

        assert_eq!(m.submatch_text(1, &s).as_deref(), Some("Editor"));
        assert_eq!(m.match_text(&s), "impl Editor {");
    }
}
