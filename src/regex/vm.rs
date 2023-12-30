//! Virtual machine based implementation based on the instruction set described
//! in Russ Cox's second article in the series and the source of plan9 Sam:
//!   https://swtch.com/~rsc/regexp/regexp2.html
//!   https://github.com/sminez/plan9port/blob/master/src/cmd/sam/regexp.c
//!
//! The compilation step used is custom (rather than using a YACC parser).
//!
//! We make use of pre-allocated buffers for the Thread lists and track the
//! index we are up to per-iteration as this results in roughly a 100x speed
//! up from not having to allocate and free inside of the main loop.
use super::{
    ast::{parse, Assertion},
    compile::{compile_ast, optimise, CompiledOps, Inst, Op, Prog},
    matches::{Match, MatchIter},
    Error,
};
use crate::buffer::{Buffer, GapBuffer};
use std::{mem::swap, rc::Rc};

/// A regular expression engine designed for use within the ad text editor.
///
/// This is a relatively naive implementation though it does have some
/// optimisations and runs reasonably quickly. It is not at all designed to
/// be robust against mallicious input and it does not attempt to support
/// full PCRE syntax or functionality.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Regex {
    /// The compiled instructions for running the VM
    prog: Prog,
    /// Names to be used for extracting named submatches
    submatch_names: Rc<[String]>,
    /// Pre-allocated Thread list in priority order to handle leftmost-longest semantics
    clist: Box<[Thread]>,
    /// Pre-allocated Thread list in priority order to handle leftmost-longest semantics
    nlist: Box<[Thread]>,
    /// Pre-allocated SubMatch positions referenced by threads
    sms: Box<[SubMatches]>,
    /// Available indicies into self.sms for storing SubMatch positions for new threads
    free_sms: Vec<usize>,
    track_submatches: bool,
    /// Monotonically increasing index used to dedup Threads
    /// Will overflow at some point if a given regex is used a VERY large number of times
    gen: usize,
    /// Index into the current Thread list
    p: usize,
    /// Previous character from the input
    prev: Option<char>,
    /// Next character in the input after the one currently being processed
    next: Option<char>,
}

impl Regex {
    /// Attempt to compile the given regular expression into its optimised VM opcode form.
    ///
    /// This method handles pre-allocation of the memory required for running the VM so
    /// that the allocation cost is paid once up front rather than on each use of the Regex.
    pub fn compile(re: &str) -> Result<Self, Error> {
        let mut ast = parse(re)?;
        ast.optimise();
        let CompiledOps {
            ops,
            submatch_names,
        } = compile_ast(ast, false);

        Ok(Self::new(ops, submatch_names))
    }

    /// Attempt to compile the given regular expression into its reversed optimised VM opcode form.
    /// This is used for searching backwards through an input stream.
    ///
    /// This method handles pre-allocation of the memory required for running the VM so
    /// that the allocation cost is paid once up front rather than on each use of the Regex.
    pub fn compile_reverse(re: &str) -> Result<Self, Error> {
        let mut ast = parse(re)?;
        ast.optimise();
        let CompiledOps {
            ops,
            submatch_names,
        } = compile_ast(ast, true);

        Ok(Self::new(ops, submatch_names))
    }

    fn new(ops: Vec<Op>, submatch_names: Vec<String>) -> Self {
        let prog: Prog = optimise(ops)
            .into_iter()
            .map(|op| Inst { op, gen: 0 })
            .collect();

        let clist = vec![Thread::default(); prog.len()].into_boxed_slice();
        let nlist = vec![Thread::default(); prog.len()].into_boxed_slice();
        let sms = vec![SubMatches::default(); prog.len()].into_boxed_slice();
        let free_sms = (1..prog.len()).collect();

        Self {
            prog,
            submatch_names: Rc::from(submatch_names.into_boxed_slice()),
            clist,
            nlist,
            gen: 0,
            p: 0,
            prev: None,
            next: None,

            sms,
            free_sms,
            track_submatches: true,
        }
    }

    /// Attempt to match this Regex against a given `&str` input, returning the position
    /// of the match and all submatches if successful.
    pub fn match_str(&mut self, input: &str) -> Option<Match> {
        self.track_submatches = true;
        self.match_iter(&mut input.chars().enumerate(), 0)
    }

    /// Iterate over all non-overlapping matches of this Regex for a given `&str` input.
    pub fn match_str_all<'a, 'b>(&'a mut self, input: &'b str) -> MatchIter<'a, &'b str> {
        self.track_submatches = true;
        MatchIter {
            it: input,
            r: self,
            from: 0,
        }
    }

    /// Iterate over all non-overlapping matches of this Regex for a given `Buffer` input.
    pub fn match_buffer_all<'a, 'b>(&'a mut self, b: &'b Buffer) -> MatchIter<'a, &'b GapBuffer> {
        self.track_submatches = true;
        MatchIter {
            it: &b.txt,
            r: self,
            from: 0,
        }
    }

    /// Attempt to match this Regex against an arbitrary iterator input, returning the
    /// position of the match and all submatches if successful.
    pub fn match_iter<I>(&mut self, input: &mut I, sp: usize) -> Option<Match>
    where
        I: Iterator<Item = (usize, char)>,
    {
        self.track_submatches = true;
        self._match_iter(input, sp)
    }

    /// Determine whether or not this Regex matches the input `&str` without searching for
    /// the leftmost-longest match and associated submatch boundaries.
    pub fn matches_str(&mut self, input: &str) -> bool {
        self.track_submatches = false;
        self.matches_iter(&mut input.chars().enumerate(), 0)
    }

    /// Determine whether or not this Regex matches the input iterator without searching
    /// for the leftmost-longest match and associated submatch boundaries.
    pub fn matches_iter<I>(&mut self, input: &mut I, sp: usize) -> bool
    where
        I: Iterator<Item = (usize, char)>,
    {
        self.track_submatches = false;
        self._match_iter(input, sp).is_some()
    }

    /// This is the main VM implementation that is used by all other matching methods on Regex.
    ///
    /// The `return_on_first_match` flag is used to early return a dummy Match as soon as we
    /// can tell that the given regular expression matches the input (rather than looking for
    /// the leftmost-longest match).
    /// >> The Match returned in this case will always point to the null string at the start
    ///    of the string and should only be used for conversion to a bool in `matches_*`
    ///    methods.
    fn _match_iter<I>(&mut self, input: &mut I, mut sp: usize) -> Option<Match>
    where
        I: Iterator<Item = (usize, char)>,
    {
        let mut sub_matches = [0; 20];
        self.free_sms = (1..self.prog.len()).collect();
        self.sms[0] = SubMatches {
            refs: 1,
            inner: [0; 20],
        };

        // We bump the generation to ensure we don't collide with anything from
        // a previous run while initialising the VM.
        self.gen += 1;
        // When setting up the initial threads we have our prelude which uses "@" so we provide a
        // null byte for the initial character as it is not needed and it avoids us having to make
        // the "ch" param of add_thread optional.
        self.add_thread(Thread::default(), sp, '\0', true);
        swap(&mut self.clist, &mut self.nlist);
        self.gen += 1;

        // Same as at the end of the outer for-loop, we need to reset self.p to 0
        // so that we are correctly tracking the length of the new nlist.
        let mut n = self.p;
        self.p = 0;
        let mut matched = false;

        let mut it = input.peekable();
        self.prev = None;
        self.next = None;

        while let Some((i, ch)) = it.next() {
            sp = i;
            self.next = it.peek().map(|(_, c)| *c);

            for i in 0..n {
                if let Some(sm) = self.step_thread(i, sp, ch) {
                    if !self.track_submatches {
                        return Some(Match::synthetic(0, 0));
                    }

                    matched = true;
                    sub_matches = self.sms[sm].inner;

                    // We're ending this thread and all others that have lower priority
                    // so decrement the references they have to their submatches
                    for j in i..n {
                        self.sm_dec_ref(self.clist[j].sm);
                    }

                    break;
                }
            }

            swap(&mut self.clist, &mut self.nlist);
            self.prev = Some(ch);
            self.gen += 1;
            n = self.p;

            if self.p == 0 {
                break;
            }

            self.p = 0;
        }

        self.prev = None;
        self.next = None;

        // Check to see if the final pass had a match which would be better than any
        // that we have so far.
        for t in self.clist.iter_mut().take(n) {
            if self.prog[t.pc].op == Op::Match && self.sms[t.sm].inner[1] >= sub_matches[1] {
                matched = true;
                sub_matches = self.sms[t.sm].inner;
                break;
            }
        }

        if !matched {
            return None;
        }

        Some(Match {
            sub_matches,
            submatch_names: self.submatch_names.clone(),
        })
    }

    #[inline]
    fn step_thread(&mut self, i: usize, sp: usize, ch: char) -> Option<usize> {
        let t = &self.clist[i];
        match &self.prog[t.pc].op {
            // If comparisons and their assertions hold then queue the resulting threads
            Op::Comp(comp) if comp.matches(ch) => match t.assertion {
                Some(a) if !a.holds_for(self.prev, ch, self.next) => {
                    self.sm_dec_ref(t.sm);
                    return None;
                }
                _ => self.add_thread(thread(t.pc + 1, t.sm), sp, ch, false),
            },

            Op::Match => return Some(t.sm),

            // Save, Jump & Split are handled in add_thread.
            // Non-matching comparison ops result in that thread dying.
            _ => self.sm_dec_ref(t.sm),
        }

        None
    }

    #[inline]
    fn add_thread(&mut self, t: Thread, sp: usize, ch: char, initial: bool) {
        if self.prog[t.pc].gen == self.gen {
            self.sm_dec_ref(t.sm);
            return; // already on the list we are currently building
        }
        self.prog[t.pc].gen = self.gen;

        // We do this as chained if-let as we need to recursively call add_thread with data
        // from self.prog but add_thread required &mut self, so matching would mean we had
        // to Clone as Op::Class does not implement Copy.
        // > This is faster than cloning the op and matching
        if let Op::Jump(l1) = self.prog[t.pc].op {
            let th = match t.assertion {
                Some(a) => assert_thread(l1, t.sm, a),
                None => thread(l1, t.sm),
            };
            self.add_thread(th, sp, ch, initial);
        } else if let Op::Split(l1, l2) = self.prog[t.pc].op {
            self.sms[t.sm].refs += 1;
            let (t1, t2) = match t.assertion {
                Some(a) => (assert_thread(l1, t.sm, a), assert_thread(l2, t.sm, a)),
                None => (thread(l1, t.sm), thread(l2, t.sm)),
            };
            self.add_thread(t1, sp, ch, initial);
            self.add_thread(t2, sp, ch, initial);
        } else if let Op::Assertion(a) = self.prog[t.pc].op {
            self.add_thread(assert_thread(t.pc + 1, t.sm, a), sp, ch, initial);
        } else if let Op::Save(s) = self.prog[t.pc].op {
            self.handle_save(t, s, sp, ch, initial, false)
        } else if let Op::RSave(s) = self.prog[t.pc].op {
            self.handle_save(t, s, sp, ch, initial, true)
        } else {
            self.nlist[self.p] = t;
            self.p += 1;
        }
    }

    #[inline]
    fn handle_save(&mut self, t: Thread, s: usize, sp: usize, ch: char, initial: bool, rev: bool) {
        if (!rev && s % 2 == 0) || (rev && s % 2 == 1) {
            let sm = self.sm_update(t.sm, s, sp, initial, rev);
            let th = match t.assertion {
                Some(a) => assert_thread(t.pc + 1, sm, a),
                None => thread(t.pc + 1, sm),
            };
            self.add_thread(th, sp, ch, initial);
        } else {
            match t.assertion {
                Some(a) if !a.holds_for(self.prev, ch, self.next) => self.sm_dec_ref(t.sm),
                _ => {
                    let sm = self.sm_update(t.sm, s, sp, initial, rev);
                    self.add_thread(thread(t.pc + 1, sm), sp, ch, initial);
                }
            }
        }
    }

    #[inline]
    fn sm_dec_ref(&mut self, i: usize) {
        if !self.track_submatches {
            return;
        }

        self.sms[i].refs -= 1;
        if self.sms[i].refs == 0 {
            self.free_sms.push(i);
        }
    }

    #[inline]
    fn sm_update(&mut self, i: usize, s: usize, sp: usize, initial: bool, reverse: bool) -> usize {
        // We don't hard error on compiling a regex with more than 9 submatches
        // but we don't track anything past the 9th
        if !self.track_submatches || s >= 20 {
            return i;
        }

        let i = if self.sms[i].refs == 1 {
            i
        } else {
            self.sm_dec_ref(i);
            let j = self.free_sms.swap_remove(0);
            self.sms[j].inner = self.sms[i].inner;
            self.sms[j].refs = 1;
            j
        };

        // If we are saving our initial position then we are looking at the correct
        // character, otherwise the Save op is being processed at the character
        // before the one we need to save.
        let mut val = if !initial { sp + 1 } else { sp };
        if reverse && !initial {
            val = val.saturating_sub(1);
        }

        self.sms[i].inner[s] = val;

        i
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct SubMatches {
    /// How many threads are currently pointing at this SubMatches
    refs: usize,
    /// $0 -> $9 submatches with $0 being the full match
    /// for submatch $k the start index is 2$k and the end is 2$k+1
    inner: [usize; 20],
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Thread {
    /// VM program counter for this thread
    pc: usize,
    /// An assertion that must hold for this instruction to be runnable
    assertion: Option<Assertion>,
    /// Index into the Regex sms field
    sm: usize,
}

#[inline]
fn thread(pc: usize, sm: usize) -> Thread {
    Thread {
        pc,
        sm,
        assertion: None,
    }
}

#[inline]
fn assert_thread(pc: usize, sm: usize, a: Assertion) -> Thread {
    Thread {
        pc,
        sm,
        assertion: Some(a),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

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
    #[test]
    fn match_works(re: &str, s: &str, expected: Option<&str>) {
        let mut r = Regex::compile(re).unwrap();
        let m = r.match_str(s).map(|m| m.str_match_text(s));
        assert_eq!(m.as_deref(), expected);
    }

    #[test_case("foo", "foo", Some("foo"); "literal full string")]
    #[test_case("ba*", " baaaaa foo", Some("baaaaa"); "zero or more present")]
    #[test_case("ba*", "b foo", Some("b"); "zero or more not present")]
    #[test_case("foo$", "a line that ends with foo\nnow bar", Some("foo"); "BOL holding before newline")]
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
        "multiline intput"
    )]
    #[test]
    fn rev_match_works(re: &str, s: &str, expected: Option<&str>) {
        use crate::exec::IterBoundedChars;

        let mut r = Regex::compile_reverse(re).unwrap();
        let b = Buffer::new_unnamed(0, s);
        let mut it = b.rev_iter_between(s.len(), 0);
        let m = r
            .match_iter(&mut it, s.len())
            .map(|m| m.str_match_text(&b.txt.to_string()).to_string());

        assert_eq!(m.as_deref(), expected);
    }

    #[test_case("[0-9]+", " 42 3 127 9991 ", &["42", "3", "127", "9991"]; "integers")]
    #[test_case("[0-9]+", " 42 3 127 9991", &["42", "3", "127", "9991"]; "integers to EOF")]
    #[test_case("[0-9]+", "42 3 127 9991 ", &["42", "3", "127", "9991"]; "integers from BOF")]
    #[test_case("[0-9]+", "42 3 127 9991", &["42", "3", "127", "9991"]; "integers full input")]
    #[test_case("foo|bar|baz", "baz bar foo bar", &["baz", "bar", "foo", "bar"]; "alts spaced in s")]
    #[test_case("foo|bar|baz", "bazbarfoobar", &["baz", "bar", "foo", "bar"]; "alts back to back in s")]
    #[test_case("(foo|bar|baz)", "foo foobar barfoo baz", &["foo", "foo", "bar", "bar", "foo", "baz"]; "alts in parens")]
    #[test_case("\\b(foo|bar|baz)\\b", "foo foobar barfoo baz", &["foo", "baz"]; "alts with word boundaries")]
    #[test]
    fn match_all_works(re: &str, s: &str, expected: &[&str]) {
        let mut r = Regex::compile(re).unwrap();
        let matches: Vec<String> = r.match_str_all(s).map(|m| m.str_match_text(s)).collect();

        assert_eq!(&matches, expected);
    }

    #[test]
    fn dot_star_works() {
        let mut r = Regex::compile(".*").unwrap();
        let s = "\nthis is\na multiline\nfile";
        let m1 = r.match_str(s).unwrap();
        assert_eq!(m1.str_match_text(s), "");

        // Skipping the leading newline should cause us to match all of the following line
        let m2 = r.match_str(&s[1..]).unwrap();
        assert_eq!(m2.str_match_text(&s[1..]), "this is");
    }

    #[test]
    fn match_extraction_works() {
        let re = "([0-9]+)-([0-9]+)-([0-9]+)";
        let mut r = Regex::compile(re).unwrap();
        let s = "this should work 123-456-789 other stuff";
        let m = r.match_str(s).unwrap();

        assert_eq!(m.str_submatch_text(1, s).as_deref(), Some("123"));
        assert_eq!(m.str_submatch_text(2, s).as_deref(), Some("456"));
        assert_eq!(m.str_submatch_text(3, s).as_deref(), Some("789"));
        assert_eq!(m.str_match_text(s), "123-456-789");
    }

    #[test_case("(?<xy>X|Y)", "xy", "X"; "named match on its own")]
    #[test_case("(?<xy>X|Y)(a|b)", "xy", "X"; "named match before unnamed")]
    #[test_case("(e| )(?<xy>X|Y)", "xy", "X"; "named match after unnamed")]
    #[test_case("(e| )(?<xy>X|Y)(a|b)", "xy", "X"; "named match inbetween unnamed")]
    #[test]
    fn named_submatch_works(re: &str, name: &str, expected: &str) {
        let mut r = Regex::compile(re).unwrap();
        let s = "text before Xanadu";
        let m = r.match_str(s).unwrap();

        assert_eq!(m.named_matches(), vec![name]);
        assert_eq!(m.str_sub_loc_text_ref_by_name(name, s), Some(expected));
    }

    #[test]
    fn multiline_input_match_dot_star_works() {
        let mut r = Regex::compile(".*").unwrap();
        let s = "this is\na multiline\nfile";

        let m = r.match_str(s).unwrap();
        assert_eq!(m.str_match_text(s), "this is");
    }

    #[test]
    fn multiline_input_match_dot_star_works_with_non_zero_initial_sp() {
        let mut r = Regex::compile(".*").unwrap();
        let s = "this is\na multiline\nfile";

        // Just to convince me that the offsets here are exactly as I am expecting
        assert_eq!(s.chars().skip(7).collect::<String>(), "\na multiline\nfile");

        let m1 = r.match_iter(&mut s.chars().enumerate().skip(7), 7).unwrap();
        assert_eq!(m1.str_match_text(s), "");

        let m2 = r.match_iter(&mut s.chars().enumerate().skip(8), 8).unwrap();
        assert_eq!(m2.str_match_text(s), "a multiline");
    }

    #[test]
    fn multiline_input_match_all_dot_star_works() {
        let mut r = Regex::compile(".*").unwrap();
        let s = "this is\na multiline\nfile";

        let mut it = r.match_str_all(s);

        // written this way rather than using collect as if we introduce a bug in the MatchIter
        // impl we can end up with an iterator that gets stuck and never terminates.
        let m1 = it.next().unwrap();
        assert_eq!(m1.str_match_text(s), "this is");

        let m2 = it.next().unwrap();
        assert_eq!(m2.str_match_text(s), "");

        let m3 = it.next().unwrap();
        assert_eq!(m3.str_match_text(s), "a multiline");

        let m4 = it.next().unwrap();
        assert_eq!(m4.str_match_text(s), "");

        let m5 = it.next().unwrap();
        assert_eq!(m5.str_match_text(s), "file");
        assert_eq!(it.next(), None);
    }

    #[test]
    fn match_extraction_works_when_multibyte_characters_are_present() {
        let s: &str = "const VLINE: char = '│';

impl Editor {
";

        let re = r"impl (\w+) \{";
        let mut r = Regex::compile(re).unwrap();
        let m = r.match_str(s).unwrap();

        assert_eq!(m.str_submatch_text(1, s).as_deref(), Some("Editor"));
        assert_eq!(m.str_match_text(s), "impl Editor {");
    }

    // This is the pathological case that Cox covers in his article which leads
    // to exponential behaviour in backtracking based implementations.
    #[test]
    fn pathological_match_doesnt_explode() {
        let s = "a".repeat(100);
        let mut re = "a?".repeat(100);
        re.push_str(&s);

        let mut r = Regex::compile(&re).unwrap();
        assert!(r.match_str(&s).is_some());
    }

    // Make sure that the previous cached state for a given Regex doesn't cause
    // any strange behaviour for future matches
    #[test]
    fn repeated_match_works() {
        let re = "a(bb)+a";

        let mut r = Regex::compile(re).unwrap();
        for _ in 0..10 {
            assert!(r.match_str("abbbba").is_some());
            assert!(r.match_str("foo").is_none());
        }
    }
}
