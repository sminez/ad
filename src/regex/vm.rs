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
    compile::{compile, optimise, Assertion, Inst, Op, Prog},
    matches::{Match, MatchIter},
    re_to_postfix, Error,
};
use crate::buffer::Buffer;
use ropey::Rope;
use std::mem::take;

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
    /// Pre-allocated Thread list in priority order to handle leftmost-longest semantics
    clist: Vec<Thread>,
    /// Pre-allocated Thread list in priority order to handle leftmost-longest semantics
    nlist: Vec<Thread>,
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
        let pfix = re_to_postfix(re)?;
        let ops = optimise(compile(pfix));
        let prog: Prog = ops.into_iter().map(|op| Inst { op, gen: 0 }).collect();

        let clist = vec![Thread::default(); prog.len()];
        let nlist = vec![Thread::default(); prog.len()];

        Ok(Self {
            prog,
            clist,
            nlist,
            gen: 0,
            p: 0,
            prev: None,
            next: None,
        })
    }

    /// Attempt to match this Regex against a given `&str` input, returning the position
    /// of the match and all submatches if successful.
    pub fn match_str(&mut self, input: &str) -> Option<Match> {
        self.match_iter(&mut input.chars().enumerate(), 0)
    }

    /// Iterate over all non-overlapping matches of this Regex for a given `&str` input.
    pub fn match_str_all<'a, 'b>(&'a mut self, input: &'b str) -> MatchIter<'a, &'b str> {
        MatchIter {
            it: input,
            r: self,
            from: 0,
        }
    }

    /// Attempt to match this Regex against a given `Rope` input, returning the position
    /// of the match and all submatches if successful.
    pub fn match_rope(&mut self, r: &Rope) -> Option<Match> {
        self.match_iter(&mut r.chars().enumerate(), 0)
    }

    /// Iterate over all non-overlapping matches of this Regex for a given `Rope` input.
    pub fn match_rope_all<'a, 'b>(&'a mut self, r: &'b Rope) -> MatchIter<'a, &'b Rope> {
        MatchIter {
            it: r,
            r: self,
            from: 0,
        }
    }

    /// Iterate over all non-overlapping matches of this Regex for a given `Buffer` input.
    pub fn match_buffer_all<'a, 'b>(&'a mut self, b: &'b Buffer) -> MatchIter<'a, &'b Rope> {
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
        self._match_iter(input, sp, false)
    }

    /// Determine whether or not this Regex matches the input `&str` without searching for
    /// the leftmost-longest match and associated submatch boundaries.
    pub fn matches_str(&mut self, input: &str) -> bool {
        self.matches_iter(&mut input.chars().enumerate(), 0)
    }

    /// Determine whether or not this Regex matches the input `Rope` without searching for
    /// the leftmost-longest match and associated submatch boundaries.
    pub fn matches_rope(&mut self, r: &Rope) -> bool {
        self.matches_iter(&mut r.chars().enumerate(), 0)
    }

    /// Determine whether or not this Regex matches the input iterator without searching
    /// for the leftmost-longest match and associated submatch boundaries.
    pub fn matches_iter<I>(&mut self, input: &mut I, sp: usize) -> bool
    where
        I: Iterator<Item = (usize, char)>,
    {
        self._match_iter(input, sp, true).is_some()
    }

    /// This is the main VM implementation that is used by all other matching methods on Regex.
    ///
    /// The `return_on_first_match` flag is used to early return a dummy Match as soon as we
    /// can tell that the given regular expression matches the input (rather than looking for
    /// the leftmost-longest match).
    /// >> The Match returned in this case will always point to the null string at the start
    ///    of the string and should only be used for conversion to a bool in `matches_*`
    ///    methods.
    fn _match_iter<I>(
        &mut self,
        input: &mut I,
        mut sp: usize,
        return_on_first_match: bool,
    ) -> Option<Match>
    where
        I: Iterator<Item = (usize, char)>,
    {
        let mut clist = take(&mut self.clist);
        let mut nlist = take(&mut self.nlist);
        let mut sub_matches = [0; 20];

        // We bump the generation to ensure we don't collide with anything from
        // a previous run while initialising the VM.
        self.gen += 1;
        self.add_thread(&mut clist, Thread::default(), sp, true);
        self.gen += 1;

        // Same as at the end of the outer for-loop, we need to reset self.p to 0
        // so that we are correctly tracking the length of the new nlist.
        let mut n = self.p;
        self.p = 0;
        let mut matched = false;
        let mut did_break = false;

        let mut it = input.peekable();
        self.prev = None;
        self.next = None;

        while let Some((i, ch)) = it.next() {
            sp = i;
            self.next = it.peek().map(|(_, c)| *c);

            for t in clist.iter().take(n) {
                if let Some(sms) = self.step_thread(t, &mut nlist, sp, ch) {
                    if return_on_first_match {
                        return Some(Match::synthetic(0, 0));
                    }

                    matched = true;
                    sub_matches = sms;
                    sub_matches[1] = sp; // Save end of the match
                    break;
                }
            }

            (clist, nlist) = (nlist, clist);
            self.prev = Some(ch);
            self.gen += 1;

            if self.p == 0 {
                did_break = true;
                break;
            }

            n = self.p;
            self.p = 0;
        }

        self.clist = clist;
        self.nlist = nlist;
        self.prev = None;
        self.next = None;

        if !matched {
            for t in self.clist.iter_mut().take(n) {
                if self.prog[t.pc].op == Op::Match {
                    t.sub_matches[1] = sp; // Save end of the match
                    if t.sub_matches[1] >= t.sub_matches[0] {
                        return Some(Match {
                            sub_matches: t.sub_matches,
                        });
                    }
                }
            }

            return None;
        }

        // If we broke before the end of input when matching then the index we have for
        // the end of the match is one character too far so we need to back it up.
        if did_break {
            sub_matches[1] -= 1;
        }

        Some(Match { sub_matches })
    }

    fn step_thread(
        &mut self,
        t: &Thread,
        nlist: &mut [Thread],
        sp: usize,
        ch: char,
    ) -> Option<[usize; 20]> {
        match &self.prog[t.pc].op {
            // If comparisons and their assertions hold then queue the resulting threads
            op @ (Op::Char(_) | Op::Class(_) | Op::Any | Op::TrueAny) if op.matches(ch) => {
                match t.assertion {
                    Some(a) if !a.holds_for(self.prev, self.next) => return None,
                    _ => self.add_thread(nlist, thread(t.pc + 1, t.sub_matches), sp, false),
                }
            }

            Op::Match => return Some(t.sub_matches),

            // Save, Jump & Split are handled in add_thread.
            // Non-matching comparison ops result in that thread dying.
            _ => (),
        }

        None
    }

    fn add_thread(&mut self, lst: &mut [Thread], mut t: Thread, sp: usize, initial: bool) {
        if self.prog[t.pc].gen == self.gen {
            return; // already on the list we are currently building
        }
        self.prog[t.pc].gen = self.gen;

        // We do this as chained if-let as we need to recursively call add_thread with data
        // from self.prog but add_thread required &mut self, so matching would mean we had
        // to Clone as Op::Class does not implement Copy
        if let Op::Jump(l1) = self.prog[t.pc].op {
            self.add_thread(lst, thread(l1, t.sub_matches), sp, initial);
        } else if let Op::Split(l1, l2) = self.prog[t.pc].op {
            self.add_thread(lst, thread(l1, t.sub_matches), sp, initial);
            self.add_thread(lst, thread(l2, t.sub_matches), sp, initial);
        } else if let Op::Assertion(a) = self.prog[t.pc].op {
            self.add_thread(lst, assert_thread(t.pc + 1, t.sub_matches, a), sp, initial);
        } else if let Op::Save(s) = self.prog[t.pc].op {
            match t.assertion {
                Some(a) if !a.holds_for(self.prev, self.next) => (),
                _ => {
                    // Save start is for the NEXT char and Save end is for CURRENT char
                    t.sub_matches[s] = if s % 2 == 0 && !initial { sp + 1 } else { sp };
                    self.add_thread(lst, thread(t.pc + 1, t.sub_matches), sp, initial);
                }
            }
        } else {
            lst[self.p] = t;
            self.p += 1;
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Thread {
    /// VM program counter for this thread
    pc: usize,
    /// An assertion that must hold for this instruction to be runnable
    assertion: Option<Assertion>,
    /// $0 -> $9 submatches with $0 being the full match
    /// for submatch $k the start index is 2$k and the end is 2$k+1
    sub_matches: [usize; 20],
}

fn thread(pc: usize, sub_matches: [usize; 20]) -> Thread {
    Thread {
        pc,
        sub_matches,
        assertion: None,
    }
}

fn assert_thread(pc: usize, sub_matches: [usize; 20], a: Assertion) -> Thread {
    Thread {
        pc,
        sub_matches,
        assertion: Some(a),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test_case("ba*", "baaaaa", true; "zero or more present")]
    #[test_case("ba*", "b", true; "zero or more not present")]
    #[test_case("ba+", "baaaaa", true; "one or more present")]
    #[test_case("ba+", "b", false; "one or more not present")]
    #[test_case("b?a", "ba", true; "optional present")]
    #[test_case("b?a", "a", true; "optional not present")]
    #[test_case("a(bb)+a", "abbbba", true; "article example matching")]
    #[test_case("a(bb)+a", "abbba", false; "article example non matching")]
    #[test_case(".*b", "123b", true; "dot star prefix")]
    #[test_case("1.*", "123b", true; "dot star suffix")]
    #[test_case("1.*b", "123b", true; "dot star inner")]
    #[test_case("(c|C)ase matters", "case matters", true; "alternation first")]
    #[test_case("(c|C)ase matters", "Case matters", true; "alternation second")]
    #[test_case("(a|b|c)", "c", true; "chained alternation")]
    #[test_case("this@*works", "this contains\nbut still works", true; "true any")]
    #[test_case(r"literal\?", "literal?", true; "escape special char")]
    #[test_case(r"literal\t", "literal\t", true; "escape sequence")]
    #[test_case("[abc] happy cow", "a happy cow", true; "character class")]
    #[test_case("[^abc] happy cow", "a happy cow", false; "negated character class")]
    #[test_case("[a-zA-Z]*", "camelCaseFtw", true; "char class ranges matching")]
    #[test_case("[a-zA-Z]*1", "kebab-case-not-so-much", false; "char class ranges non matching")]
    #[test_case("[a-zA-Z ]*", "this should work", true; "char class mixed")]
    #[test_case("[\\]5]*", "5]]5555]]", true; "char class escaped bracket")]
    #[test_case("[0-9]+", "0123", true; "digit range")]
    #[test_case("[0-9]+", "0", true; "digit range range start only")]
    #[test_case("25[0-5]", "255", true; "ipv4 element one")]
    #[test_case("2[0-4][0-9]", "231", true; "ipv4 element two")]
    #[test_case("1?[0-9]?[0-9]", "155", true; "ipv4 element three three digit")]
    #[test_case("1?[0-9]?[0-9]", "72", true; "ipv4 element three two digit")]
    #[test_case("1?[0-9]?[0-9]", "8", true; "ipv4 element three one digit")]
    #[test_case("1?[0-9]?[0-9]", "0", true; "ipv4 element three zero")]
    #[test_case("(25[0-5]|2[0-4][0-9])", "255", true; "ipv4 elements one and two matching one")]
    #[test_case("(25[0-5]|2[0-4][0-9])", "219", true; "ipv4 elements one and two matching two")]
    #[test_case("(25[0-5]|2[0-4][0-9])", "42", false; "ipv4 elements one and two not matching")]
    #[test_case("(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])", "251", true; "ipv4 all elements matching one")]
    #[test_case("(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])", "237", true; "ipv4 all elements matching two")]
    #[test_case("(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])", "142", true; "ipv4 all elements matching three")]
    #[test_case(
        r"(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])",
        "127.0.0.1",
        true;
        "ipv4 full"
    )]
    #[test_case("^foo", "foo at the start", true; "SOL holding")]
    #[test_case("^foo", "bar\nfoo at the start", true; "SOL holding after newline")]
    #[test_case("^foo", "we have foo but not at the start", false; "SOL not holding")]
    #[test_case("foo$", "a line that ends with foo", true; "BOL holding")]
    #[test_case("foo$", "a line that ends with foo\nnow bar", true; "BOL holding before newline")]
    #[test_case("foo$", "a line with foo in the middle", false; "BOL not holding")]
    #[test_case("a{3}", "aaa", true; "counted repetition")]
    #[test_case("a{3}", "aa", false; "counted repetition non matching")]
    #[test_case("a{3,}", "aaaaaa", true; "counted repetition at least")]
    #[test_case("a{3,}", "aa", false; "counted repetition at least non matching")]
    #[test_case("a{3,5}", "aaa", true; "counted repetition between lower")]
    #[test_case("a{3,5}", "aaaaa", true; "counted repetition between upper")]
    #[test_case("a{3,5}", "aaaa", true; "counted repetition in range")]
    #[test_case("a{3,5}", "aa", false; "counted repetition less")]
    #[test_case("^a{3,5}$", "aaaaaa", false; "counted repetition more")]
    #[test]
    fn match_works(re: &str, s: &str, matches: bool) {
        let mut r = Regex::compile(re).unwrap();
        assert_eq!(r.match_str(s).is_some(), matches);
    }

    #[test_case("[0-9]+", " 42 3 127 9991 ", &["42", "3", "127", "9991"]; "integers")]
    #[test_case("[0-9]+", " 42 3 127 9991", &["42", "3", "127", "9991"]; "integers to EOF")]
    #[test_case("[0-9]+", "42 3 127 9991 ", &["42", "3", "127", "9991"]; "integers from BOF")]
    #[test_case("[0-9]+", "42 3 127 9991", &["42", "3", "127", "9991"]; "integers full input")]
    #[test]
    fn match_all_works(re: &str, s: &str, expected: &[&str]) {
        let mut r = Regex::compile(re).unwrap();
        let matches: Vec<String> = r.match_str_all(s).map(|m| m.str_match_text(s)).collect();

        assert_eq!(&matches, expected);
    }

    #[test]
    fn match_extraction_works() {
        let re = "([0-9]+)-([0-9]+)";
        let mut r = Regex::compile(re).unwrap();
        let s = "this should work 123-456 other stuff";
        let m = r.match_str(s).unwrap();

        assert_eq!(m.str_submatch_text(1, s).as_deref(), Some("123"));
        assert_eq!(m.str_submatch_text(2, s).as_deref(), Some("456"));
        assert_eq!(m.str_match_text(s), "123-456");
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
