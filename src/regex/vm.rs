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
    compile::{compile, optimise, Inst, Op, Prog},
    matches::{Match, MatchIter},
    re_to_postfix, Error,
};
use ropey::Rope;
use std::mem::take;

/// A regular expression engine designed for use within the ad text editor.
///
/// This is a relatively naive implementation though it does have some
/// optimisations and runs reasonably quickly. It is not at all designed to
/// be robust against mallicious input and it does not attempt to support
/// full PCRE syntax or functionality.
pub struct Regex {
    /// The compiled instructions for running the VM
    prog: Prog,
    /// Pre-allocated Thread list
    clist: Vec<Thread>,
    /// Pre-allocated Thread list
    nlist: Vec<Thread>,
    /// Monotonically increasing index used to dedup Threads
    /// Will overflow at some point if a given regex is used a VERY large number of times
    gen: usize,
    /// Index into the current Thread list
    p: usize,
}

impl Regex {
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
        })
    }

    pub fn match_str(&mut self, input: &str) -> Option<Match> {
        self.match_iter(&mut input.char_indices(), 0)
    }

    pub fn match_str_all<'a, 'b>(&'a mut self, input: &'b str) -> MatchIter<'a, &'b str> {
        MatchIter {
            it: input,
            r: self,
            from: 0,
        }
    }

    pub fn match_rope(&mut self, rope: &Rope) -> Option<Match> {
        self.match_iter(&mut rope.chars().enumerate(), 0)
    }

    pub fn match_rope_all<'a, 'b>(&'a mut self, input: &'b Rope) -> MatchIter<'a, &'b Rope> {
        MatchIter {
            it: input,
            r: self,
            from: 0,
        }
    }

    pub fn match_iter<I>(&mut self, mut input: &mut I, mut sp: usize) -> Option<Match>
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

        for (i, ch) in &mut input {
            sp = i;
            for t in clist.iter_mut().take(n) {
                match &self.prog[t.pc].op {
                    Op::Char(c) if *c == ch => {
                        self.add_thread(&mut nlist, t.split_to(t.pc + 1), sp, false)
                    }
                    Op::Class(cls) if cls.matches(ch) => {
                        self.add_thread(&mut nlist, t.split_to(t.pc + 1), sp, false)
                    }
                    Op::Any if ch != '\n' => {
                        self.add_thread(&mut nlist, t.split_to(t.pc + 1), sp, false)
                    }
                    Op::TrueAny => self.add_thread(&mut nlist, t.split_to(t.pc + 1), sp, false),

                    Op::Match => {
                        matched = true;
                        sub_matches = t.sub_matches;
                        sub_matches[1] = sp; // Save end of the match
                        break;
                    }

                    // Save, Jump & Split are handled in add_thread.
                    // Non-matching comparison ops result in that thread dying.
                    _ => (),
                }
            }

            (clist, nlist) = (nlist, clist);

            if self.p == 0 {
                did_break = true;
                break;
            }

            self.gen += 1;
            n = self.p;
            self.p = 0;
        }

        self.clist = clist;
        self.nlist = nlist;

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

    fn add_thread(&mut self, lst: &mut [Thread], mut t: Thread, sp: usize, initial: bool) {
        if self.prog[t.pc].gen == self.gen {
            return;
        }
        self.prog[t.pc].gen = self.gen;

        if let Op::Jump(l1) = self.prog[t.pc].op {
            self.add_thread(lst, t.split_to(l1), sp, initial);
        } else if let Op::Split(l1, l2) = self.prog[t.pc].op {
            self.add_thread(lst, t.split_to(l1), sp, initial);
            self.add_thread(lst, t.split_to(l2), sp, initial);
        } else if let Op::Save(s) = self.prog[t.pc].op {
            // Save start is for the NEXT char and Save end is for CURRENT char
            t.sub_matches[s] = if s % 2 == 0 && !initial { sp + 1 } else { sp };
            self.add_thread(lst, t.split_to(t.pc + 1), sp, initial);
        } else {
            lst[self.p] = t;
            self.p += 1;
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Thread {
    // VM program counter for this thread
    pc: usize,
    // $0 -> $9 submatches with $0 being the full match
    // for submatch $k the start index is 2$k and the end is 2$k+1
    sub_matches: [usize; 20],
}

impl Thread {
    fn split_to(&self, pc: usize) -> Thread {
        Thread {
            pc,
            sub_matches: self.sub_matches,
        }
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
        let matches: Vec<&str> = r.match_str_all(s).map(|m| m.str_match_text(s)).collect();

        assert_eq!(&matches, expected);
    }

    #[test]
    fn match_extraction_works() {
        let re = "([0-9]+)-([0-9]+)";
        let mut r = Regex::compile(re).unwrap();
        let s = "this should work 123-456 other stuff";
        let m = r.match_str(s).unwrap();

        assert_eq!(m.str_submatch_text(1, s), Some("123"));
        assert_eq!(m.str_submatch_text(2, s), Some("456"));
        assert_eq!(m.str_match_text(s), "123-456");
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
