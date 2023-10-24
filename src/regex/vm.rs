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
    re_to_postfix, Error,
};
use ropey::{Rope, RopeSlice};
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
            gen: 1,
            p: 0,
        })
    }

    pub fn match_str(&mut self, input: &str) -> Option<Match> {
        self.match_iter(input.chars().enumerate())
    }

    pub fn match_rope(&mut self, rope: &Rope) -> Option<Match> {
        self.match_iter(rope.chars().enumerate())
    }

    pub fn match_iter<I>(&mut self, input: I) -> Option<Match>
    where
        I: Iterator<Item = (usize, char)>,
    {
        let mut clist = take(&mut self.clist);
        let mut nlist = take(&mut self.nlist);
        let mut sub_matches = [0; 20];
        self.p = 0;

        self.add_thread(&mut clist, Thread::default(), 0);
        self.gen += 1;
        let mut n = self.p;
        let mut matched = false;

        for (sp, ch) in input {
            for &t in clist.iter().take(n) {
                match &self.prog[t.pc].op {
                    Op::Char(c) if *c == ch => {
                        self.add_thread(&mut nlist, t.split_to(t.pc + 1), sp)
                    }
                    Op::Class(cls) if cls.matches(ch) => {
                        self.add_thread(&mut nlist, t.split_to(t.pc + 1), sp)
                    }
                    Op::Any if ch != '\n' => self.add_thread(&mut nlist, t.split_to(t.pc + 1), sp),
                    Op::TrueAny => self.add_thread(&mut nlist, t.split_to(t.pc + 1), sp),

                    Op::Match => {
                        matched = true;
                        sub_matches = t.sub_matches;
                        break;
                    }

                    // Save, Jump & Split are handled in add_thread.
                    // Non-matching comparison ops result in that thread dying.
                    _ => (),
                }
            }

            (clist, nlist) = (nlist, clist);

            if self.p == 0 {
                break;
            }

            self.gen += 1;
            n = self.p;
            self.p = 0;
        }

        self.clist = clist;
        self.nlist = nlist;

        if !matched {
            for t in self.clist.iter() {
                if self.prog[t.pc].op == Op::Match {
                    return Some(Match {
                        sub_matches: t.sub_matches,
                    });
                }
            }

            return None;
        }

        Some(Match { sub_matches })
    }

    fn add_thread(&mut self, lst: &mut [Thread], mut t: Thread, sp: usize) {
        if self.prog[t.pc].gen == self.gen {
            return;
        }
        self.prog[t.pc].gen = self.gen;

        if let Op::Jump(l1) = self.prog[t.pc].op {
            self.add_thread(lst, t.split_to(l1), sp);
        } else if let Op::Split(l1, l2) = self.prog[t.pc].op {
            self.add_thread(lst, t.split_to(l1), sp);
            self.add_thread(lst, t.split_to(l2), sp);
        } else if let Op::Save(s) = self.prog[t.pc].op {
            t.sub_matches[s] = sp + 1;
            self.add_thread(lst, t.split_to(t.pc + 1), sp);
        } else {
            lst[self.p] = t;
            self.p += 1;
        }
    }
}

/// The match location of a Regex against a given input.
///
/// The sub-match indices are relative to the input used to run the original match.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Match {
    sub_matches: [usize; 20],
}

impl Match {
    pub fn str_match_text<'a>(&self, s: &'a str) -> &'a str {
        let (a, b) = self.loc();
        &s[a..b]
    }

    pub fn str_submatch_text<'a>(&self, n: usize, s: &'a str) -> Option<&'a str> {
        let (a, b) = self.sub_loc(n)?;
        Some(&s[a..b])
    }

    pub fn rope_match_text<'a>(&self, r: &'a Rope) -> RopeSlice<'a> {
        let (a, b) = self.loc();
        r.slice(a..b)
    }

    pub fn rope_submatch_text<'a>(&self, n: usize, r: &'a Rope) -> Option<RopeSlice<'a>> {
        let (a, b) = self.sub_loc(n)?;
        Some(r.slice(a..b))
    }

    pub fn loc(&self) -> (usize, usize) {
        (self.sub_matches[0], self.sub_matches[1])
    }

    pub fn sub_loc(&self, n: usize) -> Option<(usize, usize)> {
        if n > 9 {
            return None;
        }
        let (start, end) = (self.sub_matches[2 * n], self.sub_matches[2 * n + 1]);
        if start == end {
            return None;
        }

        Some((start, end))
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
    #[test_case("this@*works", "this contains\nbut still works", true; "true any")]
    #[test_case(r"literal\?", "literal?", true; "escape special char")]
    #[test_case(r"literal\t", "literal\t", true; "escape sequence")]
    #[test_case("[abc] happy cow", "a happy cow", true; "character class")]
    #[test_case("[^abc] happy cow", "a happy cow", false; "negated character class")]
    #[test_case("[a-zA-Z]*", "camelCaseFtw", true; "char class ranges matching")]
    #[test_case("[a-zA-Z]*1", "kebab-case-not-so-much", false; "char class ranges non matching")]
    #[test_case("[a-zA-Z ]*", "this should work", true; "char class mixed")]
    #[test_case("[\\]5]*", "5]]5555]]", true; "char class escaped bracket")]
    #[test]
    fn match_works(re: &str, s: &str, matches: bool) {
        let mut r = Regex::compile(re).unwrap();
        assert_eq!(r.match_str(s).is_some(), matches);
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
