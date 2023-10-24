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
use super::{re_to_postfix, CharClass, Error, Pfix};
use ropey::{Rope, RopeSlice};
use std::{collections::BTreeSet, mem::take};

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

#[derive(Debug, Clone, PartialEq, Eq)]
enum Op {
    // Comparison ops
    Char(char),
    Class(CharClass),
    Any,
    TrueAny,
    // Control ops
    Split(usize, usize),
    Jump(usize),
    Save(usize),
    Match,
}

impl Op {
    fn is_comp(&self) -> bool {
        !matches!(self, Op::Split(_, _) | Op::Jump(_) | Op::Match)
    }

    // Increment jumps and splits past the given index
    fn inc(&mut self, i: usize) {
        match self {
            Op::Jump(j) if *j >= i => *j += 1,
            Op::Split(l1, l2) => {
                if *l1 >= i {
                    *l1 += 1;
                }
                if *l2 >= i {
                    *l2 += 1;
                }
            }
            _ => (),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Inst {
    op: Op,
    gen: usize,
}

type Prog = Vec<Inst>;

fn compile(pfix: Vec<Pfix>) -> Vec<Op> {
    let mut prog: Vec<Op> = Vec::with_capacity(pfix.len());
    let mut expr_offsets: Vec<usize> = Vec::with_capacity(pfix.len());

    macro_rules! push {
        ($op:expr) => {{
            expr_offsets.push(prog.len());
            prog.push($op);
        }};
        (@expr $exp:expr) => {{
            expr_offsets.push(prog.len());
            prog.append(&mut $exp);
        }};
        (@save $s:expr) => {{
            if $s % 2 == 0 {
                expr_offsets.push(prog.len());
            } else {
                let ix = prog
                    .iter()
                    .position(|op| op == &Op::Save($s - 1))
                    .expect("to have save start");
                expr_offsets.truncate(ix + 1);
            }
            prog.push(Op::Save($s));
        }};
    }

    macro_rules! pop {
        () => {{
            let ix = expr_offsets.pop().unwrap();
            prog.split_off(ix)
        }};
    }

    for p in pfix.into_iter() {
        match p {
            Pfix::Save(s) => push!(@save s),
            Pfix::Class(cls) => push!(Op::Class(cls)),
            Pfix::Char(ch) => push!(Op::Char(ch)),
            Pfix::TrueAny => push!(Op::TrueAny),
            Pfix::Any => push!(Op::Any),

            Pfix::Concat => {
                expr_offsets.pop();
            }

            Pfix::Alt => {
                let mut e2 = pop!();
                let mut e1 = pop!();
                let ix = prog.len(); // index of the split we are inserting

                push!(Op::Split(ix + 1, ix + 2 + e1.len()));
                e1.iter_mut().for_each(|op| op.inc(ix));
                e2.iter_mut().for_each(|op| op.inc(ix));
                push!(@expr e1);

                let ix2 = prog.len(); // index of the split we are inserting
                push!(Op::Jump(ix2 + 1 + e2.len()));
                e2.iter_mut().for_each(|op| op.inc(ix2));
                push!(@expr e2);
            }

            // Lazy operators are implemented by reversing the priority order of the threads
            // the create so that shorter matches are preferred.
            Pfix::Plus | Pfix::LazyPlus => {
                let ix = *expr_offsets.last().unwrap();
                let (mut l1, mut l2) = (ix, prog.len() + 1);
                if p == Pfix::LazyPlus {
                    (l1, l2) = (l2, l1);
                };

                push!(Op::Split(l1, l2));
            }

            Pfix::Quest | Pfix::LazyQuest => {
                let mut e = pop!();

                let ix = prog.len(); // index of the split we are inserting
                let (mut l1, mut l2) = (ix + 1, ix + 1 + e.len());
                if p == Pfix::LazyQuest {
                    (l1, l2) = (l2, l1);
                };

                push!(Op::Split(l1, l2));
                e.iter_mut().for_each(|op| op.inc(ix));
                push!(@expr e);
            }

            Pfix::Star | Pfix::LazyStar => {
                let mut e = pop!();

                let ix = prog.len(); // index of the split we are inserting
                let (mut l1, mut l2) = (ix + 1, ix + 2 + e.len());
                if p == Pfix::LazyStar {
                    (l1, l2) = (l2, l1);
                };

                push!(Op::Split(l1, l2));
                e.iter_mut().for_each(|op| op.inc(ix));
                push!(@expr e);
                push!(Op::Jump(ix))
            }
        }
    }

    // Compiled code for "@*?" to allow for unanchored matching.
    // Save(0) marks the beginning of the regex in the input
    let mut full = vec![Op::Split(3, 1), Op::TrueAny, Op::Jump(0), Op::Save(0)];
    // Unconditionally increment all jumps and splits in the compiled program
    // to account for the prefix we just added.
    full.extend(prog.into_iter().map(|op| match op {
        Op::Jump(j) => Op::Jump(j + 4),
        Op::Split(l1, l2) => Op::Split(l1 + 4, l2 + 4),
        op => op,
    }));
    // Save(1) marks the end of the regex in the input
    full.extend([Op::Save(1), Op::Match]);

    full
}

fn optimise(mut ops: Vec<Op>) -> Vec<Op> {
    let mut optimising = true;

    while optimising {
        optimising = false;
        for i in 0..ops.len() {
            optimising |= inline_jumps(&mut ops, i);
        }
    }

    strip_unreachable_instructions(&mut ops);

    ops
}

// - Chained jumps or jumps to splits can be inlined
// - Split to jump can be inlined
// - Jump to Match is just Match
// - Split to Match is Match if both branches are Match,
//   otherwise there could be a longer match available
//   on the non-Match branch so we keep the split
#[inline]
fn inline_jumps(ops: &mut [Op], i: usize) -> bool {
    if let Op::Jump(j) = ops[i] {
        if let Op::Jump(l1) = ops[j] {
            ops[i] = Op::Jump(l1);
        } else if let Op::Split(l1, l2) = ops[j] {
            ops[i] = Op::Split(l1, l2);
        } else if let Op::Match = ops[j] {
            ops[i] = Op::Match;
        } else {
            return false;
        }
        return true;
    } else if let Op::Split(s1, s2) = ops[i] {
        if ops[s1] == Op::Match && ops[s2] == Op::Match {
            ops[i] = Op::Match;
            return true;
        }

        let new_s1 = if let Op::Jump(j1) = ops[s1] { j1 } else { s1 };
        let new_s2 = if let Op::Jump(j2) = ops[s2] { j2 } else { s2 };
        if new_s1 != s1 || new_s2 != s2 {
            ops[i] = Op::Split(new_s1, new_s2);
            return true;
        }
    }

    false
}

// An instruction is unreachable if:
// - it doesn't follow a comparison instruction (pc wouldn't advance to it)
// - nothing now jumps or splits to it
fn strip_unreachable_instructions(ops: &mut Vec<Op>) {
    let mut to_from: Vec<(usize, usize)> = Vec::with_capacity(ops.len());
    let mut jumps = BTreeSet::new();

    for (i, op) in ops.iter().enumerate() {
        match op {
            Op::Jump(j) => {
                jumps.insert(*j);
                to_from.push((*j, i));
            }
            Op::Split(l1, l2) => {
                jumps.extend([*l1, *l2]);
                to_from.push((*l1, i));
                to_from.push((*l2, i));
            }
            _ => (),
        }
    }

    for i in (1..ops.len() - 1).rev() {
        if ops[i - 1].is_comp() || jumps.contains(&i) {
            continue;
        }

        for &(to, from) in to_from.iter() {
            if to > i {
                match &mut ops[from] {
                    Op::Jump(x) => *x -= 1,
                    Op::Split(x, _) if *x > i => *x -= 1,
                    Op::Split(_, x) if *x > i => *x -= 1,
                    _ => (),
                }
            }
        }
        ops.remove(i);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    fn sp(l1: usize, l2: usize) -> Op {
        Op::Split(l1, l2)
    }

    fn jmp(l1: usize) -> Op {
        Op::Jump(l1)
    }

    fn c(ch: char) -> Op {
        Op::Char(ch)
    }

    fn sv(s: usize) -> Op {
        Op::Save(s)
    }

    #[test_case("abc", vec![c('a'), c('b'), c('c')]; "lit only")]
    #[test_case("a|b", vec![sp(5, 7), c('a'), jmp(8), c('b')]; "single char alt")]
    #[test_case("ab(c|d)", vec![c('a'), c('b'), sv(2), sp(8, 10), c('c'), jmp(11), c('d'), sv(3)]; "lits then alt")]
    #[test_case("ab+a", vec![c('a'), c('b'), sp(5, 7), c('a')]; "plus for single lit")]
    #[test_case("ab?a", vec![c('a'), sp(6, 7), c('b'), c('a')]; "quest for single lit")]
    #[test_case("ab*a", vec![c('a'), sp(6, 8), c('b'), jmp(5), c('a')]; "star for single lit")]
    #[test_case("a(bb)+a", vec![c('a'), sv(2), c('b'), c('b'), sv(3), sp(6, 10), c('a')]; "rep of cat")]
    #[test_case("ba*", vec![c('b'), sp(6, 8), c('a'), jmp(5)]; "trailing star")]
    #[test_case("b?a", vec![sp(5, 6), c('b'), c('a')]; "first lit is optional")]
    #[test_case("(a*)", vec![sv(2), sp(6, 8), c('a'), jmp(5), sv(3)]; "star")]
    #[test_case("(a*)*", vec![sp(5, 11), sv(2), sp(7, 9), c('a'), jmp(6), sv(3), jmp(4)]; "star star")]
    #[test]
    fn opcode_compile_works(re: &str, expected: Vec<Op>) {
        let pfix = re_to_postfix(re).unwrap();
        let prog = compile(pfix);
        let mut full = vec![sp(3, 1), Op::TrueAny, jmp(0), sv(0)];
        full.extend(expected);
        full.extend([sv(1), Op::Match]);

        assert_eq!(prog, full);
    }

    // '[Save(2), Char('a'), Star, Save(3), Star, Concat]',

    #[test_case("a|b", vec![sp(5, 7), c('a'), jmp(8), c('b')]; "single char alt")]
    #[test_case("ab(c|d)", vec![c('a'), c('b'), sv(2), sp(8, 10), c('c'), jmp(11), c('d'), sv(3)]; "lits then alt")]
    #[test_case("ab*a", vec![c('a'), sp(6, 8), c('b'), sp(6, 8), c('a')]; "star for single lit")]
    #[test_case("ba*", vec![c('b'), sp(6, 8), c('a'), sp(6, 8)]; "trailing star")]
    // #[test_case("(a*)*", vec![sp(5, 11), sv(2), sp(7, 9), c('a'), sp(7, 9), sv(3), sp(5, 11)]; "star star")]
    #[test]
    fn opcode_optimise_works(re: &str, expected: Vec<Op>) {
        let prog = optimise(compile(re_to_postfix(re).unwrap()));
        let mut full = vec![sp(3, 1), Op::TrueAny, sp(3, 1), sv(0)];
        full.extend(expected);
        full.extend([sv(1), Op::Match]);

        assert_eq!(prog, full);
    }
}
