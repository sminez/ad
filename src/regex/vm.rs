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
use std::mem::take;

pub struct Regex {
    /// The compiled instructions for running the VM
    prog: Prog,
    /// Pre-allocated Thread list
    clist: Vec<usize>,
    /// Pre-allocated Thread list
    nlist: Vec<usize>,
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

        let clist = vec![0; prog.len()];
        let nlist = vec![0; prog.len()];

        Ok(Self {
            prog,
            clist,
            nlist,
            gen: 1,
            p: 0,
        })
    }

    pub fn matches_str(&mut self, input: &str) -> bool {
        self.matches_iter(input.chars().enumerate())
    }

    // TODO: track the start of the match so we can return the range that is matching
    pub fn matches_iter<I>(&mut self, input: I) -> bool
    where
        I: Iterator<Item = (usize, char)>,
    {
        let mut clist = take(&mut self.clist);
        let mut nlist = take(&mut self.nlist);
        self.p = 0;

        self.add_thread(&mut clist, 0);
        self.gen += 1;
        let mut n = self.p;
        let mut matched = false;

        for (_, ch) in input {
            for &tpc in clist.iter().take(n) {
                match &self.prog[tpc].op {
                    Op::Char(c) if *c == ch => self.add_thread(&mut nlist, tpc + 1),
                    Op::Class(cls) if cls.matches_char(ch) => self.add_thread(&mut nlist, tpc + 1),
                    Op::Any if ch != '\n' => self.add_thread(&mut nlist, tpc + 1),
                    Op::TrueAny => self.add_thread(&mut nlist, tpc + 1),

                    Op::Match => {
                        matched = true;
                        break;
                    }

                    // Jump & Split are handled in add_thread.
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
        matched || self.clist.iter().any(|&tpc| self.prog[tpc].op == Op::Match)
    }

    fn add_thread(&mut self, lst: &mut [usize], pc: usize) {
        if self.prog[pc].gen == self.gen {
            return;
        }
        self.prog[pc].gen = self.gen;

        if let Op::Jump(l1) = self.prog[pc].op {
            self.add_thread(lst, l1);
        } else if let Op::Split(l1, l2) = self.prog[pc].op {
            self.add_thread(lst, l1);
            self.add_thread(lst, l2);
        } else {
            lst[self.p] = pc;
            self.p += 1;
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
    Match,
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
    }

    macro_rules! pop {
        () => {{
            let ix = expr_offsets.pop().unwrap();
            prog.split_off(ix)
        }};
    }

    for p in pfix.into_iter() {
        match p {
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
                push!(@expr e1);
                push!(Op::Jump(prog.len() + 1 + e2.len()));
                push!(@expr e2);
            }

            Pfix::Plus => {
                let ix = *expr_offsets.last().unwrap();
                push!(Op::Split(ix, prog.len() + 1));
            }

            Pfix::Quest => {
                let mut e = pop!();
                let ix = prog.len(); // index of the split we are inserting

                push!(Op::Split(ix + 1, ix + 1 + e.len()));
                push!(@expr e);
            }

            Pfix::Star => {
                let mut e = pop!();
                let ix = prog.len(); // index of the split we are inserting

                push!(Op::Split(ix + 1, ix + 2 + e.len()));
                push!(@expr e);
                push!(Op::Jump(ix))
            }
        }
    }

    prog.push(Op::Match);

    prog
}

fn optimise(mut ops: Vec<Op>) -> Vec<Op> {
    for i in 0..ops.len() {
        if let Op::Jump(j) = ops[i] {
            // - Chained jumps or jumps to splits can be compressed
            // - Jump to match is just match
            if let Op::Jump(l1) = ops[j] {
                ops[i] = Op::Jump(l1);
            } else if let Op::Split(l1, l2) = ops[j] {
                ops[i] = Op::Split(l1, l2);
            } else if let Op::Match = ops[j] {
                ops[i] = Op::Match;
            }
        } else if let Op::Split(s1, s2) = ops[i] {
            // - Split to jump can be inlined
            let new_s1 = if let Op::Jump(j1) = ops[s1] { j1 } else { s1 };
            let new_s2 = if let Op::Jump(j2) = ops[s2] { j2 } else { s2 };
            if new_s1 != s1 || new_s2 != s2 {
                ops[i] = Op::Split(new_s1, new_s2);
            }
        }
    }

    ops
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

    #[test_case("abc", &[c('a'), c('b'), c('c'), Op::Match]; "lit only")]
    #[test_case("a|b", &[sp(1, 3), c('a'), jmp(4), c('b'), Op::Match]; "single char alt")]
    #[test_case("ab(c|d)", &[c('a'), c('b'), sp(3, 5), c('c'), jmp(6), c('d'), Op::Match]; "lits then alt")]
    #[test_case("ab+a", &[c('a'), c('b'), sp(1, 3), c('a'), Op::Match]; "plus for single lit")]
    #[test_case("ab?a", &[c('a'), sp(2, 3), c('b'), c('a'), Op::Match]; "quest for single lit")]
    #[test_case("ab*a", &[c('a'), sp(2, 4), c('b'), jmp(1), c('a'), Op::Match]; "star for single lit")]
    #[test_case("a(bb)+a", &[c('a'), c('b'), c('b'), sp(1, 4), c('a'), Op::Match]; "rep of cat")]
    #[test_case("ba*", &[c('b'), sp(2, 4), c('a'), jmp(1), Op::Match]; "trailing star")]
    #[test_case("b?a", &[sp(1, 2), c('b'), c('a'), Op::Match]; "first lit is optional")]
    #[test]
    fn opcode_compile_works(re: &str, expected: &[Op]) {
        let prog = compile(re_to_postfix(re).unwrap());
        assert_eq!(&prog, expected);
    }
}
