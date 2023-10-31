//! The op-code compiler and optimised for the regex VM
use super::{CharClass, Pfix};
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Inst {
    pub(super) op: Op,
    pub(super) gen: usize,
}

pub(super) type Prog = Vec<Inst>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Assertion {
    LineStart,
    LineEnd,
    WordBoundary,
    NonWordBoundary,
}

impl Assertion {
    pub(super) fn holds_for(&self, prev: Option<char>, next: Option<char>) -> bool {
        match self {
            Assertion::LineStart => matches!(prev, Some('\n') | None),
            Assertion::LineEnd => matches!(next, Some('\n') | None),
            Assertion::WordBoundary => match (prev, next) {
                (_, None) | (None, _) => true,
                (Some(p), Some(n)) => p.is_alphanumeric() != n.is_alphanumeric(),
            },
            Assertion::NonWordBoundary => match (prev, next) {
                (_, None) | (None, _) => false,
                (Some(p), Some(n)) => p.is_alphanumeric() == n.is_alphanumeric(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Op {
    // Comparison ops
    Char(char),
    Class(CharClass),
    Any,
    TrueAny,
    // Assertions
    Assertion(Assertion),
    // Control ops
    Split(usize, usize),
    Jump(usize),
    Save(usize),
    Match,
}

impl Op {
    /// Whether or not this Op matches the current VM state.
    /// This will panic if not called on a comparison or assertion Op.
    pub(super) fn matches(&self, ch: char) -> bool {
        match self {
            Op::Char(c) => *c == ch,
            Op::Class(cls) => cls.matches(ch),
            Op::Any => ch != '\n',
            Op::TrueAny => true,

            op => panic!("matches called on invalid op: {op:?}"),
        }
    }

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

pub(super) fn compile(pfix: Vec<Pfix>) -> Vec<Op> {
    let mut prog: Vec<Op> = Vec::with_capacity(pfix.len());
    let mut expr_offsets: Vec<usize> = Vec::with_capacity(pfix.len());

    macro_rules! push {
        ($op:expr) => {{
            expr_offsets.push(prog.len());
            prog.push($op);
        }};
        (@concat $op:expr) => {{
            prog.push($op);
        }};
        (@expr $exp:expr) => {{
            expr_offsets.push(prog.len());
            prog.append(&mut $exp);
        }};
        (@extend $exp:expr) => {{
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

    macro_rules! plus {
        ($p:expr) => {{
            let ix = *expr_offsets.last().unwrap();
            let (mut l1, mut l2) = (ix, prog.len() + 1);
            if $p == Pfix::LazyPlus {
                (l1, l2) = (l2, l1);
            };
            push!(@concat Op::Split(l1, l2));
        }};
    }

    macro_rules! quest {
        ($p:expr, $e:expr) => {{
            let ix = prog.len(); // index of the split we are inserting
            let (mut l1, mut l2) = (ix + 1, ix + 1 + $e.len());
            if $p == Pfix::LazyQuest {
                (l1, l2) = (l2, l1);
            };
            push!(Op::Split(l1, l2));
            $e.iter_mut().for_each(|op| op.inc(ix));
            push!(@extend $e);
        }};
    }

    for p in pfix.into_iter() {
        match p {
            Pfix::Save(s) => push!(@save s),
            Pfix::Class(cls) => push!(Op::Class(cls)),
            Pfix::Char(ch) => push!(Op::Char(ch)),
            Pfix::TrueAny => push!(Op::TrueAny),
            Pfix::Any => push!(Op::Any),

            Pfix::LineStart => push!(Op::Assertion(Assertion::LineStart)),
            Pfix::LineEnd => push!(Op::Assertion(Assertion::LineEnd)),
            Pfix::WordBoundary => push!(Op::Assertion(Assertion::WordBoundary)),
            Pfix::NonWordBoundary => push!(Op::Assertion(Assertion::NonWordBoundary)),

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
                push!(@extend e1);

                let ix2 = prog.len(); // index of the split we are inserting
                push!(@concat Op::Jump(ix2 + 1 + e2.len()));
                e2.iter_mut().for_each(|op| op.inc(ix2));
                push!(@extend e2);
            }

            // Lazy operators are implemented by reversing the priority order of the threads
            // the create so that shorter matches are preferred.
            Pfix::Plus | Pfix::LazyPlus => plus!(p),

            Pfix::Quest | Pfix::LazyQuest => {
                let mut e = pop!();
                quest!(p, e);
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
                push!(@extend e);
                push!(@concat Op::Jump(ix))
            }

            // Counted repetitions are expanded explicitly into
            // larger single expressions

            // e{3} -> eee
            Pfix::Rep(n) => {
                if n == 1 {
                    continue;
                }
                let e = pop!();
                for _ in 0..n {
                    push!(@expr e.clone());
                }
            }

            // e{3,} -> eee+
            Pfix::RepAtLeast(n) => {
                let e = pop!();
                for _ in 0..n {
                    push!(@expr e.clone());
                }
                plus!(Pfix::Plus);
            }

            // e{3,5} -> eeee?e?
            Pfix::RepBetween(n, m) => {
                let e = pop!();
                for _ in 0..n {
                    push!(@expr e.clone());
                }
                for _ in n..m {
                    quest!(Pfix::Quest, e.clone());
                }
            }
        }
    }

    // Compiled code for "@*?" to allow for unanchored matching.
    // Save(0) marks the beginning of the regex in the input
    let mut full = vec![Op::Split(3, 1), Op::TrueAny, Op::Split(3, 1), Op::Save(0)];
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

pub(super) fn optimise(mut ops: Vec<Op>) -> Vec<Op> {
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
    use crate::regex::re_to_postfix;
    use simple_test_case::test_case;

    const BOL: Op = Op::Assertion(Assertion::LineStart);
    const EOL: Op = Op::Assertion(Assertion::LineEnd);

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
    #[test_case("a|b|c", vec![sp(5, 7), c('a'), jmp(11), sp(8, 10), c('b'), jmp(11), c('c')]; "chained alternation")]
    #[test_case("^foo", vec![BOL, c('f'), c('o'), c('o')]; "BOL then literals")]
    #[test_case("foo$", vec![c('f'), c('o'), c('o'), EOL]; "literals then EOL")]
    #[test]
    fn opcode_compile_works(re: &str, expected: Vec<Op>) {
        let pfix = re_to_postfix(re).unwrap();
        let prog = compile(pfix);
        let mut full = vec![sp(3, 1), Op::TrueAny, sp(3, 1), sv(0)];
        full.extend(expected);
        full.extend([sv(1), Op::Match]);

        assert_eq!(prog, full);
    }

    #[test_case("a|b", vec![sp(5, 7), c('a'), jmp(8), c('b')]; "single char alt")]
    #[test_case("ab(c|d)", vec![c('a'), c('b'), sv(2), sp(8, 10), c('c'), jmp(11), c('d'), sv(3)]; "lits then alt")]
    #[test_case("ab*a", vec![c('a'), sp(6, 8), c('b'), sp(6, 8), c('a')]; "star for single lit")]
    #[test_case("ba*", vec![c('b'), sp(6, 8), c('a'), sp(6, 8)]; "trailing star")]
    #[test_case("(a*)*", vec![sp(5, 11), sv(2), sp(7, 9), c('a'), sp(7, 9), sv(3), sp(5, 11)]; "star star")]
    #[test]
    fn opcode_optimise_works(re: &str, expected: Vec<Op>) {
        let prog = optimise(compile(re_to_postfix(re).unwrap()));
        let mut full = vec![sp(3, 1), Op::TrueAny, sp(3, 1), sv(0)];
        full.extend(expected);
        full.extend([sv(1), Op::Match]);

        assert_eq!(prog, full);
    }
}
