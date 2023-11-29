//! The op-code compiler and optimised for the regex VM
use super::ast::{Assertion, Ast, Comp, Greed, Rep};
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Inst {
    pub(super) op: Op,
    pub(super) gen: usize,
}

pub(super) type Prog = Vec<Inst>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Op {
    Comp(Comp),
    Assertion(Assertion),
    Split(usize, usize),
    Jump(usize),
    Save(usize),
    RSave(usize),
    Match,
}

impl Op {
    fn is_control(&self) -> bool {
        matches!(self, Op::Jump(_) | Op::Split(_, _) | Op::Match)
    }
}

enum Ops {
    One(Op),
    Many(Vec<Op>),
}

impl Ops {
    fn len(&self) -> usize {
        match self {
            Ops::One(_) => 1,
            Ops::Many(ops) => ops.len(),
        }
    }

    fn add_to(self, buf: &mut Vec<Op>) {
        match self {
            Ops::One(op) => buf.push(op),
            Ops::Many(ops) => buf.extend(ops),
        }
    }
}

fn alt_ops(mut nodes: Vec<Ast>, offset: usize, saves: &mut usize, reverse: bool) -> Vec<Op> {
    // Each node other than the last has an additional split and jump op
    let alt_len = nodes.iter().map(|n| n.op_len()).sum::<usize>() + (nodes.len() - 1) * 2;
    let mut buf = Vec::with_capacity(alt_len);
    let last = nodes.pop().unwrap();
    let it = nodes.into_iter();

    for node in it {
        let base = offset + buf.len();
        buf.push(Op::Split(base + 1, base + 2 + node.op_len()));
        node.into_ops(base + 1, saves, reverse).add_to(&mut buf);
        buf.push(Op::Jump(offset + alt_len));
    }

    last.into_ops(offset + buf.len(), saves, reverse)
        .add_to(&mut buf);

    buf
}

fn concat_ops(nodes: Vec<Ast>, offset: usize, saves: &mut usize, reverse: bool) -> Vec<Op> {
    let concat_len = nodes.iter().map(|n| n.op_len()).sum();
    let mut buf = Vec::with_capacity(concat_len);
    for node in nodes.into_iter() {
        node.into_ops(offset + buf.len(), saves, reverse)
            .add_to(&mut buf);
    }

    buf
}

fn submatch_ops(node: Ast, offset: usize, saves: &mut usize, reverse: bool) -> Vec<Op> {
    *saves += 1;
    let s = *saves * 2;
    let ops = node.into_ops(offset + 1, saves, reverse);
    let mut buf = Vec::with_capacity(ops.len() + 2);

    buf.push(if reverse {
        Op::RSave(s + 1)
    } else {
        Op::Save(s)
    });

    ops.add_to(&mut buf);

    buf.push(if reverse {
        Op::RSave(s)
    } else {
        Op::Save(s + 1)
    });

    buf
}

fn rep_ops(r: Rep, node: Ast, offset: usize, saves: &mut usize, reverse: bool) -> Vec<Op> {
    match r {
        Rep::Quest(greed) => {
            let (mut l1, mut l2) = (offset + 1, offset + 1 + node.op_len());
            if greed == Greed::Lazy {
                (l1, l2) = (l2, l1);
            };
            let ops = node.into_ops(offset + 1, saves, reverse);
            let mut buf = vec![Op::Split(l1, l2)];
            ops.add_to(&mut buf);
            buf
        }

        Rep::Star(greed) => {
            let (mut l1, mut l2) = (offset + 1, offset + 2 + node.op_len());
            if greed == Greed::Lazy {
                (l1, l2) = (l2, l1);
            };
            let ops = node.into_ops(offset + 1, saves, reverse);
            let mut buf = vec![Op::Split(l1, l2)];
            ops.add_to(&mut buf);
            buf.push(Op::Jump(offset));
            buf
        }

        Rep::Plus(greed) => {
            let (mut l1, mut l2) = (offset, offset + 1 + node.op_len());
            if greed == Greed::Lazy {
                (l1, l2) = (l2, l1);
            };
            let ops = node.into_ops(offset, saves, reverse);
            let mut buf = Vec::new();
            ops.add_to(&mut buf);
            buf.push(Op::Split(l1, l2));
            buf
        }
    }
}

impl Ast {
    /// Each AST node returns split and jump offsets as if it were zero indexed.
    /// Wrapper nodes (alts, reps, cats, subexprs) update those offsets as needed
    fn into_ops(self, offset: usize, saves: &mut usize, reverse: bool) -> Ops {
        match self {
            Ast::Comp(comp) => Ops::One(Op::Comp(comp)),
            Ast::Assertion(a) => Ops::One(Op::Assertion(a)),
            Ast::Alt(nodes) => Ops::Many(alt_ops(nodes, offset, saves, reverse)),
            Ast::Concat(nodes) => Ops::Many(concat_ops(nodes, offset, saves, reverse)),
            Ast::SubMatch(node) => Ops::Many(submatch_ops(*node, offset, saves, reverse)),
            Ast::Rep(r, node) => Ops::Many(rep_ops(r, *node, offset, saves, reverse)),
        }
    }
}

pub(super) fn compile_ast(mut ast: Ast, reverse: bool) -> Vec<Op> {
    if reverse {
        ast.reverse();
    }

    let mut saves = 0;
    let prog = match ast.into_ops(0, &mut saves, reverse) {
        Ops::One(op) => vec![op],
        Ops::Many(prog) => prog,
    };

    let (first_save, second_save) = if reverse {
        (Op::RSave(1), Op::RSave(0))
    } else {
        (Op::Save(0), Op::Save(1))
    };

    // Compiled code for "@*?" to allow for unanchored matching.
    // Save(0) marks the beginning of the regex in the input
    // Save(1) marks the end of the regex in the input
    let mut full = vec![
        Op::Split(3, 1),
        Op::Comp(Comp::TrueAny),
        Op::Split(3, 1),
        first_save,
    ];
    // Unconditionally increment all jumps and splits in the compiled program
    // to account for the prefix we just added.
    full.extend(prog.into_iter().map(|op| match op {
        Op::Jump(j) => Op::Jump(j + 4),
        Op::Split(l1, l2) => Op::Split(l1 + 4, l2 + 4),
        op => op,
    }));
    full.extend([second_save, Op::Match]);

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
        if !ops[i - 1].is_control() || jumps.contains(&i) {
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
    use crate::regex::ast::parse;
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
        Op::Comp(Comp::Char(ch))
    }

    fn sv(s: usize) -> Op {
        Op::Save(s)
    }

    #[test_case("abc", vec![c('a'), c('b'), c('c')]; "lit only")]
    #[test_case("a|b", vec![sp(5, 7), c('a'), jmp(8), c('b')]; "single char alt")]
    #[test_case("a|b|c", vec![sp(5, 7), c('a'), jmp(11), sp(8, 10), c('b'), jmp(11), c('c')]; "chained alternation")]
    #[test_case("(a|b)", vec![sv(2), sp(6, 8), c('a'), jmp(9), c('b'), sv(3)]; "single char alt submatch")]
    #[test_case("ab(c|d)", vec![c('a'), c('b'), sv(2), sp(8, 10), c('c'), jmp(11), c('d'), sv(3)]; "lits then alt")]
    #[test_case("ab+a", vec![c('a'), c('b'), sp(5, 7), c('a')]; "plus for single lit")]
    #[test_case("ab?a", vec![c('a'), sp(6, 7), c('b'), c('a')]; "quest for single lit")]
    #[test_case("ab*a", vec![c('a'), sp(6, 8), c('b'), jmp(5), c('a')]; "star for single lit")]
    #[test_case("a(bb)+a", vec![c('a'), sv(2), c('b'), c('b'), sv(3), sp(5, 10), c('a')]; "rep of cat")]
    #[test_case("ba*", vec![c('b'), sp(6, 8), c('a'), jmp(5)]; "trailing star")]
    #[test_case("b?a", vec![sp(5, 6), c('b'), c('a')]; "first lit is optional")]
    #[test_case("(a*)", vec![sv(2), sp(6, 8), c('a'), jmp(5), sv(3)]; "star")]
    #[test_case("(a*)*", vec![sp(5, 11), sv(2), sp(7, 9), c('a'), jmp(6), sv(3), jmp(4)]; "star star")]
    #[test_case("^foo", vec![BOL, c('f'), c('o'), c('o')]; "BOL then literals")]
    #[test_case("foo$", vec![c('f'), c('o'), c('o'), EOL]; "literals then EOL")]
    #[test]
    fn ast_compile_works(re: &str, expected: Vec<Op>) {
        let ast = parse(re).unwrap();
        let prog = compile_ast(ast, false);

        let mut full = vec![sp(3, 1), Op::Comp(Comp::TrueAny), sp(3, 1), sv(0)];
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
        let ast = parse(re).unwrap();
        let prog = optimise(compile_ast(ast, false));
        let mut full = vec![sp(3, 1), Op::Comp(Comp::TrueAny), sp(3, 1), sv(0)];
        full.extend(expected);
        full.extend([sv(1), Op::Match]);

        assert_eq!(prog, full);
    }
}
