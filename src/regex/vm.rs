//! Virtual machine based
use super::{re_to_postfix, CharClass, Error, Pfix};

pub struct Regex {
    prog: Prog,
    gen: usize,
}

impl Regex {
    pub fn compile(re: &str) -> Result<Self, Error> {
        let pfix = re_to_postfix(re)?;
        let prog = compile(pfix);

        Ok(Self { prog, gen: 1 })
    }

    pub fn matches_str(&mut self, input: &str) -> bool {
        self.matches_iter(input.chars().enumerate())
    }

    // TODO: track the start of the match so we can return the range that is matching
    pub fn matches_iter<I>(&mut self, input: I) -> bool
    where
        I: Iterator<Item = (usize, char)>,
    {
        let mut clist = Vec::new();
        let mut nlist = Vec::new();

        self.add_thread(&mut clist, 0);
        self.gen += 1;

        for (_, ch) in input {
            if clist.is_empty() {
                break;
            }
            nlist.clear();

            for tpc in clist.iter() {
                match &self.prog[*tpc].op {
                    Op::Char(c) if *c == ch => self.add_thread(&mut nlist, tpc + 1),
                    Op::Class(cls) if cls.matches_char(ch) => self.add_thread(&mut nlist, tpc + 1),
                    Op::Any if ch != '\n' => self.add_thread(&mut nlist, tpc + 1),
                    Op::TrueAny => self.add_thread(&mut nlist, tpc + 1),

                    Op::Match => return true,

                    // Jump & Split are handled in add_thread.
                    // Non-matching comparison ops result in that thread dying.
                    _ => (),
                }
            }

            (clist, nlist) = (nlist, clist);
            self.gen += 1;
        }

        clist.into_iter().any(|tpc| self.prog[tpc].op == Op::Match)
    }

    fn add_thread(&mut self, lst: &mut Vec<usize>, pc: usize) {
        if self.prog[pc].gen == self.gen {
            return;
        }
        self.prog[pc].gen = self.gen;

        match self.prog[pc].op.clone() {
            Op::Jump(l1) => self.add_thread(lst, l1),
            Op::Split(l1, l2) => {
                self.add_thread(lst, l1);
                self.add_thread(lst, l2);
            }

            _ => {
                lst.push(pc);
            }
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

fn compile(pfix: Vec<Pfix>) -> Prog {
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
    prog.into_iter().map(|op| Inst { op, gen: 0 }).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    // This is the example given by Russ in his article
    #[test]
    fn postfix_construction_works() {
        let re = "a(bb)+a";
        let expected = vec![
            Pfix::Char('a'),
            Pfix::Char('b'),
            Pfix::Char('b'),
            Pfix::Concat,
            Pfix::Plus,
            Pfix::Concat,
            Pfix::Char('a'),
            Pfix::Concat,
        ];

        assert_eq!(re_to_postfix(re).unwrap(), expected);
    }

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
        let ops: Vec<Op> = prog.into_iter().map(|inst| inst.op).collect();
        assert_eq!(&ops, expected);
    }

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

        assert_eq!(r.matches_str(s), matches);
    }

    // This is the pathological case that Cox covers in his article which leads
    // to exponential behaviour in backtracking based implementations.
    #[test]
    fn pathological_match_doesnt_explode() {
        let s = "a".repeat(100);
        let mut re = "a?".repeat(100);
        re.push_str(&s);

        let mut r = Regex::compile(&re).unwrap();
        assert!(r.matches_str(&s));
    }

    // Make sure that the previous cached state for a given Regex doesn't cause
    // any strange behaviour for future matches
    #[test]
    fn repeated_match_works() {
        let re = "a(bb)+a";
        let mut r = Regex::compile(re).unwrap();

        for _ in 0..10 {
            assert!(r.matches_str("abbbba"));
            assert!(!r.matches_str("foo"));
        }
    }
}
