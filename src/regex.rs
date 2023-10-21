//! A simple regex engine for operating on character streams and supporting
//! the Sam text editor's structural regular expressions.
//!
//! The implementation of this engine is adapted from the one presented by
//! Russ Cox here:
//!   https://swtch.com/~rsc/regexp/regexp1.html
//!
//! Thompson's original paper on writing a regex engine can be found here:
//!   https://dl.acm.org/doi/pdf/10.1145/363347.363387
use std::{collections::BTreeMap, mem::take};

const POSTFIX_BUF_SIZE: usize = 2000;
const POSTFIX_MAX_PARENS: usize = 100;
const NFA_MAX_FRAGMENTS: usize = 1000;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    EmptyParens,
    EmptyRegex,
    InvalidEscape(char),
    InvalidRepetition,
    ReTooLong,
    TooManyParens,
    UnbalancedAlt,
    UnbalancedParens,
}

// Postfix form notation for building up the compiled state machine
#[derive(Debug, PartialEq, Eq)]
enum Pfix {
    Char(char),
    Concat,
    Alt,
    Quest,
    Star,
    Plus,
    Any,
    TrueAny,
}
fn insert_cats(natom: &mut usize, output: &mut Vec<Pfix>) {
    *natom -= 1;
    while *natom > 0 {
        output.push(Pfix::Concat);
        *natom -= 1;
    }
}

fn insert_alts(nalt: &mut usize, output: &mut Vec<Pfix>) {
    while *nalt > 0 {
        output.push(Pfix::Alt);
        *nalt -= 1;
    }
}

fn push_cat(natom: &mut usize, output: &mut Vec<Pfix>) {
    if *natom > 1 {
        output.push(Pfix::Concat);
        *natom -= 1;
    }
}

fn push_atom(p: Pfix, natom: &mut usize, output: &mut Vec<Pfix>) {
    push_cat(natom, output);
    output.push(p);
    *natom += 1;
}

fn push_rep(p: Pfix, natom: usize, output: &mut Vec<Pfix>) -> Result<(), Error> {
    if natom == 0 {
        return Err(Error::InvalidRepetition);
    }
    output.push(p);
    Ok(())
}

fn re_to_postfix(re: &str) -> Result<Vec<Pfix>, Error> {
    #[derive(Clone, Copy)]
    struct Paren {
        natom: usize,
        nalt: usize,
    }

    if re.is_empty() {
        return Err(Error::EmptyRegex);
    } else if re.len() > POSTFIX_BUF_SIZE / 2 {
        return Err(Error::ReTooLong);
    }

    let mut output = Vec::with_capacity(POSTFIX_BUF_SIZE);
    let mut paren: [Paren; POSTFIX_MAX_PARENS] = [Paren { natom: 0, nalt: 0 }; POSTFIX_MAX_PARENS];
    let mut natom = 0;
    let mut nalt = 0;
    let mut p = 0;
    let mut escaping = false;

    for ch in re.chars() {
        match ch {
            '\\' => escaping = true,
            ch if escaping => {
                let atom = match ch {
                    '\\' | '*' | '+' | '?' | '.' | '@' | '(' | ')' => Pfix::Char(ch),
                    'n' => Pfix::Char('\n'),
                    'r' => Pfix::Char('\r'),
                    't' => Pfix::Char('\t'),
                    _ => return Err(Error::InvalidEscape(ch)),
                };
                push_atom(atom, &mut natom, &mut output);
                escaping = false;
            }

            '(' => {
                if p >= POSTFIX_MAX_PARENS {
                    return Err(Error::TooManyParens);
                }
                push_cat(&mut natom, &mut output);
                paren[p].natom = natom;
                paren[p].nalt = nalt;
                p += 1;
                natom = 0;
                nalt = 0;
            }

            ')' => {
                if p == 0 {
                    return Err(Error::UnbalancedParens);
                } else if natom == 0 {
                    return Err(Error::EmptyParens);
                }

                insert_cats(&mut natom, &mut output);
                insert_alts(&mut nalt, &mut output);

                p -= 1;
                natom = paren[p].natom;
                nalt = paren[p].nalt;
                natom += 1;
            }

            '|' => {
                if natom == 0 {
                    return Err(Error::UnbalancedAlt);
                }

                insert_cats(&mut natom, &mut output);
                nalt += 1;
            }

            '*' => push_rep(Pfix::Star, natom, &mut output)?,
            '+' => push_rep(Pfix::Plus, natom, &mut output)?,
            '?' => push_rep(Pfix::Quest, natom, &mut output)?,
            '.' => push_atom(Pfix::Any, &mut natom, &mut output),
            '@' => push_atom(Pfix::TrueAny, &mut natom, &mut output),
            ch => push_atom(Pfix::Char(ch), &mut natom, &mut output),
        }
    }

    if p != 0 {
        return Err(Error::UnbalancedParens);
    }

    insert_cats(&mut natom, &mut output);
    insert_alts(&mut nalt, &mut output);

    Ok(output)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NfaState {
    Char(char),
    Any,
    TrueAny,
    Match,
    Split,
}

#[derive(Debug, Clone, Copy)]
struct State {
    s: NfaState,
    out: usize,
    out1: Option<usize>,
    last_list: usize,
}

impl State {
    fn new(s: NfaState, out: usize, out1: Option<usize>) -> Self {
        Self {
            s,
            out,
            out1,
            last_list: 0,
        }
    }

    fn matches(&self, ch: char) -> bool {
        match self.s {
            NfaState::Char(c) => ch == c,
            NfaState::Any => ch != '\n',
            NfaState::TrueAny => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
enum Sptr {
    Out(usize),
    Out1(usize),
}

#[derive(Debug)]
struct Fragment {
    start: usize,
    out: Vec<Sptr>,
}

/// Connect all dangling State pointers for this Fragment to ptr.
fn patch(states: &mut [State], out: &[Sptr], state_ix: usize) {
    for s in out.iter() {
        match s {
            Sptr::Out(i) => states[*i].out = state_ix,
            Sptr::Out1(i) => states[*i].out1 = Some(state_ix),
        }
    }
}

fn push_state(s: NfaState, states: &mut Vec<State>, stack: &mut Vec<Fragment>) {
    let ix = states.len();
    states.push(State::new(s, usize::MAX, None));
    stack.push(Fragment {
        start: ix,
        out: vec![Sptr::Out(ix)],
    });
}

fn post_to_nfa(postfix: Vec<Pfix>) -> Regex {
    let mut stack: Vec<Fragment> = Vec::with_capacity(NFA_MAX_FRAGMENTS);
    // states[0] is always our match state which loops and points to itself
    let mut states = vec![State::new(NfaState::Match, 0, None)];

    for c in postfix.into_iter() {
        match c {
            // Concatenation
            // -> [e1] -> [e2] ->
            Pfix::Concat => {
                let e2 = stack.pop().unwrap();
                let e1 = stack.pop().unwrap();
                patch(&mut states, &e1.out, e2.start);
                stack.push(Fragment {
                    start: e1.start,
                    out: e2.out,
                });
            }

            // Alternation
            //    + -> [e1] ->
            // -> O
            //    + -> [e2] ->
            Pfix::Alt => {
                let mut e2 = stack.pop().unwrap();
                let mut e1 = stack.pop().unwrap();
                let ix = states.len();
                states.push(State::new(NfaState::Split, e1.start, Some(e2.start)));
                e1.out.append(&mut e2.out);
                stack.push(Fragment {
                    start: ix,
                    out: e1.out,
                });
            }

            // Zero or one (optional)
            //
            //    + -> [e] ->
            // -> O
            //    + -------->
            Pfix::Quest => {
                let Fragment { start, mut out } = stack.pop().unwrap();
                let ix = states.len();
                states.push(State::new(NfaState::Split, start, None));
                out.push(Sptr::Out1(ix));
                stack.push(Fragment { start: ix, out });
            }

            // Zero or more
            //
            //    + -> [e] -+
            //    |         |
            // -> O <------+
            //    |
            //    + -------->
            Pfix::Star => {
                let Fragment { start, out } = stack.pop().unwrap();
                let ix = states.len();
                states.push(State::new(NfaState::Split, start, None));
                patch(&mut states, &out, ix);
                stack.push(Fragment {
                    start: ix,
                    out: vec![Sptr::Out1(ix)],
                });
            }

            // One or more
            //
            //     +-----+
            //     v     |
            // -> [e] -> O ->
            Pfix::Plus => {
                let Fragment { start, out } = stack.pop().unwrap();
                let ix = states.len();
                states.push(State::new(NfaState::Split, start, None));
                patch(&mut states, &out, ix);
                stack.push(Fragment {
                    start,
                    out: vec![Sptr::Out1(ix)],
                });
            }

            // Match node
            Pfix::Char(c) => push_state(NfaState::Char(c), &mut states, &mut stack),
            Pfix::TrueAny => push_state(NfaState::TrueAny, &mut states, &mut stack),
            Pfix::Any => push_state(NfaState::Any, &mut states, &mut stack),
        }
    }

    let Fragment { start, out } = stack.pop().expect("to have an element to pop");
    patch(&mut states, &out, 0);

    Regex {
        start,
        nfa_states: states,
        dfa_states: Default::default(),
        dfa: Default::default(),
        list_id: 0,
    }
}

/// A cached copy of NFA states along with a map of the next DFA state to transition
/// to for a given input character.
///
///   !!-> This will be limited to ascii inputs only under the current implementation
#[derive(Debug, Clone)]
struct DState {
    nfa_states: Vec<usize>,
    next: [Option<usize>; 256],
}

impl DState {
    fn new(nfa_states: Vec<usize>) -> Self {
        Self {
            nfa_states,
            next: [None; 256],
        }
    }

    fn next_for(&mut self, ch: char) -> &mut Option<usize> {
        let ix = ((ch as u16) & 0xFF) as usize;
        &mut self.next[ix]
    }
}

#[derive(Debug, Clone)]
pub struct Regex {
    /// NFA index to start the match from. 0 == matched but 1 might not be the starting
    /// state depending on the exact regex being executed
    start: usize,
    /// Compiled NFA states to be matched against the input
    nfa_states: Vec<State>,
    /// On the fly cached DFA states built up as the machine is executed
    dfa_states: Vec<DState>,
    /// Map of sets of NFA states to their cached DFA state representation
    dfa: BTreeMap<Vec<usize>, usize>,
    /// Monotonically increasing index used to dedup NFA states.
    /// Will overflow at some point if a given regex is used a VERY large number of times
    list_id: usize,
}

impl Regex {
    pub fn compile(re: &str) -> Result<Self, Error> {
        let pfix = re_to_postfix(re)?;

        Ok(post_to_nfa(pfix))
    }

    pub fn matches_str(&mut self, input: &str) -> bool {
        self.matches_iter(input.chars().enumerate())
    }

    // TODO: track the start of the match so we can return the range that is matching
    pub fn matches_iter<I>(&mut self, input: I) -> bool
    where
        I: Iterator<Item = (usize, char)>,
    {
        let mut clist = Vec::with_capacity(self.nfa_states.len());
        let mut nlist = Vec::with_capacity(self.nfa_states.len());

        // A little gross but this avoids some ownership issues that result in us needing
        // to clone the dfa_states as we match the nfa states against the input
        let mut dfa_states = take(&mut self.dfa_states);

        // Make sure that we don't clash with any list IDs from a previous run
        self.list_id += 1;

        self.add_state(&mut clist, Some(self.start));
        clist.sort_unstable();
        let mut d_ix = self.get_or_create_dstate(&clist, &mut dfa_states);
        self.dfa.insert(clist, d_ix);

        for (_, ch) in input {
            // If we have this DFA state already precomputed and cached then use it...
            if let Some(next) = dfa_states[d_ix].next_for(ch) {
                d_ix = *next;
                continue;
            }

            // ...otherwise compute the new DFA state and add it to the cache
            self.list_id += 1;
            nlist.clear();

            for &s_ix in dfa_states[d_ix].nfa_states.iter() {
                let s = &self.nfa_states[s_ix];
                if s.matches(ch) {
                    self.add_state(&mut nlist, Some(s.out));
                }
            }

            nlist.sort_unstable();

            let new_dfa = self.get_or_create_dstate(&nlist, &mut dfa_states);
            *dfa_states[d_ix].next_for(ch) = Some(new_dfa);
            d_ix = new_dfa;
        }

        // Replace the cached dfa_states for the next run (if there is one)
        self.dfa_states = dfa_states;
        self.dfa_states[d_ix].nfa_states.iter().any(|&ix| ix == 0)
    }

    fn add_state(&mut self, lst: &mut Vec<usize>, state_ix: Option<usize>) {
        let s_ix = match state_ix {
            Some(ix) => ix,
            None => return,
        };

        if self.nfa_states[s_ix].last_list != self.list_id {
            self.nfa_states[s_ix].last_list = self.list_id;
            let State { s, out, out1, .. } = self.nfa_states[s_ix];
            if s == NfaState::Split {
                self.add_state(lst, Some(out));
                self.add_state(lst, out1);
                return;
            }
            lst.push(s_ix);
        }
    }

    fn get_or_create_dstate(&mut self, lst: &Vec<usize>, dfa_states: &mut Vec<DState>) -> usize {
        match self.dfa.get(lst) {
            Some(ix) => *ix,
            None => {
                dfa_states.push(DState::new(lst.clone()));
                dfa_states.len() - 1
            }
        }
    }
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
