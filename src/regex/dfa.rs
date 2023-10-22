use super::{re_to_postfix, CharClass, Error, Pfix};
use std::{collections::BTreeMap, mem::take};

const NFA_MAX_FRAGMENTS: usize = 1000;

#[derive(Debug, Clone, PartialEq, Eq)]
enum NfaState {
    Char(char),
    Class(CharClass),
    Any,
    TrueAny,
    Match,
    Split,
}

#[derive(Debug, Clone)]
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
        match &self.s {
            NfaState::Class(cls) => cls.matches_char(ch),
            NfaState::Char(c) => ch == *c,
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
            Pfix::Class(cls) => push_state(NfaState::Class(cls), &mut states, &mut stack),
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
            if self.nfa_states[s_ix].s == NfaState::Split {
                let State { out, out1, .. } = self.nfa_states[s_ix];
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
