//! A simple regex engine for operating on character streams and supporting
//! the Sam text editor's structural regular expressions.
//!
//! The implementation of this engine is adapted from the one presented by
//! Russ Cox here:
//!   https://swtch.com/~rsc/regexp/regexp1.html
//!
//! Thompson's original paper on writing a regex engine can be found here:
//!   https://dl.acm.org/doi/pdf/10.1145/363347.363387
use std::{collections::BTreeMap, ptr::null_mut};

const POSTFIX_BUF_SIZE: usize = 2000;
const POSTFIX_MAX_PARENS: usize = 100;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    EmptyParens,
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
}

fn re_to_postfix(re: &str) -> Result<Vec<Pfix>, Error> {
    #[derive(Clone, Copy)]
    struct Paren {
        natom: usize,
        nalt: usize,
    }

    if re.len() > POSTFIX_BUF_SIZE / 2 {
        return Err(Error::ReTooLong);
    }

    let mut output = Vec::with_capacity(POSTFIX_BUF_SIZE);
    let mut paren: [Paren; POSTFIX_MAX_PARENS] = [Paren { natom: 0, nalt: 0 }; POSTFIX_MAX_PARENS];

    let mut natom = 0;
    let mut nalt = 0;
    let mut p = 0;

    // C: while(--natom > 0) { *dst++ = '.'; }
    let insert_cats = |natom: &mut usize, output: &mut Vec<Pfix>| {
        *natom -= 1;
        while *natom > 0 {
            output.push(Pfix::Concat);
            *natom -= 1;
        }
    };

    // C: for(; nalt > 0; nalt--) { *dts++ = '|'; }
    let insert_alts = |nalt: &mut usize, output: &mut Vec<Pfix>| {
        while *nalt > 0 {
            output.push(Pfix::Char('|'));
            *nalt -= 1;
        }
    };

    for ch in re.chars() {
        match ch {
            '(' => {
                if natom > 1 {
                    natom -= 1;
                    output.push(Pfix::Concat);
                }
                if p >= POSTFIX_MAX_PARENS {
                    return Err(Error::TooManyParens);
                }
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

            '*' | '+' | '?' => {
                if natom == 0 {
                    return Err(Error::InvalidRepetition);
                }
                output.push(Pfix::Char(ch));
            }

            ch => {
                if natom > 1 {
                    output.push(Pfix::Concat);
                    natom -= 1;
                }
                output.push(Pfix::Char(ch));
                natom += 1;
            }
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
    Match,
    Split,
}

#[derive(Debug, Clone, Copy)]
struct State {
    s: NfaState,
    out: *mut State,
    out1: *mut State,
    last_list: usize,
}

impl State {
    fn new(s: NfaState, out: *mut State, out1: *mut State) -> Self {
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
            NfaState::Any => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
struct Fragment {
    start: *mut State,
    out: Vec<*mut *mut State>,
}

impl Fragment {
    /// Connect all dangling State pointers for this Fragment to ptr.
    fn patch(&self, ptr: *mut State) {
        for &o in self.out.iter() {
            unsafe { *o = ptr };
        }
    }
}

fn post_to_nfa(postfix: Vec<Pfix>) -> Regex {
    let mut stack: Vec<Fragment> = Vec::with_capacity(1000);
    let mut nstates = 0;

    for c in postfix.into_iter() {
        match c {
            // Concatenation of two states
            // -> [e1] -> [e2] ->
            Pfix::Concat => {
                let e2 = stack.pop().unwrap();
                let e1 = stack.pop().unwrap();
                e1.patch(e2.start);
                stack.push(Fragment {
                    start: e1.start,
                    out: e2.out,
                });
            }

            // Alternation
            //    + -> [e1] ->
            // -> O
            //    + -> [e2] ->
            Pfix::Char('|') => {
                let mut e2 = stack.pop().unwrap();
                let mut e1 = stack.pop().unwrap();
                let s = Box::new(State::new(NfaState::Split, e1.start, e2.start));
                nstates += 1;
                e1.out.append(&mut e2.out);
                stack.push(Fragment {
                    start: Box::into_raw(s),
                    out: e1.out,
                });
            }

            // Zero or one (optional)
            //
            //    + -> [e] ->
            // -> O
            //    + -------->
            Pfix::Char('?') => {
                let mut e = stack.pop().unwrap();
                let mut s = Box::new(State::new(NfaState::Split, e.start, null_mut()));
                nstates += 1;
                e.out.push(&mut s.out1 as *mut _);
                stack.push(Fragment {
                    start: Box::into_raw(s),
                    out: e.out,
                });
            }

            // Zero or more
            //
            //    + -> [e] -+
            //    |         |
            // -> O <------+
            //    |
            //    + -------->
            Pfix::Char('*') => {
                let e = stack.pop().unwrap();
                let mut s = Box::new(State::new(NfaState::Split, e.start, null_mut()));
                nstates += 1;
                let out = vec![&mut s.out1 as *mut _];
                let s_ptr = Box::into_raw(s);
                e.patch(s_ptr);
                stack.push(Fragment { start: s_ptr, out });
            }

            // One or more
            //
            //     +-----+
            //     v     |
            // -> [e] -> O ->
            Pfix::Char('+') => {
                let e = stack.pop().unwrap();
                let mut s = Box::new(State::new(NfaState::Split, e.start, null_mut()));
                nstates += 1;
                let out = vec![&mut s.out1 as *mut _];
                e.patch(Box::into_raw(s));

                stack.push(Fragment {
                    start: e.start,
                    out,
                });
            }

            // Character literal
            //
            //       c
            //  -> O ->
            Pfix::Char(c) => {
                let nfas = if c == '.' {
                    NfaState::Any
                } else {
                    NfaState::Char(c)
                };
                let mut s = Box::new(State::new(nfas, null_mut(), null_mut()));
                nstates += 1;
                let out = vec![&mut s.out as *mut _];
                stack.push(Fragment {
                    start: Box::into_raw(s),
                    out,
                });
            }
        }
    }

    let e = stack.pop().expect("to have an element to pop");
    let matched = Box::new(State::new(NfaState::Match, null_mut(), null_mut()));
    e.patch(Box::into_raw(matched));

    Regex {
        start: e.start,
        dfa: Default::default(),
        nstates,
    }
}

/// A cached copy of nfa states along with a map of the next Dfa state to transition
/// to for a given input character.
///
///   !!-> This will be limited to ascii inputs only under the current implementation
#[derive(Debug, Clone)]
struct DState {
    nfa_states: Vec<*mut State>,
    next: [*mut DState; 256],
}

impl DState {
    fn new(nfa_states: Vec<*mut State>) -> Self {
        Self {
            nfa_states,
            next: [null_mut(); 256],
        }
    }

    fn next_dfa_state(&self, ch: char) -> Option<*mut DState> {
        let ix = ((ch as u16) & 0xFF) as usize;
        let next = self.next[ix];
        if !next.is_null() {
            Some(next)
        } else {
            None
        }
    }

    fn add_state(&mut self, ch: char, p: *mut DState) {
        let ix = ((ch as u16) & 0xFF) as usize;
        self.next[ix] = p;
    }
}

unsafe fn child_ptrs(s: *mut State, current: &mut Vec<*mut State>) {
    for ptr in [(*s).out, (*s).out1] {
        if !ptr.is_null() && !current.contains(&ptr) {
            current.push(ptr);
            child_ptrs(ptr, current)
        }
    }
}

#[derive(Debug)]
pub struct Regex {
    start: *mut State,
    dfa: BTreeMap<Vec<*mut State>, *mut DState>,
    nstates: usize,
}

impl Drop for Regex {
    fn drop(&mut self) {
        // SAFETY: self.start is non-null and child_ptrs null checks before recursing
        unsafe {
            for &p in self.dfa.values() {
                drop(Box::from_raw(p));
            }

            for p in self.state_ptrs().into_iter() {
                drop(Box::from_raw(p));
            }
        }
    }
}

// TODO: extract match position and match against a character iterator
impl Regex {
    pub fn compile(re: &str) -> Result<Self, Error> {
        let pfix = re_to_postfix(re)?;

        Ok(post_to_nfa(pfix))
    }

    unsafe fn state_ptrs(&self) -> Vec<*mut State> {
        let mut ptrs = Vec::new();
        child_ptrs(self.start, &mut ptrs);
        ptrs.push(self.start);
        ptrs.sort_unstable();
        ptrs.dedup();

        ptrs
    }

    pub fn matches(&mut self, input: &str) -> bool {
        self.reset();

        let mut clist = Vec::with_capacity(self.nstates);
        let mut nlist = Vec::with_capacity(self.nstates);
        let mut list_id = 1;

        add_state(&mut clist, self.start, list_id);
        let mut d = Box::into_raw(Box::new(DState::new(clist.clone())));
        self.dfa.insert(clist, d);

        unsafe {
            for ch in input.chars() {
                // If we have this DFA state already precomputed and cached then use it...
                if let Some(next) = (*d).next_dfa_state(ch) {
                    d = next;
                    continue;
                }

                // ...otherwise compute the new DFA state and add it to the cache
                list_id += 1;
                nlist.clear();
                for &s in (*d).nfa_states.iter() {
                    if (*s).matches(ch) {
                        add_state(&mut nlist, (*s).out, list_id);
                    }
                }

                let new_dfa = self.get_or_create_dstate(nlist.clone());
                (*d).add_state(ch, new_dfa);
                d = new_dfa;
            }

            (*d).nfa_states.iter().any(|&s| (*s).s == NfaState::Match)
        }
    }

    #[inline]
    fn reset(&mut self) {
        // SAFETY: self.start is non-null and child_ptrs null checks before recursing
        unsafe {
            if (*self.start).last_list != 0 {
                for p in self.state_ptrs().into_iter() {
                    (*p).last_list = 0;
                }
            }
        }
    }

    #[inline]
    fn get_or_create_dstate(&mut self, lst: Vec<*mut State>) -> *mut DState {
        let d = self
            .dfa
            .entry(lst.clone())
            .or_insert_with(|| Box::into_raw(Box::new(DState::new(lst.clone()))));

        *d
    }
}

fn add_state(lst: &mut Vec<*mut State>, s: *mut State, list_id: usize) {
    if s.is_null() {
        return;
    }

    unsafe {
        if (*s).last_list != list_id {
            (*s).last_list = list_id;
            if (*s).s == NfaState::Split {
                add_state(lst, (*s).out, list_id);
                add_state(lst, (*s).out1, list_id);
                return;
            }
            lst.push(s);
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
            Pfix::Char('+'),
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
    #[test]
    fn match_works(re: &str, s: &str, matches: bool) {
        let mut r = Regex::compile(re).unwrap();

        assert_eq!(r.matches(s), matches);
    }
}
