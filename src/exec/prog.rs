//! An executable SE program
use crate::{
    buffer::GapBuffer,
    dot::Dot,
    exec::{
        Addr, Edit,
        compile::{Action, ActionKind, Compiler, Extract, Guard, Inst},
    },
    regex::{Match, Regex},
};
use aho_corasick::AhoCorasick;
use std::{
    cell::RefCell,
    cmp::min,
    collections::BTreeMap,
    fmt::{self, Write as _},
    io::{self, Write},
    mem,
    ops::{Deref, DerefMut},
    sync::{LazyLock, Mutex, OnceLock},
};

/// Variable usable in templates for injecting the current filename.
/// (Following the naming convention used in Awk)
const FNAME_VAR: &str = "$FILENAME";
/// Variable usable in templates for injecting the row that the current match starts at
const ROW_VAR: &str = "$ROW";
/// Variable usable in templates for injecting the column that the current match starts at
const COL_VAR: &str = "$COL";

static TEMPLATE_AC: OnceLock<AhoCorasick> = OnceLock::new();
const TEMPLATE_PATTERNS: [&str; 15] = [
    "$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8", "$9", FNAME_VAR, ROW_VAR, COL_VAR, "\\n",
    "\\t",
];

/// A shared pool of [Runner]s for executing programs
static RUNNER_POOL: LazyLock<Mutex<Vec<Runner>>> =
    LazyLock::new(|| Mutex::new((0..4).map(|_| Runner::new()).collect()));

/// An exec [Prog] uses structural regular expressions to identify a set of edit points within a
/// buffer which are then executed in parallel.
#[derive(Debug)]
pub struct Prog {
    inst: Inst,
    addrs: Vec<RefCell<Addr>>,
    re: Vec<RefCell<Regex>>,
    templates: Vec<String>,
}

impl Prog {
    pub fn compile(s: &str) -> Result<Self, String> {
        let mut c = Compiler::default();
        let inst = c.compile(s)?;
        let Compiler {
            addrs,
            re,
            templates,
        } = c;

        let addrs: Vec<_> = addrs.into_iter().map(RefCell::new).collect();

        let re = re
            .into_iter()
            .map(|re| {
                Regex::compile(re)
                    .map_err(|e| e.to_string())
                    .map(RefCell::new)
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            inst,
            addrs,
            re,
            templates,
        })
    }

    pub fn execute<E, W>(&self, ed: &mut E, fname: &str, out: &mut W) -> Result<Dot, String>
    where
        E: Edit,
        W: Write,
    {
        let mut runner = RunnerHandle::get_from_pool();

        runner.reset();
        ed.try_make_contiguous();

        let (from, to) = ed.current_dot().as_char_indices();
        let mut m = Match::synthetic(from, to.saturating_add(1));

        if let Some(new) = runner.execute_instruction(self, &self.inst, &m, ed) {
            m = new;
        }

        ed.begin_edit_transaction();
        let (from, to) = runner.apply_actions(fname, self, m, ed, out);
        ed.end_edit_transaction();

        // In the case of running against a lazy stream our initial `to` will be a sential value of
        // usize::MAX which needs to be clamped to the size of the input. For Buffers and GapBuffers
        // where we know that we should already be in bounds this is not required but the overhead
        // of always doing it is minimal as checking the number of chars in the buffer is O(1) due
        // to us caching the value.
        let ix_max = ed.len_chars();

        Ok(Dot::from_char_indices(min(from, ix_max), min(to, ix_max)))
    }
}

/// Reusable mutable state required for running a [Prog].
///
/// Automatically released back to a shared pool when dropped.
#[derive(Debug, Clone)]
struct Runner {
    actions: BTreeMap<Match, Vec<Action>>,
    ac_buf: Vec<aho_corasick::Match>,
    template_buf: GapBuffer,
    row_buf: String,
    col_buf: String,
}

impl Runner {
    fn new() -> Self {
        Self {
            actions: BTreeMap::new(),
            ac_buf: Vec::with_capacity(10),
            template_buf: GapBuffer::new(),
            row_buf: String::with_capacity(4),
            col_buf: String::with_capacity(4),
        }
    }

    fn reset(&mut self) {
        self.actions.clear();
        self.ac_buf.clear();
        self.template_buf.clear();
        self.row_buf.clear();
        self.col_buf.clear();
    }

    fn execute_instruction<E>(
        &mut self,
        prog: &Prog,
        inst: &Inst,
        m: &Match,
        ed: &E,
    ) -> Option<Match>
    where
        E: Edit,
    {
        match inst {
            Inst::Series(insts) => self.execute_series(prog, insts, m.clone(), ed),
            Inst::Parallel(insts) => self.execute_parallel(prog, insts, m.clone(), ed),
            Inst::Extract(ext) => self.execute_extract(prog, ext, m.clone(), ed),
            Inst::Filter(ext) => self.execute_filter(prog, ext, m.clone(), ed),
            Inst::Guard(g) => self.execute_guard(prog, g, m.clone(), ed),

            Inst::SetAddr(i) => {
                let mut addr = prog.addrs[*i].borrow_mut();
                let (from, to) = ed.map_addr(&mut addr).as_char_indices();

                Some(Match::synthetic(from, to))
            }

            // Actions end the chain
            Inst::Action(a) => {
                self.actions.entry(m.clone()).or_default().push(*a);
                None
            }
        }
    }

    /// Run each instruction in sequence, passing the updated match back each time.
    /// Returns the final match position
    fn execute_series<E>(
        &mut self,
        prog: &Prog,
        insts: &[Inst],
        mut m: Match,
        ed: &E,
    ) -> Option<Match>
    where
        E: Edit,
    {
        for inst in insts.iter() {
            m = self.execute_instruction(prog, inst, &m, ed)?;
        }

        Some(m)
    }

    /// Run each instruction against the original match, returning the original match
    fn execute_parallel<E>(
        &mut self,
        prog: &Prog,
        insts: &[Inst],
        m: Match,
        ed: &E,
    ) -> Option<Match>
    where
        E: Edit,
    {
        for inst in insts.iter() {
            self.execute_instruction(prog, inst, &m, ed);
        }

        Some(m)
    }

    fn execute_extract<E>(&mut self, prog: &Prog, ext: &Extract, m: Match, ed: &E) -> Option<Match>
    where
        E: Edit,
    {
        let mut re = prog.re[ext.re].borrow_mut();
        let (mut from, to) = m.loc();
        let mut last = None;

        while let Some(m) = re.find_between(ed, from, to) {
            // It's possible for the Regex we're using to match a 0-length string which
            // would cause us to get stuck trying to advance to the next match position.
            // If this happens we advance from by a character to ensure that we search
            // further in the input.
            let mut new_from = m.loc().1;
            if new_from == from {
                new_from += 1;
            }
            from = new_from;

            if let Some(new) = self.execute_series(prog, &ext.per_match, m, ed) {
                last = Some(new);
            }

            if from >= to || from >= ed.max_iter() {
                break;
            }
        }

        last
    }

    fn execute_filter<E>(&mut self, prog: &Prog, ext: &Extract, m: Match, ed: &E) -> Option<Match>
    where
        E: Edit,
    {
        let mut re = prog.re[ext.re].borrow_mut();
        let (mut from, to) = m.loc();
        let mut last = None;

        while let Some(m) = re.find_between(ed, from, to) {
            let (new_from, new_to) = m.loc();
            if from < new_from {
                let m = Match::synthetic(from, new_from);
                if let Some(new) = self.execute_series(prog, &ext.per_match, m, ed) {
                    last = Some(new);
                }
            }

            from = new_to;
            if from > to || from >= ed.max_iter() {
                break;
            }
        }

        if from < to {
            let m = Match::synthetic(from, to);
            if let Some(new) = self.execute_series(prog, &ext.per_match, m, ed) {
                last = Some(new);
            }
        }

        last
    }

    fn execute_guard<E>(&mut self, prog: &Prog, g: &Guard, m: Match, ed: &E) -> Option<Match>
    where
        E: Edit,
    {
        let mut re = prog.re[g.re].borrow_mut();
        let (from, to) = m.loc();
        let matching = re.matches_between(ed, from, to);

        if matching && !g.if_matching.is_empty() {
            self.execute_series(prog, &g.if_matching, m.clone(), ed)
        } else if !matching && !g.if_not_matching.is_empty() {
            self.execute_series(prog, &g.if_not_matching, m.clone(), ed)
        } else {
            None
        }
    }

    fn apply_actions<E, W>(
        &mut self,
        fname: &str,
        prog: &Prog,
        mut m: Match,
        ed: &mut E,
        out: &mut W,
    ) -> (usize, usize)
    where
        E: Edit,
        W: Write,
    {
        todo!()
    }
}

#[derive(Debug, Clone)]
struct RunnerHandle(Option<Runner>);

impl RunnerHandle {
    fn get_from_pool() -> Self {
        let mut guard = RUNNER_POOL.lock().unwrap();
        match guard.pop() {
            Some(r) => Self(Some(r)),
            None => Self(Some(Runner::new())),
        }
    }
}

impl Deref for RunnerHandle {
    type Target = Runner;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}

impl DerefMut for RunnerHandle {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}

impl Drop for RunnerHandle {
    fn drop(&mut self) {
        let mut inner = None;
        mem::swap(&mut self.0, &mut inner);
        RUNNER_POOL.lock().unwrap().push(inner.unwrap());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const PROG: &str = r#"
x/^impl(?:<.*?>)?.*? (\w+)@*?^\}/
v/^impl(?:<.*?>)?.*? for/ {
  p/\nimpl $1 ($FILENAME:$ROW:$COL)\n/;
  x/fn@*?\{/ {
    g/->/
    x/fn (\w+)@*?-> (.*?)\w*\{/ {
      g/&('. )?mut self/ p/  mut $1 -> $2\n/;
      v/&('. )?mut self/ p/      $1 -> $2\n/;
    };

    v/->/
    x/fn (\w+)@*\{/ {
      g/&('. )?mut self/ p/  mut $1 -> ()\n/;
      v/&('. )?mut self/ p/      $1 -> ()\n/;
    };
  };
}"#;

    #[test]
    fn execute_build_the_correct_actions() {
        use crate::{exec::Address, regex::Haystack};

        let prog = Prog::compile(PROG).unwrap();
        let mut runner = RunnerHandle::get_from_pool();

        let mut gb = GapBuffer::from(include_str!("../buffer/internal.rs"));
        gb.try_make_contiguous();

        let (from, to) = gb.current_dot().as_char_indices();
        let m = Match::synthetic(from, to.saturating_add(1));
        runner.execute_instruction(&prog, &prog.inst, &m, &gb);

        panic!("{:#?}", runner.actions);
    }
}
