//! An executable SE program
use crate::{
    buffer::GapBuffer,
    dot::Dot,
    regex::{Match, Regex},
    structex::{
        Addr, Edit, Error,
        compile::{Action, ActionKind, Compiler, Extract, Guard, Inst},
    },
};
use aho_corasick::AhoCorasick;
use std::{
    borrow::Cow,
    cell::RefCell,
    cmp::min,
    collections::BTreeMap,
    fmt::Write as _,
    io::Write,
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

/// An exec [Program] uses structural regular expressions to identify a set of edit points within a
/// buffer which are then executed in parallel.
#[derive(Debug)]
pub struct Program {
    inst: Inst,
    addrs: Vec<RefCell<Addr>>,
    re: Vec<RefCell<Regex>>,
    templates: Vec<String>,
}

impl Program {
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

    pub fn execute<E, W>(&self, ed: &mut E, fname: &str, out: &mut W) -> Result<Dot, Error>
    where
        E: Edit,
        W: Write,
    {
        let mut runner = RunnerHandle::get_from_pool();

        runner.reset();
        ed.try_make_contiguous();

        let (from, to) = ed.current_dot().as_char_indices();
        let mut m = Match::synthetic(from, to.saturating_add(1));

        if let Some(new) = runner.execute_instruction(self, &self.inst, &m, ed)? {
            m = new;
        }

        ed.begin_edit_transaction();
        let (from, to) = runner.apply_actions(fname, self, m, ed, out)?;
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
    actions: BTreeMap<Match, Action>,
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
        prog: &Program,
        inst: &Inst,
        m: &Match,
        ed: &E,
    ) -> Result<Option<Match>, Error>
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

                Ok(Some(Match::synthetic(from, to)))
            }

            // Actions end the chain
            Inst::Action(a) => match self.actions.insert(m.clone(), *a) {
                Some(a2) => {
                    let (row, col) = m.loc();
                    Err(Error::OverlappingMatches(
                        row,
                        col,
                        format!("overlapping actions: {a2:?} {a:?}"),
                    ))
                }
                None => Ok(None),
            },
        }
    }

    /// Run each instruction in sequence, passing the updated match back each time.
    /// Returns the final match position
    fn execute_series<E>(
        &mut self,
        prog: &Program,
        insts: &[Inst],
        mut m: Match,
        ed: &E,
    ) -> Result<Option<Match>, Error>
    where
        E: Edit,
    {
        for inst in insts.iter() {
            m = match self.execute_instruction(prog, inst, &m, ed)? {
                Some(m) => m,
                None => break,
            };
        }

        Ok(Some(m))
    }

    /// Run each instruction against the original match, returning the original match
    fn execute_parallel<E>(
        &mut self,
        prog: &Program,
        insts: &[Inst],
        m: Match,
        ed: &E,
    ) -> Result<Option<Match>, Error>
    where
        E: Edit,
    {
        for inst in insts.iter() {
            self.execute_instruction(prog, inst, &m, ed)?;
        }

        Ok(Some(m))
    }

    fn execute_extract<E>(
        &mut self,
        prog: &Program,
        ext: &Extract,
        m: Match,
        ed: &E,
    ) -> Result<Option<Match>, Error>
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

            if let Some(new) = self.execute_series(prog, &ext.per_match, m, ed)? {
                last = Some(new);
            }

            if from >= to || from >= ed.max_iter() {
                break;
            }
        }

        Ok(last)
    }

    fn execute_filter<E>(
        &mut self,
        prog: &Program,
        ext: &Extract,
        m: Match,
        ed: &E,
    ) -> Result<Option<Match>, Error>
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
                if let Some(new) = self.execute_series(prog, &ext.per_match, m, ed)? {
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
            if let Some(new) = self.execute_series(prog, &ext.per_match, m, ed)? {
                last = Some(new);
            }
        }

        Ok(last)
    }

    fn execute_guard<E>(
        &mut self,
        prog: &Program,
        g: &Guard,
        m: Match,
        ed: &E,
    ) -> Result<Option<Match>, Error>
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
            Ok(None)
        }
    }

    fn apply_actions<E, W>(
        &mut self,
        fname: &str,
        prog: &Program,
        m: Match,
        ed: &mut E,
        out: &mut W,
    ) -> Result<(usize, usize), Error>
    where
        E: Edit,
        W: Write,
    {
        let mut offset: isize = 0;
        let (from, to) = m.loc();
        let mut dot = Dot::from_char_indices(from, to);
        let actions = mem::take(&mut self.actions);

        for (mut m, action) in actions.into_iter() {
            m.apply_offset(offset);

            let cur_len = ed.len_chars() as isize;
            dot = self.apply_action(prog, &action, &m, fname, ed, out)?;
            let new_len = ed.len_chars() as isize;
            offset += new_len - cur_len;
        }

        Ok(dot.as_char_indices())
    }

    fn apply_action<E, W>(
        &mut self,
        prog: &Program,
        action: &Action,
        m: &Match,
        fname: &str,
        ed: &mut E,
        out: &mut W,
    ) -> Result<Dot, Error>
    where
        E: Edit,
        W: Write,
    {
        let (from, to) = m.loc();
        let (from, to) = match action.kind {
            ActionKind::Print => {
                let pat = &prog.templates[action.template];
                self.template_match(pat, m, ed, fname)?;
                write!(out, "{}", self.template_buf.as_str())?;
                (from, to)
            }

            ActionKind::Insert => {
                let pat = &prog.templates[action.template];
                self.template_match(pat, m, ed, fname)?;
                ed.insert(from, self.template_buf.as_str());
                (from, to + self.template_buf.len_chars())
            }

            ActionKind::Append => {
                let pat = &prog.templates[action.template];
                self.template_match(pat, m, ed, fname)?;
                ed.insert(to, self.template_buf.as_str());
                (from, to + self.template_buf.len_chars())
            }

            ActionKind::Change => {
                let pat = &prog.templates[action.template];
                self.template_match(pat, m, ed, fname)?;
                ed.remove(from, to);
                ed.insert(from, self.template_buf.as_str());
                (from, from + self.template_buf.len_chars())
            }

            ActionKind::Delete => {
                ed.remove(from, to);
                (from, from)
            }
        };

        Ok(Dot::from_char_indices(from, to))
    }

    fn template_match<E>(&mut self, s: &str, m: &Match, ed: &E, fname: &str) -> Result<(), Error>
    where
        E: Edit,
    {
        self.ac_buf.clear();
        self.template_buf.clear();
        self.template_buf.insert_str(0, s);

        self.ac_buf.extend(
            TEMPLATE_AC
                .get_or_init(|| {
                    AhoCorasick::new(TEMPLATE_PATTERNS)
                        .expect("using auto builder so no errors possible")
                })
                .find_iter(s),
        );

        if self.ac_buf.is_empty() {
            return Ok(());
        }

        self.row_buf.clear();
        self.col_buf.clear();
        let mut seen_row_col = false;

        // process the matches in reverse order so we don't need to update the match positions as
        // we alter the contents of the buffer.
        for mat in self.ac_buf.iter().rev() {
            let (from, to) = (mat.start(), mat.end());
            let pat = mat.pattern().as_u32() as usize;
            let pattern = TEMPLATE_PATTERNS[pat];

            let new_s = match pattern {
                "\\n" => Cow::Borrowed("\n"),
                "\\t" => Cow::Borrowed("\t"),
                FNAME_VAR => Cow::Borrowed(fname),

                ROW_VAR | COL_VAR => {
                    if !seen_row_col {
                        let (i, _) = m.loc();
                        let row = ed.char_to_line(i).ok_or(Error::InvalidMatchIndices)?;
                        let col = i - ed.line_to_char(row).ok_or(Error::InvalidMatchIndices)?;
                        write!(&mut self.row_buf, "{row}")?;
                        write!(&mut self.col_buf, "{col}")?;
                        seen_row_col = true;
                    }
                    if pattern == ROW_VAR {
                        Cow::Borrowed(self.row_buf.as_str())
                    } else {
                        Cow::Borrowed(self.col_buf.as_str())
                    }
                }

                _ => {
                    debug_assert_eq!(
                        TEMPLATE_PATTERNS[0], "$0",
                        "submatch patterns must be first"
                    );

                    match m.submatch_text(pat, ed) {
                        Some(sm) => sm,
                        None => return Err(Error::InvalidSubstitution(pat)),
                    }
                }
            };

            let char_from = self.template_buf.byte_to_char(from);
            let char_to = self.template_buf.byte_to_char(to);
            self.template_buf.remove_range(char_from, char_to);
            self.template_buf.insert_str(char_from, new_s.as_ref());
        }

        Ok(())
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
