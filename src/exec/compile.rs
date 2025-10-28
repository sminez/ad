//! Compiling of [Ast] nodes into a complete [Prog];
use crate::{
    exec::{
        Addr,
        parse::{self, Ast, Parser, Sequence, SetAddr, Template},
    },
    regex::Regex,
};
use std::cell::RefCell;

/// An exec [Prog] uses structural regular expressions to identify a set of edit points within a
/// buffer which are then executed in parallel.
#[derive(Debug)]
pub struct Prog {
    instructions: Vec<Inst>,
    addrs: Vec<Addr>,
    re: RefCell<Vec<Regex>>,
    templates: Vec<String>,
}

impl Prog {
    pub fn compile(s: &str) -> Result<Self, String> {
        Compiler::default().compile(s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Inst {
    /// Set the current dot to addr
    SetAddr(usize),
    /// Run all instructions in order over the current dot
    Series(Vec<Inst>),
    /// Run all branches in parallel over the current dot
    Parallel(Vec<Inst>),
    /// For each match of the regex in dot, det dot and run per_match. Between each match, run
    /// between_matches
    Extract(Extract),
    /// If re matches, run if_matching for the current dot otherwise run if_not_matching
    Guard(Guard),
    /// An action to store against the current match
    Action(Action),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ActionKind {
    Insert,
    Append,
    Change,
    Print,
    Delete,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Action {
    kind: ActionKind,
    template: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Guard {
    re: usize,
    if_matching: Vec<Inst>,
    if_not_matching: Vec<Inst>,
}

impl Guard {
    /// Try to merge this Guard into an existing branch if possible.
    ///
    /// Returns None if merging was successful or Some(self) if not.
    fn try_merge(self, branches: &mut [Inst]) -> Option<Self> {
        for branch in branches.iter_mut() {
            match branch {
                Inst::Guard(g) if self.re == g.re => match (
                    g.if_matching.is_empty(),
                    g.if_not_matching.is_empty(),
                    self.if_matching.is_empty(),
                    self.if_not_matching.is_empty(),
                ) {
                    (true, false, false, true) => {
                        g.if_matching = self.if_matching;
                        return None;
                    }
                    (false, true, true, false) => {
                        g.if_not_matching = self.if_not_matching;
                        return None;
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        Some(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Extract {
    re: usize,
    per_match: Vec<Inst>,
    between_matches: Vec<Inst>,
}

impl Extract {
    /// Try to merge this Extract into an existing branch if possible.
    ///
    /// Returns None if merging was successful or Some(self) if not.
    fn try_merge(self, branches: &mut [Inst]) -> Option<Self> {
        for branch in branches.iter_mut() {
            match branch {
                Inst::Extract(e) if self.re == e.re => match (
                    e.per_match.is_empty(),
                    e.between_matches.is_empty(),
                    self.per_match.is_empty(),
                    self.between_matches.is_empty(),
                ) {
                    (true, false, false, true) => {
                        e.per_match = self.per_match;
                        return None;
                    }
                    (false, true, true, false) => {
                        e.between_matches = self.between_matches;
                        return None;
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        Some(self)
    }
}

#[derive(Debug, Default)]
struct Compiler {
    addrs: Vec<Addr>,
    re: Vec<String>,
    templates: Vec<String>,
}

impl Compiler {
    pub fn compile(mut self, s: &str) -> Result<Prog, String> {
        let ast = Parser::new(s).parse().map_err(|e| e.to_string())?;
        let mut instructions = Vec::new();
        self.add_instructions_for(ast, &mut instructions);

        let re = self
            .re
            .into_iter()
            .map(|re| Regex::compile(re).map_err(|e| e.to_string()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Prog {
            instructions,
            addrs: self.addrs,
            re: RefCell::new(re),
            templates: self.templates,
        })
    }

    fn add_instructions_for(&mut self, ast: Ast, instructions: &mut Vec<Inst>) {
        match ast {
            Ast::SetAddr(SetAddr { addr, .. }) => {
                instructions.push(Inst::SetAddr(self.push_addr(*addr)));
            }

            Ast::Extract(ext) => self.add_for_extract(ext, false, instructions),
            Ast::ExtractBetween(ext) => self.add_for_extract(ext, true, instructions),

            Ast::Series(Sequence { nodes, .. }) => {
                for node in nodes {
                    self.add_instructions_for(node, instructions);
                }
            }

            Ast::Parallel(Sequence { nodes, .. }) => self.add_for_group(nodes, instructions),

            Ast::Guard(g) => self.add_for_guard(g, false, instructions),
            Ast::InvGuard(g) => self.add_for_guard(g, true, instructions),

            Ast::Insert(t) => self.add_for_template(ActionKind::Insert, t, instructions),
            Ast::Append(t) => self.add_for_template(ActionKind::Append, t, instructions),
            Ast::Change(t) => self.add_for_template(ActionKind::Change, t, instructions),
            Ast::Print(t) => self.add_for_template(ActionKind::Print, t, instructions),

            Ast::Delete(_) => instructions.push(Inst::Action(Action {
                kind: ActionKind::Delete,
                template: 0, // ignored
            })),

            Ast::Comment(_) => (),
        }
    }

    fn push_addr(&mut self, addr: Addr) -> usize {
        match self.addrs.iter().position(|a| a == &addr) {
            Some(idx) => idx,
            None => {
                self.addrs.push(addr);
                self.addrs.len() - 1
            }
        }
    }

    fn push_re(&mut self, re: String) -> usize {
        match self.re.iter().position(|s| s == &re) {
            Some(idx) => idx,
            None => {
                self.re.push(re);
                self.re.len() - 1
            }
        }
    }

    fn push_template(&mut self, t: String) -> usize {
        match self.templates.iter().position(|s| s == &t) {
            Some(idx) => idx,
            None => {
                self.templates.push(t);
                self.templates.len() - 1
            }
        }
    }

    fn add_for_extract(
        &mut self,
        ext: parse::Extract,
        between: bool,
        instructions: &mut Vec<Inst>,
    ) {
        let re = self.push_re(ext.re);
        let mut branch = Vec::new();
        for node in ext.nodes {
            self.add_instructions_for(node, &mut branch);
        }

        let (per_match, between_matches) = if between {
            (Vec::new(), branch)
        } else {
            (branch, Vec::new())
        };

        instructions.push(Inst::Extract(Extract {
            re,
            per_match,
            between_matches,
        }));
    }

    fn add_for_group(&mut self, nodes: Vec<Ast>, instructions: &mut Vec<Inst>) {
        let mut branches = Vec::new();

        for node in nodes {
            let mut branch = Vec::new();
            self.add_instructions_for(node, &mut branch);

            if branch.len() > 1 {
                branches.push(Inst::Series(branch));
                continue;
            }

            match branch.remove(0) {
                Inst::Guard(g) => {
                    if let Some(g) = g.try_merge(&mut branches) {
                        branches.push(Inst::Guard(g));
                    }
                }

                Inst::Extract(e) => {
                    if let Some(e) = e.try_merge(&mut branches) {
                        branches.push(Inst::Extract(e));
                    }
                }

                inst => branches.push(inst),
            }
        }

        if branches.len() == 1 {
            instructions.push(branches.remove(0));
        } else {
            instructions.push(Inst::Parallel(branches));
        }
    }

    fn add_for_guard(&mut self, g: parse::Guard, inverted: bool, instructions: &mut Vec<Inst>) {
        let re = self.push_re(g.re);
        let mut branch = Vec::new();
        for node in g.nodes {
            self.add_instructions_for(node, &mut branch);
        }

        let (if_matching, if_not_matching) = if inverted {
            (Vec::new(), branch)
        } else {
            (branch, Vec::new())
        };

        instructions.push(Inst::Guard(Guard {
            re,
            if_matching,
            if_not_matching,
        }));
    }

    #[inline(always)]
    fn add_for_template(&mut self, kind: ActionKind, t: Template, instructions: &mut Vec<Inst>) {
        let template = self.push_template(t.s);
        instructions.push(Inst::Action(Action { kind, template }));
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
    fn compile_works() {
        let prog = Prog::compile(PROG).unwrap();
    }
}
