//! Simple regex based highlighting by line.
//!
//! This should only be used as a fallback for simple highlighting of file formats that do not have
//! a tree-sitter grammar available. It will almost certainly produce incorrect results for complex
//! or overlapping regex sets but its better than nothing if all you need is some quick and simple
//! rules for highlighting regions of a known file format.
use crate::{
    buffer::GapBuffer,
    dot::Range,
    regex::{Match, Regex},
    syntax::{ByteRange, LineIter, SyntaxRange},
};

#[derive(Debug)]
pub struct ReState {
    mr: MultiRegex,
    names: Vec<String>,
    ranges: Vec<SyntaxRange>,
}

impl ReState {
    pub fn new(tagged_re: &[(impl AsRef<str>, impl AsRef<str>)]) -> Result<Self, String> {
        let mut names = Vec::with_capacity(tagged_re.len());
        let mut re = Vec::with_capacity(tagged_re.len());

        for (name, re_str) in tagged_re.iter() {
            names.push(name.as_ref().to_string());
            re.push(
                Regex::compile(re_str.as_ref())
                    .map_err(|err| format!("invalid regex ({:?}): {err:?}", re_str.as_ref()))?,
            );
        }

        Ok(Self {
            mr: MultiRegex { re },
            names,
            ranges: Vec::new(),
        })
    }

    pub fn update(&mut self, gb: &GapBuffer, from: usize, n_rows: usize) {
        self.ranges.clear();

        let pos = gb.line_to_char(from);
        let ch_to = if from + n_rows + 1 < gb.len_lines() {
            gb.line_to_char(from + n_rows + 1)
        } else {
            gb.len_chars()
        };

        self.ranges.extend(Tokenizer {
            mr: &mut self.mr,
            gb,
            pos,
            ch_to,
        });
    }

    #[inline]
    pub fn iter_tokenized_lines_from<'a>(
        &'a self,
        line: usize,
        gb: &'a GapBuffer,
        dot_range: Range,
        load_exec_range: Option<(bool, Range)>,
    ) -> LineIter<'a> {
        LineIter::new(
            line,
            gb,
            dot_range,
            load_exec_range,
            &self.names,
            &self.ranges,
        )
    }
}

#[derive(Debug)]
struct Tokenizer<'a> {
    mr: &'a mut MultiRegex,
    gb: &'a GapBuffer,
    pos: usize,
    ch_to: usize,
}

impl Iterator for Tokenizer<'_> {
    type Item = SyntaxRange;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.ch_to {
            return None;
        }

        let (i, m) = self.mr.match_gb_from(self.gb, self.pos)?;
        let (mfrom, mto) = m.loc();
        self.pos = mto;

        let from = self.gb.char_to_byte(mfrom);
        let to = self.gb.offset_char_to_byte(mto, from, mfrom);

        Some(SyntaxRange {
            cap_idx: Some(i),
            r: ByteRange { from, to },
        })
    }
}

/// An set of [Regex] that will report the first match encountered out of any of the inner
/// patterns.
#[derive(Debug, Clone)]
struct MultiRegex {
    re: Vec<Regex>,
}

impl MultiRegex {
    // FIXME: for now this is just a dumb way to run multiple regex patterns over the same input
    // really this should live in the regex module and it should run the VMs in parallel, returning
    // the first match rather than in series like this

    pub fn match_gb_from(&mut self, gb: &GapBuffer, ch_from: usize) -> Option<(usize, Match)> {
        let mut leftmost: Option<(usize, Match)> = None;

        for (i, re) in self.re.iter_mut().enumerate() {
            let m = re.find_from(gb, ch_from);
            match (leftmost.as_ref(), m) {
                (Some((_, prev)), Some(m)) if &m < prev => leftmost = Some((i, m)),
                (None, Some(m)) => leftmost = Some((i, m)),
                _ => (),
            }
        }

        leftmost
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const PLUMB_FILE: &str = "
# this is a comment
data matches foo
data from running some command
attr add action=showdata filename=/+synthetic
plumb to edit";

    #[test]
    fn tokenizing_works() {
        let gb = GapBuffer::from(PLUMB_FILE);
        let tagged_re = vec![
            ("comment", "#.*"),
            ("keyword", r"\b(data|attr|plumb|arg)\b"),
            (
                "function",
                r"\b(matches|narrows|from|add|to|start|isfile|isdir)\b",
            ),
        ];
        let expected: Vec<(String, String)> = [
            ("dot", ""),
            ("comment", "# this is a comment"),
            ("keyword", "data"),
            ("default", " "),
            ("function", "matches"),
            ("default", " foo"),
            ("keyword", "data"),
            ("default", " "),
            ("function", "from"),
            ("default", " running some command"),
            ("keyword", "attr"),
            ("default", " "),
            ("function", "add"),
            ("default", " action=showdata filename=/+synthetic"),
            ("keyword", "plumb"),
            ("default", " "),
            ("function", "to"),
            ("default", " edit"),
        ]
        .iter()
        .map(|(tag, s)| (tag.to_string(), s.to_string()))
        .collect();

        let mut state = ReState::new(&tagged_re).expect("valid regex patterns");
        state.update(&gb, 0, 6);

        let mut toks = Vec::new();
        for line in state.iter_tokenized_lines_from(0, &gb, Range::BOF, None) {
            for tok in line {
                let s = tok.as_slice(&gb).to_string();
                toks.push((tok.tag.to_string(), s));
            }
        }

        assert_eq!(toks, expected);
    }
}
