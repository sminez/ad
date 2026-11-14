//! Simple regex based highlighting by line.
//!
//! This should only be used as a fallback for simple highlighting of file formats that do not have
//! a tree-sitter grammar available. It will almost certainly produce incorrect results for complex
//! or overlapping regex sets but its better than nothing if all you need is some quick and simple
//! rules for highlighting regions of a known file format.
use crate::{
    buffer::{ByteIdx, GapBuffer, LineIdx},
    dot::Range,
    regex::Regex,
    syntax::{ByteRange, LineIter, SyntaxRange},
};

#[derive(Debug)]
pub struct ReState {
    re: Regex,
    names: Vec<String>,
    ranges: Vec<SyntaxRange>,
}

impl ReState {
    pub fn new(tagged_re: &[(impl AsRef<str>, impl AsRef<str>)]) -> Result<Self, String> {
        let mut names = Vec::with_capacity(tagged_re.len());
        let mut patterns = Vec::with_capacity(tagged_re.len());

        for (name, pattern) in tagged_re.iter() {
            names.push(name.as_ref().to_string());
            patterns.push(pattern.as_ref());
        }

        let re = Regex::compile_many(&patterns)
            .map_err(|err| format!("invalid regex ({patterns:?}): {err:?}"))?;

        Ok(Self {
            re,
            names,
            ranges: Vec::new(),
        })
    }

    pub fn update(&mut self, gb: &GapBuffer, from: LineIdx, n_rows: usize) {
        self.ranges.clear();

        for i in from.0..from.0 + n_rows {
            let line = gb.line(LineIdx(i));
            let line_start = gb.line_to_byte(LineIdx(i));
            let mut offset = ByteIdx(0);

            while let Some(m) = self.re.find_from(&line, offset) {
                self.ranges.push(SyntaxRange {
                    cap_idx: Some(m.pattern_id),
                    r: ByteRange {
                        from: m.from + line_start,
                        to: m.to + line_start,
                    },
                });
                offset = m.to;
            }
        }
    }

    #[inline]
    pub fn iter_tokenized_lines_from<'a>(
        &'a self,
        line: LineIdx,
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
        state.update(&gb, LineIdx(0), 6);

        let mut toks = Vec::new();
        for line in state.iter_tokenized_lines_from(LineIdx(0), &gb, Range::BOF, None) {
            for tok in line {
                let s = tok.as_slice(&gb).to_string();
                toks.push((tok.tag.to_string(), s));
            }
        }

        assert_eq!(toks, expected);
    }

    #[test]
    fn tokenizing_handles_multibyte_chars() {
        // "data" is at chars 4..8 but bytes 6..10
        let gb = GapBuffer::from("été data");
        let mut state = ReState::new(&[("keyword", r"\bdata\b")]).expect("valid regex patterns");
        state.update(&gb, LineIdx(0), 1);

        let ranges: Vec<_> = state.ranges.iter().map(|sr| (sr.r.from, sr.r.to)).collect();
        assert_eq!(ranges, vec![(ByteIdx(6), ByteIdx(10))]);
    }
}
