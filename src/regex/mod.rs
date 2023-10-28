//! A simple regex engine for operating on character streams and supporting
//! the Sam text editor's structural regular expressions.
//!
//! Thompson's original paper on writing a regex engine can be found here:
//!   https://dl.acm.org/doi/pdf/10.1145/363347.363387
use crate::util::parse_num;
use std::{iter::Peekable, str::Chars};

mod compile;
mod matches;
mod vm;

pub use matches::{Match, MatchIter};
pub use vm::Regex;

const POSTFIX_BUF_SIZE: usize = 2000;
const POSTFIX_MAX_PARENS: usize = 100;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    EmptyParens,
    EmptyRegex,
    InvalidClass,
    InvalidEscape(char),
    InvalidRepetition,
    ReTooLong,
    TooManyParens,
    UnbalancedAlt,
    UnbalancedParens,
}

// Postfix form notation for building up the compiled state machine
#[derive(Debug, Clone, PartialEq, Eq)]
enum Pfix {
    // Comparisons
    Char(char),
    Class(CharClass),
    Any,
    TrueAny,
    // Repetitions and control flow
    Concat,
    Alt,
    Quest,
    Star,
    Plus,
    Rep(usize),               // {n}
    RepAtLeast(usize),        // {n,}
    RepBetween(usize, usize), // {n,m}
    LazyQuest,
    LazyStar,
    LazyPlus,
    // Assertions
    LineStart,
    LineEnd,
    WordBoundary,
    NonWordBoundary,
    // Sub match save
    Save(usize),
}

impl Pfix {
    fn make_lazy(&mut self) {
        match self {
            Self::Quest => *self = Self::LazyQuest,
            Self::Star => *self = Self::LazyStar,
            Self::Plus => *self = Self::LazyPlus,
            _ => (),
        }
    }
}

/// Helper for converting characters to 0 based inicies for looking things up in caches.
const fn char_ix(ch: char) -> usize {
    ((ch as u16) & 0xFF) as usize
}

const fn init_escapes() -> [Option<char>; 256] {
    macro_rules! escape {
        ($escapes:expr, $($ch:expr),+) => {
            $($escapes[char_ix($ch)] = Some($ch);)+
        };
        ($escapes:expr, $($ch:expr => $esc:expr),+) => {
            $($escapes[char_ix($ch)] = Some($esc);)+
        };
    }

    let mut escapes = [None; 256];
    escape!(escapes, '*', '+', '?', '.', '@', '(', ')', '[', ']', '{', '}', '|');
    escape!(escapes, '\\', '\'', '"', '^', '$');
    escape!(escapes, 'b', 'B', 'd', 'D', 'w', 'W', 's', 'S');
    escape!(escapes, 'n'=>'\n', 'r'=>'\r', 't'=>'\t');

    escapes
}

/// Supported escape sequences
const ESCAPES: [Option<char>; 256] = init_escapes();

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

#[derive(Debug, Clone, PartialEq, Eq)]
struct CharClass {
    negated: bool,
    chars: Vec<char>,
    ranges: Vec<(char, char)>,
}

impl CharClass {
    fn new(negated: bool, chars: Vec<char>, ranges: Vec<(char, char)>) -> Self {
        Self {
            negated,
            chars,
            ranges,
        }
    }

    fn try_parse(it: &mut Peekable<Chars>) -> Result<Self, Error> {
        let mut next = || next_char(it)?.ok_or(Error::InvalidClass);
        let (mut ch, _) = next()?;

        let negated = ch == '^';
        if negated {
            (ch, _) = next()?
        };
        let mut chars = vec![ch];
        let mut ranges = vec![];

        loop {
            let (ch, escaped) = next()?;
            match ch {
                ']' if !escaped => break,

                '-' => {
                    let start = chars.pop().ok_or(Error::InvalidClass)?;
                    let (end, _) = next()?;
                    ranges.push((start, end));
                }

                ch => chars.push(ch),
            }
        }

        Ok(Self {
            negated,
            chars,
            ranges,
        })
    }

    // Negated classes still don't match a newline
    fn matches(&self, ch: char) -> bool {
        if self.negated && ch == '\n' {
            return false;
        }

        let res = self.chars.contains(&ch)
            || self
                .ranges
                .iter()
                .any(|&(start, end)| ch >= start && ch <= end);

        if self.negated {
            !res
        } else {
            res
        }
    }
}

fn try_parse_counted_repetition(it: &mut Peekable<Chars>) -> Result<Pfix, Error> {
    let (mut ch, _) = next_char(it)?.ok_or(Error::InvalidRepetition)?;

    if !ch.is_ascii_digit() {
        return Err(Error::InvalidRepetition);
    }
    let n = parse_num(ch, it);
    if n == 0 {
        return Err(Error::InvalidRepetition);
    }

    (ch, _) = next_char(it)?.ok_or(Error::InvalidRepetition)?;
    if ch == '}' {
        return Ok(Pfix::Rep(n));
    } else if ch != ',' {
        return Err(Error::InvalidRepetition);
    }

    (ch, _) = next_char(it)?.ok_or(Error::InvalidRepetition)?;
    if ch == '}' {
        return Ok(Pfix::RepAtLeast(n));
    }

    if !ch.is_ascii_digit() {
        return Err(Error::InvalidRepetition);
    }
    let m = parse_num(ch, it);
    if m == 0 {
        return Err(Error::InvalidRepetition);
    }

    (ch, _) = next_char(it)?.ok_or(Error::InvalidRepetition)?;
    if ch == '}' {
        Ok(Pfix::RepBetween(n, m))
    } else {
        Err(Error::InvalidRepetition)
    }
}

fn next_char(it: &mut Peekable<Chars>) -> Result<Option<(char, bool)>, Error> {
    match it.next() {
        Some('\\') => (),
        Some(ch) => return Ok(Some((ch, false))),
        None => return Ok(None),
    }

    let ch = match it.next() {
        Some(ch) => ch,
        None => return Err(Error::InvalidEscape('\0')),
    };

    match ESCAPES[char_ix(ch)] {
        Some(ch) => Ok(Some((ch, true))),
        None => Err(Error::InvalidEscape(ch)),
    }
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
    let mut s = 2; // Save(0/1) are for the whole match

    let mut it = re.chars().peekable();

    while let Some((ch, escaped)) = next_char(&mut it)? {
        if escaped {
            match ch {
                'b' => push_atom(Pfix::WordBoundary, &mut natom, &mut output),
                'B' => push_atom(Pfix::NonWordBoundary, &mut natom, &mut output),
                // digits
                'd' => push_atom(
                    Pfix::Class(CharClass::new(false, vec![], vec![('0', '9')])),
                    &mut natom,
                    &mut output,
                ),
                'D' => push_atom(
                    Pfix::Class(CharClass::new(true, vec![], vec![('0', '9')])),
                    &mut natom,
                    &mut output,
                ),
                // alphanumeric
                'w' => push_atom(
                    Pfix::Class(CharClass::new(
                        false,
                        vec!['_'],
                        vec![('a', 'z'), ('A', 'Z'), ('0', '9')],
                    )),
                    &mut natom,
                    &mut output,
                ),
                'W' => push_atom(
                    Pfix::Class(CharClass::new(
                        true,
                        vec!['_'],
                        vec![('a', 'z'), ('A', 'Z'), ('0', '9')],
                    )),
                    &mut natom,
                    &mut output,
                ),
                // whitespace (this is not the full utf8 whitespace character semantics used by
                // other engines currently)
                's' => push_atom(
                    Pfix::Class(CharClass::new(false, vec![' ', '\t', '\n', '\r'], vec![])),
                    &mut natom,
                    &mut output,
                ),
                'S' => push_atom(
                    Pfix::Class(CharClass::new(true, vec![' ', '\t', '\n', '\r'], vec![])),
                    &mut natom,
                    &mut output,
                ),

                ch => push_atom(Pfix::Char(ch), &mut natom, &mut output),
            }
            continue;
        }

        match ch {
            '(' => {
                if p >= POSTFIX_MAX_PARENS {
                    return Err(Error::TooManyParens);
                }
                push_atom(Pfix::Save(s), &mut natom, &mut output);
                s += 1;
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
                push_atom(Pfix::Save(s), &mut natom, &mut output);
                s += 1;
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

            '?' => {
                if matches!(output.last(), Some(Pfix::Star | Pfix::Plus | Pfix::Quest)) {
                    output.last_mut().unwrap().make_lazy();
                } else {
                    push_rep(Pfix::Quest, natom, &mut output)?;
                }
            }

            '{' => {
                let rep = try_parse_counted_repetition(&mut it)?;
                push_rep(rep, natom, &mut output)?;
            }

            '[' => {
                let cls = CharClass::try_parse(&mut it)?;
                push_atom(Pfix::Class(cls), &mut natom, &mut output);
            }

            '.' => push_atom(Pfix::Any, &mut natom, &mut output),
            '@' => push_atom(Pfix::TrueAny, &mut natom, &mut output),
            '^' => push_atom(Pfix::LineStart, &mut natom, &mut output),
            '$' => push_atom(Pfix::LineEnd, &mut natom, &mut output),

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

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;
    use Pfix::*;

    #[test_case(
        "a(bb)+a",
        &[Char('a'), Save(2), Concat, Char('b'), Char('b'), Concat, Save(3), Plus, Concat, Char('a'), Concat ];
        "article example"
    )]
    #[test_case(
        "a(bc|cd)+a",
        &[Char('a'), Save(2), Concat, Char('b'), Char('c'), Concat, Char('c'), Char('d'), Concat, Alt, Save(3), Plus, Concat, Char('a'), Concat];
        "repeated alt"
    )]
    #[test_case("(a*)*", &[Save(2), Char('a'), Star, Save(3), Star, Concat]; "star star")]
    #[test_case("foo$", &[Char('f'), Char('o'), Concat, Char('o'), Concat, LineEnd, Concat]; "line end")]
    #[test]
    fn postfix_construction_works(re: &str, expected: &[Pfix]) {
        assert_eq!(&re_to_postfix(re).unwrap(), expected);
    }
}
