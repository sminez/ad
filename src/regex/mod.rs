//! A simple regex engine for operating on character streams and supporting
//! the Sam text editor's structural regular expressions.
//!
//! Thompson's original paper on writing a regex engine can be found here:
//!   https://dl.acm.org/doi/pdf/10.1145/363347.363387

// Different impls of the matching algorithm
pub mod vm;

pub use vm::{Match, Regex};

const POSTFIX_BUF_SIZE: usize = 2000;
const POSTFIX_MAX_PARENS: usize = 100;

#[derive(Debug, PartialEq, Eq)]
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
    Char(char),
    Class(CharClass),
    Any,
    TrueAny,
    Concat,
    Alt,
    Quest,
    Star,
    Plus,
    LazyQuest,
    LazyStar,
    LazyPlus,
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
    escape!(escapes, '*', '+', '?', '.', '@', '(', ')', '[', ']', '|');
    escape!(escapes, '\\', '\'', '"');
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
    fn try_parse(it: &mut std::str::Chars) -> Result<Self, Error> {
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

fn next_char(it: &mut std::str::Chars) -> Result<Option<(char, bool)>, Error> {
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

    let mut it = re.chars();

    while let Some((ch, escaped)) = next_char(&mut it)? {
        if escaped {
            push_atom(Pfix::Char(ch), &mut natom, &mut output);
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

            '[' => {
                let cls = CharClass::try_parse(&mut it)?;
                push_atom(Pfix::Class(cls), &mut natom, &mut output);
            }

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
    #[test_case(
        "(a*)*",
        &[Save(2), Char('a'), Star, Save(3), Star, Concat];
        "star star"
    )]
    #[test]
    fn postfix_construction_works(re: &str, expected: &[Pfix]) {
        assert_eq!(&re_to_postfix(re).unwrap(), expected);
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
        assert_eq!(r.match_str(s).is_some(), matches);
    }

    #[test]
    fn match_extraction_works() {
        let re = "([0-9]+)-([0-9]+)";
        let mut r = Regex::compile(re).unwrap();
        let s = "this should work 123-456 other stuff";
        let m = r.match_str(s).unwrap();

        assert_eq!(m.str_submatch_text(1, s), Some("123"));
        assert_eq!(m.str_submatch_text(2, s), Some("456"));
        assert_eq!(m.str_match_text(s), "123-456");
    }

    // This is the pathological case that Cox covers in his article which leads
    // to exponential behaviour in backtracking based implementations.
    #[test]
    fn pathological_match_doesnt_explode() {
        let s = "a".repeat(100);
        let mut re = "a?".repeat(100);
        re.push_str(&s);

        let mut r = Regex::compile(&re).unwrap();
        assert!(r.match_str(&s).is_some());
    }

    // Make sure that the previous cached state for a given Regex doesn't cause
    // any strange behaviour for future matches
    #[test]
    fn repeated_match_works() {
        let re = "a(bb)+a";

        let mut r = Regex::compile(re).unwrap();
        for _ in 0..10 {
            assert!(r.match_str("abbbba").is_some());
            assert!(r.match_str("foo").is_none());
        }
    }
}
