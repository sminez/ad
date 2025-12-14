//! A simple AST for parsing and manipulating regex strings
use super::{CharClass, Error, next_char};
use std::{collections::HashSet, iter::Peekable, mem::swap, str::Chars};

/// Complex regex patterns can cause the generation of leading literal patterns to take
/// exponentially longer to compute. We impose a hard limit on the number of patterns we collect in
/// order to keep this under control at the expense of not being able to always find leading
/// literals for complex patterns.
/// When combining leading literals we check using this constant to see if we are exceeding this
/// limit and cancel the calculation (returning no leading literals at all) if the correct set
/// would need to be truncated. This prevents us running an invalid fast path optimisation that can
/// end up advancing us too far through the input.
const MAX_LEADING_LITERALS: usize = 50;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Ast {
    Comp(Comp),
    Assertion(Assertion),
    Alt(Vec<Ast>),
    Rep(Rep, Box<Ast>),
    Concat(Vec<Ast>),
    SubMatch(SmKind, Box<Ast>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum SmKind {
    Normal,
    Named(String),
    NonCapturing,
}

impl Ast {
    /// Extract leading literal string fragments from this [Ast].
    ///
    /// This is used for determining if we can make use of a fast literal search to determine the
    /// start of a potential match before dropping into running the regex engine. Doing so can
    /// result in a significant speedup.
    pub fn leading_literals(&self) -> HashSet<String> {
        let (mut lits, _) = self.leading_literals_inner();
        lits.remove("");
        lits
    }

    fn leading_literals_inner(&self) -> (HashSet<String>, bool) {
        match self {
            // Actual literals that we need to collect
            Self::Comp(Comp::Char(c)) => (std::iter::once(c.to_string()).collect(), true),
            Self::Comp(Comp::Numeric) => (('0'..='9').map(String::from).collect(), true),

            Self::Comp(Comp::Class(cls)) if !cls.negated && cls.size() <= MAX_LEADING_LITERALS => (
                cls.chars
                    .iter()
                    .map(|ch| ch.to_string())
                    .chain(
                        cls.ranges
                            .iter()
                            .flat_map(|(start, end)| (*start..=*end).map(String::from)),
                    )
                    .collect(),
                true,
            ),

            // Other comparisons are too broad for us to lift out into leading literals and
            // assertions are not literals.
            Self::Comp(_) | Self::Assertion(_) => (HashSet::new(), false),

            Self::SubMatch(_, node) => node.leading_literals_inner(),

            // Nested structure we need to combine
            Self::Concat(nodes) => leading_literals_for_concat(nodes.iter()),

            Self::Alt(nodes) => {
                let mut lits = HashSet::new();
                for node in nodes {
                    lits.extend(node.leading_literals());
                    if lits.len() > MAX_LEADING_LITERALS {
                        lits.clear();
                        return (lits, false);
                    }
                }

                (lits, true)
            }

            Self::Rep(r, node) => {
                let mut lits = node.leading_literals();
                if r.is_nullable() && !lits.is_empty() {
                    lits.insert(String::new());
                }

                (lits, false)
            }
        }
    }

    fn concat_or_node(mut nodes: Vec<Ast>) -> Ast {
        match nodes.len() {
            1 => nodes.remove(0),
            _ => Ast::Concat(nodes),
        }
    }

    pub fn op_len(&self) -> usize {
        match self {
            Ast::Comp(_) | Ast::Assertion(_) => 1,
            Ast::Alt(nodes) => {
                nodes.iter().map(|n| n.op_len()).sum::<usize>() + (nodes.len() - 1) * 2
            }
            Ast::Rep(r, node) => r.op_len(node.op_len()),
            Ast::Concat(nodes) => nodes.iter().map(|n| n.op_len()).sum(),
            Ast::SubMatch(SmKind::NonCapturing, node) => node.op_len(), // save start and end
            Ast::SubMatch(_, node) => node.op_len() + 2,                // save start and end
        }
    }

    pub fn reverse(&mut self) {
        match self {
            Self::Assertion(Assertion::LineStart) => *self = Self::Assertion(Assertion::LineEnd),
            Self::Assertion(Assertion::LineEnd) => *self = Self::Assertion(Assertion::LineStart),

            Self::Alt(nodes) => {
                nodes.reverse();
                nodes.iter_mut().for_each(|n| n.reverse());
            }
            Self::Rep(_, node) => node.reverse(),
            Self::Concat(nodes) => {
                nodes.reverse();
                nodes.iter_mut().for_each(|n| n.reverse());
            }
            Self::SubMatch(_, node) => node.reverse(),
            _ => (),
        }
    }

    pub fn optimise(&mut self) {
        let mut optimising = true;

        while optimising {
            optimising = match self {
                Ast::Comp(cmp) => cmp.replace_common_classes(),
                Ast::Concat(nodes) => compress_cats(nodes),
                // TODO: extract common alt prefixes & dedup cases
                _ => false,
            };
        }
    }

    fn contains_named_submatch(&self) -> bool {
        match self {
            Ast::SubMatch(SmKind::Named(_), _) => true,
            Ast::SubMatch(_, node) => node.contains_named_submatch(),
            Ast::Rep(_, node) => node.contains_named_submatch(),
            Ast::Alt(nodes) => nodes.iter().any(|n| n.contains_named_submatch()),
            Ast::Concat(nodes) => nodes.iter().any(|n| n.contains_named_submatch()),
            _ => false,
        }
    }

    fn named_submatch_only(&mut self) {
        let apply = |nodes: &mut Vec<Ast>| {
            for node in nodes.iter_mut() {
                node.named_submatch_only()
            }
        };

        match self {
            Ast::SubMatch(SmKind::Named(_) | SmKind::NonCapturing, node) => {
                node.named_submatch_only()
            }

            Ast::SubMatch(k @ SmKind::Normal, node) => {
                node.named_submatch_only();
                *k = SmKind::NonCapturing;
            }

            Ast::Alt(nodes) => apply(nodes),
            Ast::Concat(nodes) => apply(nodes),
            Ast::Rep(_, node) => node.named_submatch_only(),
            _ => (),
        }
    }
}

fn leading_literals_for_concat(mut it: std::slice::Iter<'_, Ast>) -> (HashSet<String>, bool) {
    let mut lits = HashSet::new();
    lits.insert(String::new());
    let mut ongoing = true;

    while let Some(node) = it.next()
        && ongoing
    {
        // We stop at assertions and repetitions alter how we proceed
        let rep = match node {
            Ast::Assertion(_) => {
                ongoing = false;
                break;
            }
            Ast::Rep(r, _) => Some(r),
            _ => None,
        };

        let node_lits;
        (node_lits, ongoing) = node.leading_literals_inner();

        match rep {
            // + and * both act as a break condition as we have an unbounded number of characters
            // to consume. In the case of * we need to account for the case where we didn't match
            // and include all of the literals that come afterwards directly.
            Some(Rep::Plus(_)) => {
                lits = combine(&lits, &node_lits);
                if lits.len() > MAX_LEADING_LITERALS {
                    lits.clear();
                    return (lits, false);
                }

                break;
            }

            Some(Rep::Star(_)) => {
                if !node_lits.is_empty() {
                    let remaining = it.clone();
                    let mut star_lits = combine(&lits, &node_lits);
                    let (without_star_lits, _) = leading_literals_for_concat(remaining);
                    star_lits.extend(combine(&lits, &without_star_lits));
                    lits = star_lits;
                }
                if lits.len() > MAX_LEADING_LITERALS {
                    lits.clear();
                    return (lits, false);
                }

                lits.remove("");
                break;
            }

            _ => {
                lits = combine(&lits, &node_lits);
                if lits.len() > MAX_LEADING_LITERALS {
                    lits.clear();
                    return (lits, false);
                }
            }
        };
    }

    (lits, ongoing)
}

fn combine(lits: &HashSet<String>, node_lits: &HashSet<String>) -> HashSet<String> {
    lits.iter()
        .flat_map(|l| {
            node_lits.iter().map(|nl| {
                let mut s = l.clone();
                s.push_str(nl);
                s
            })
        })
        .collect()
}

fn compress_cats(nodes: &mut Vec<Ast>) -> bool {
    let mut buf = Vec::new();
    swap(&mut buf, nodes);

    let mut buf2 = Vec::with_capacity(buf.len());
    let mut compressing = true;
    let mut compressed = false;

    while compressing {
        compressing = false;
        for node in buf.drain(..) {
            if let Ast::Concat(children) = node {
                buf2.extend(children);
                compressing = true;
            } else {
                buf2.push(node);
            }
        }
        swap(&mut buf, &mut buf2);
        compressed |= compressing;
    }

    swap(&mut buf, nodes);

    compressed
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Comp {
    Char(char),
    Class(CharClass),
    Any,
    TrueAny,
    // Common classes
    Numeric,
    NonNumeric,
    AlphaNumeric,
    NonAlphaNumeric,
    WhiteSpace,
    NonWhiteSpace,
}

impl Comp {
    /// Whether or not this Comp matches the current VM state.
    #[inline]
    pub(super) fn matches(&self, ch: char) -> bool {
        match self {
            Comp::Char(c) => *c == ch,
            Comp::Class(cls) => cls.matches(ch),
            Comp::Any => ch != '\n',
            Comp::TrueAny => true,
            Comp::Numeric => ch.is_ascii_digit(),
            Comp::NonNumeric => !ch.is_ascii_digit(),
            Comp::AlphaNumeric => ch.is_ascii_alphanumeric() || ch == '_',
            Comp::NonAlphaNumeric => !(ch.is_ascii_alphanumeric() || ch == '_'),
            Comp::WhiteSpace => ch.is_whitespace(),
            Comp::NonWhiteSpace => !ch.is_whitespace(),
        }
    }

    fn replace_common_classes(&mut self) -> bool {
        match self {
            Comp::Class(CharClass {
                negated: false,
                chars,
                ranges,
            }) if chars.is_empty() && ranges == &[('0', '9')] => {
                *self = Comp::Numeric;
                true
            }

            Comp::Class(CharClass {
                negated: true,
                chars,
                ranges,
            }) if chars.is_empty() && ranges == &[('0', '9')] => {
                *self = Comp::NonNumeric;
                true
            }

            Comp::Class(CharClass {
                negated: false,
                chars,
                ranges,
            }) if chars == &['_']
                && ranges.contains(&('0', '9'))
                && ranges.contains(&('a', 'z'))
                && ranges.contains(&('A', 'Z')) =>
            {
                *self = Comp::AlphaNumeric;
                true
            }

            Comp::Class(CharClass {
                negated: true,
                chars,
                ranges,
            }) if chars == &['_']
                && ranges.contains(&('0', '9'))
                && ranges.contains(&('a', 'z'))
                && ranges.contains(&('A', 'Z')) =>
            {
                *self = Comp::NonAlphaNumeric;
                true
            }

            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Assertion {
    LineStart,
    LineEnd,
    WordBoundary,
    NonWordBoundary,
}

impl Assertion {
    #[inline]
    pub(super) fn holds_for(&self, prev: Option<char>, ch: char, next: Option<char>) -> bool {
        match self {
            Assertion::LineStart => matches!(prev, Some('\n') | None),
            Assertion::LineEnd => matches!(next, Some('\n') | None),

            Assertion::WordBoundary => match (prev, next) {
                (_, None) | (None, _) => true,
                (Some(p), Some(n)) => is_word_boundary(p, ch) || is_word_boundary(ch, n),
            },

            Assertion::NonWordBoundary => match (prev, next) {
                (_, None) | (None, _) => false,
                (Some(p), Some(n)) => !is_word_boundary(p, ch) && !is_word_boundary(ch, n),
            },
        }
    }
}

fn is_word_boundary(prev: char, ch: char) -> bool {
    use CharKind::*;

    matches!(
        (CharKind::from(prev), CharKind::from(ch)),
        (Word, Punctuation)
            | (Punctuation, Word)
            | (Word | Punctuation, Whitespace)
            | (Whitespace, Word | Punctuation)
    )
}

#[derive(Clone, Copy)]
enum CharKind {
    Word,
    Punctuation,
    Whitespace,
}

impl From<char> for CharKind {
    fn from(ch: char) -> Self {
        if ch.is_alphanumeric() || ch == '_' {
            CharKind::Word
        } else if ch.is_whitespace() {
            CharKind::Whitespace
        } else {
            CharKind::Punctuation
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Greed {
    Greedy,
    Lazy,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Rep {
    Quest(Greed),
    Star(Greed),
    Plus(Greed),
}

impl Rep {
    fn is_nullable(&self) -> bool {
        matches!(self, Self::Quest(_) | Self::Star(_))
    }

    fn make_lazy(&mut self) {
        match self {
            Rep::Quest(g) => *g = Greed::Lazy,
            Rep::Star(g) => *g = Greed::Lazy,
            Rep::Plus(g) => *g = Greed::Lazy,
        }
    }

    fn apply(self, nodes: &mut Vec<Ast>) -> Result<(), Error> {
        let last = nodes.pop().ok_or(Error::InvalidRepetition)?;

        match (self, last) {
            (Rep::Quest(_), Ast::Rep(mut r, node)) => {
                r.make_lazy();
                nodes.push(Ast::Rep(r, node));
            }
            (r, last) => nodes.push(Ast::Rep(r, Box::new(last))),
        }

        Ok(())
    }

    fn op_len(&self, child_len: usize) -> usize {
        match self {
            // sp(n+1, n+2), op
            Rep::Quest(_) => child_len + 1,
            // sp(n+1, n+3), op, jmp(m-2)
            Rep::Star(_) => child_len + 2,
            // op, sp(n-1, n+1)
            Rep::Plus(_) => child_len + 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Counted {
    Rep(usize),               // {n}
    RepAtLeast(usize),        // {n,}
    RepBetween(usize, usize), // {n,m}
}

impl Counted {
    fn apply(self, nodes: &mut Vec<Ast>) -> Result<(), Error> {
        let last = nodes.pop().ok_or(Error::InvalidRepetition)?;

        match self {
            Counted::Rep(n) => nodes.extend(vec![last; n]),
            Counted::RepAtLeast(n) => {
                nodes.extend(vec![last.clone(); n - 1]);
                nodes.push(Ast::Rep(Rep::Plus(Greed::Greedy), Box::new(last)));
            }
            Counted::RepBetween(n, m) => {
                nodes.extend(vec![last.clone(); n]);
                nodes.extend(vec![
                    Ast::Rep(Rep::Quest(Greed::Greedy), Box::new(last));
                    m - n
                ]);
            }
        }

        Ok(())
    }
}

pub(super) fn parse(re: &str) -> Result<Ast, Error> {
    let mut nodes = Vec::new();
    let mut it = re.chars().peekable();

    parse_many(&mut it, &mut nodes)?;

    let mut root = match nodes.len() {
        0 => return Err(Error::EmptyRegex),
        _ => Ast::concat_or_node(nodes),
    };

    if root.contains_named_submatch() {
        root.named_submatch_only()
    }

    Ok(root)
}

enum ParseEnd {
    Rparen,
    Eof,
}

fn parse1(it: &mut Peekable<Chars<'_>>, root: &mut Vec<Ast>) -> Result<Option<ParseEnd>, Error> {
    match next_char(it)? {
        Some((ch, true)) => handle_escaped(ch, root).map(|_| None),
        Some((ch, false)) => handle_char(ch, it, root),
        None => Ok(Some(ParseEnd::Eof)),
    }
}

fn parse_many(it: &mut Peekable<Chars<'_>>, root: &mut Vec<Ast>) -> Result<ParseEnd, Error> {
    loop {
        match parse1(it, root)? {
            Some(p) => return Ok(p),
            None => continue,
        }
    }
}

fn handle_char(
    ch: char,
    it: &mut Peekable<Chars<'_>>,
    root: &mut Vec<Ast>,
) -> Result<Option<ParseEnd>, Error> {
    match ch {
        '|' => handle_alt(it, root)?,
        '(' => handle_subexp(it, root)?,
        ')' => return Ok(Some(ParseEnd::Rparen)),

        '?' => Rep::Quest(Greed::Greedy).apply(root)?,
        '*' => Rep::Star(Greed::Greedy).apply(root)?,
        '+' => Rep::Plus(Greed::Greedy).apply(root)?,
        '{' => try_parse_counted_repetition(it)?.apply(root)?,

        '^' => root.push(Ast::Assertion(Assertion::LineStart)),
        '$' => root.push(Ast::Assertion(Assertion::LineEnd)),

        '[' => root.push(Ast::Comp(Comp::Class(CharClass::try_parse(it)?))),
        '.' => root.push(Ast::Comp(Comp::Any)),
        '@' => root.push(Ast::Comp(Comp::TrueAny)),
        ch => root.push(Ast::Comp(Comp::Char(ch))),
    }

    Ok(None)
}

/// Three variants of submatch / group are supported by this engine:
///   1) "(...)"
///      Capturing: the position of this sub-expression will be extracted as submatch
///      and made available through a numeric index based on it's position within the
///      regular expression.
///   2) "(?<name>...)"
///      Named capturing: the position of this subexpression will be extracted as a submatch
///      and made available through the provided name rather than an index. To avoid confusion
///      with mixing and matching named capture groups and positional ones, the presence of a
///      named capture group will mark all unnamed capture groups within that regex to be
///      treated as non-capturing. If they are required, they will also need to be named.
///   3) "(?:...)"
///      Non-capturing: allows for grouping and application of repetition / alternation
///      of compound expressions without contributing to the captured sub-expressions.
fn handle_subexp(it: &mut Peekable<Chars<'_>>, root: &mut Vec<Ast>) -> Result<(), Error> {
    let mut sub = Vec::new();
    let kind = match it.peek() {
        Some('?') => {
            it.next();
            match it.next() {
                Some(':') => SmKind::NonCapturing,
                Some('<') => {
                    let mut name = String::new();
                    for ch in it.by_ref() {
                        if ch == '>' {
                            break;
                        }
                        name.push(ch);
                    }
                    if it.peek().is_none() {
                        return Err(Error::UnclosedGroupName(name));
                    }
                    SmKind::Named(name)
                }
                Some(ch) => return Err(Error::UnknownGroupQualifier(ch)),
                None => return Err(Error::UnbalancedParens),
            }
        }
        _ => SmKind::Normal,
    };

    let node = match parse_many(it, &mut sub)? {
        ParseEnd::Eof => return Err(Error::UnbalancedParens),
        ParseEnd::Rparen => match sub.len() {
            0 => return Err(Error::EmptyParens),
            1 => sub.remove(0),
            _ => Ast::Concat(sub),
        },
    };

    root.push(Ast::SubMatch(kind, Box::new(node)));

    Ok(())
}

fn handle_alt(it: &mut Peekable<Chars<'_>>, root: &mut Vec<Ast>) -> Result<(), Error> {
    if root.is_empty() {
        return Err(Error::UnbalancedAlt);
    }
    let mut first = Vec::new();
    swap(&mut first, root);
    let mut alt = vec![Ast::concat_or_node(first)];
    let mut buf = Vec::new();

    loop {
        if parse1(it, &mut buf)?.is_some() {
            if buf.is_empty() {
                return Err(Error::UnbalancedAlt);
            }
            alt.push(Ast::concat_or_node(buf));
            break;
        }

        match it.peek() {
            Some('|') => {
                it.next();
                alt.push(Ast::concat_or_node(buf.clone()));
                buf.clear();
            }
            Some(')') => {
                alt.push(Ast::concat_or_node(buf.clone()));
                break;
            }
            _ => (),
        }
    }

    root.push(Ast::Alt(alt));
    Ok(())
}

fn handle_escaped(ch: char, root: &mut Vec<Ast>) -> Result<(), Error> {
    match ch {
        'b' => root.push(Ast::Assertion(Assertion::WordBoundary)),
        'B' => root.push(Ast::Assertion(Assertion::NonWordBoundary)),

        'd' => root.push(Ast::Comp(Comp::Numeric)),
        'D' => root.push(Ast::Comp(Comp::NonNumeric)),
        'w' => root.push(Ast::Comp(Comp::AlphaNumeric)),
        'W' => root.push(Ast::Comp(Comp::NonAlphaNumeric)),
        's' => root.push(Ast::Comp(Comp::WhiteSpace)),
        'S' => root.push(Ast::Comp(Comp::NonWhiteSpace)),

        ch => root.push(Ast::Comp(Comp::Char(ch))),
    }

    Ok(())
}

fn try_parse_counted_repetition(it: &mut Peekable<Chars<'_>>) -> Result<Counted, Error> {
    let (mut ch, _) = next_char(it)?.ok_or(Error::InvalidRepetition)?;

    if !ch.is_ascii_digit() {
        return Err(Error::InvalidRepetition);
    }
    let n = parse_num(ch, it).ok_or(Error::InvalidRepetition)?;
    if n == 0 {
        return Err(Error::InvalidRepetition);
    }

    (ch, _) = next_char(it)?.ok_or(Error::InvalidRepetition)?;
    if ch == '}' {
        return Ok(Counted::Rep(n));
    } else if ch != ',' {
        return Err(Error::InvalidRepetition);
    }

    (ch, _) = next_char(it)?.ok_or(Error::InvalidRepetition)?;
    if ch == '}' {
        return Ok(Counted::RepAtLeast(n));
    }

    if !ch.is_ascii_digit() {
        return Err(Error::InvalidRepetition);
    }
    let m = parse_num(ch, it).ok_or(Error::InvalidRepetition)?;
    if m == 0 {
        return Err(Error::InvalidRepetition);
    }

    (ch, _) = next_char(it)?.ok_or(Error::InvalidRepetition)?;
    if ch == '}' {
        Ok(Counted::RepBetween(n, m))
    } else {
        Err(Error::InvalidRepetition)
    }
}

/// Attempt to parse a number from a sequence of digits.
///
/// `initial` must be an ASCII digit (enforced with an assert).
/// This function will return `None` if the parsed sequence of digits is too large
/// to fit within a `usize`.
fn parse_num(initial: char, it: &mut Peekable<Chars<'_>>) -> Option<usize> {
    assert!(initial.is_ascii_digit());
    let mut s = String::from(initial);
    loop {
        match it.peek() {
            Some(ch) if ch.is_ascii_digit() => {
                s.push(it.next().unwrap());
            }
            _ => return s.parse().ok(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Rep::*;
    use super::*;
    use Assertion::*;
    use Greed::*;
    use simple_test_case::test_case;

    fn cat(nodes: Vec<Ast>) -> Ast {
        Ast::Concat(nodes)
    }

    fn alt(nodes: Vec<Ast>) -> Ast {
        Ast::Alt(nodes)
    }

    fn sub(s: Ast) -> Ast {
        Ast::SubMatch(SmKind::Normal, Box::new(s))
    }

    fn ncsub(s: Ast) -> Ast {
        Ast::SubMatch(SmKind::NonCapturing, Box::new(s))
    }

    fn nsub(name: &str, s: Ast) -> Ast {
        Ast::SubMatch(SmKind::Named(name.to_string()), Box::new(s))
    }

    fn ch(c: char) -> Ast {
        Ast::Comp(Comp::Char(c))
    }

    fn asr(a: Assertion) -> Ast {
        Ast::Assertion(a)
    }

    fn rep(r: super::Rep, c: char) -> Ast {
        Ast::Rep(r, Box::new(ch(c)))
    }

    #[test_case("a", ch('a'); "single char")]
    #[test_case("abc", cat(vec![ch('a'), ch('b'), ch('c')]); "chars only")]
    #[test_case("^a", cat(vec![asr(LineStart), ch('a')]); "line start")]
    #[test_case("a$", cat(vec![ch('a'), asr(LineEnd)]); "line end")]
    #[test_case("ab+", cat(vec![ch('a'), rep(Plus(Greedy), 'b')]); "plus")]
    #[test_case("ab*", cat(vec![ch('a'), rep(Star(Greedy), 'b')]); "star")]
    #[test_case("ab?", cat(vec![ch('a'), rep(Quest(Greedy), 'b')]); "question mark")]
    #[test_case("ab+?", cat(vec![ch('a'), rep(Plus(Lazy), 'b')]); "lazy plus")]
    #[test_case("ab*?", cat(vec![ch('a'), rep(Star(Lazy), 'b')]); "lazy star")]
    #[test_case("ab??", cat(vec![ch('a'), rep(Quest(Lazy), 'b')]); "lazy question mark")]
    #[test_case("a|b", alt(vec![ch('a'), ch('b')]); "single char alt")]
    #[test_case(
        "ab(c|d)",
        cat(vec![ch('a'), ch('b'), sub(alt(vec![ch('c'), ch('d')]))]);
        "alt in sub"
    )]
    #[test_case("(foo)", sub(cat(vec![ch('f'), ch('o'), ch('o')])); "sub expression")]
    #[test_case("(?:foo)", ncsub(cat(vec![ch('f'), ch('o'), ch('o')])); "non capturing sub expression")]
    #[test_case("(?<bar>foo)", nsub("bar", cat(vec![ch('f'), ch('o'), ch('o')])); "named sub expression")]
    #[test_case(
        "(?<bar>foo)(a|b)",
        cat(vec![nsub("bar", cat(vec![ch('f'), ch('o'), ch('o')])), ncsub(alt(vec![ch('a'), ch('b')]))]);
        "named sub expression should demote non named to non capturing"
    )]
    #[test]
    fn parse_works(re: &str, expected: Ast) {
        let res = parse(re).unwrap();
        assert_eq!(res, expected);
    }

    #[test_case("abc", "cba"; "literals only")]
    #[test_case("ab+c", "cb+a"; "literals with plus")]
    #[test_case("a*bc", "cba*"; "literals with star")]
    #[test_case("abc?", "c?ba"; "literals with quest")] // typos:ignore
    #[test_case("a(bc)+", "(cb)+a"; "repeated capture group")]
    #[test_case("a|b", "b|a"; "alts")]
    #[test_case("[Gg]oo+gle", "elgo+o[Gg]"; "with class and rep")]
    #[test]
    fn ast_reverse_works(re_forward: &str, re_back: &str) {
        let mut forward_ast = parse(re_forward).unwrap();
        forward_ast.reverse();
        let back_ast = parse(re_back).unwrap();

        assert_eq!(forward_ast, back_ast);
    }

    #[test_case("abc", &["abc"]; "literals only")]
    #[test_case("ab+c", &["ab"]; "literals with plus")]
    #[test_case("a+bc", &["a"]; "leading plus")]
    #[test_case("a*bc", &["a", "bc"]; "leading star")]
    #[test_case("ba*", &["b", "ba"]; "star after lit")] // typos:ignore
    #[test_case("ab*c", &["a", "ab", "ac"]; "star between lits")]
    #[test_case("abcd*e", &["abc", "abcd", "abce"]; "star between lits long")]
    #[test_case("a?bc", &["a"]; "leading question mark")]
    #[test_case("abc?", &["ab", "abc"]; "literals with question mark")]
    #[test_case("a(bc)+", &["abc"]; "repeated capture group")]
    #[test_case("ac|bd", &["ac", "bd"]; "alts")]
    #[test_case("ac*x|bd", &["a", "ac", "ax", "bd"]; "alts with star")]
    #[test_case("[Gg]oo+gle", &["Goo", "goo"]; "with class and rep")]
    #[test_case(".*", &[]; "dot star")]
    #[test_case(".+", &[]; "dot plus")]
    #[test_case(".?", &[]; "dot question mark")]
    #[test_case(".*a", &[]; "leading dot star")]
    #[test_case(".+a", &[]; "leading dot plus")]
    #[test_case(".?a", &[]; "leading dot question mark")]
    #[test_case("([0-2]+)-more", &["0", "1", "2"]; "leading submatch")]
    // Cases where we exceed MAX_LEADING_LITERALS and therefor return nothing
    #[test_case("[a-zA-Z]+foo", &[]; "char class exceeds limit")]
    #[test_case("[a-z]|[A-Z]|[0-9]", &[]; "alt branches exceed limit")]
    #[test_case("[a-z][a-c]foo", &[]; "concat product exceeds limit")]
    #[test]
    fn ast_leading_literal_patterns_works(re: &str, expected: &[&str]) {
        let ast = parse(re).unwrap();
        println!("AST {ast:?}");
        let pats = ast.leading_literals();
        let expected: HashSet<String> = expected.iter().map(|s| s.to_string()).collect();

        assert_eq!(pats, expected);
    }

    #[test_case("a{99999999999999999999}"; "single count")]
    #[test_case("a{99999999999999999999,5}"; "lower bound")]
    #[test_case("a{2,99999999999999999999}"; "upper bound")]
    #[test_case("a{99999999999999999999,99999999999999999999}"; "both bounds")]
    #[test]
    fn giant_counted_repetitions_error(re: &str) {
        assert!(parse(re).is_err());
    }
}
