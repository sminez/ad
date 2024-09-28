//! A plumbing interface for user defined "loading" of text inspired by plan 9's plumber.
//!
//! See the following papers and man pages for references on the original plan 9 system:
//!   - http://doc.cat-v.org/plan_9/4th_edition/papers/plumb
//!   - http://man.cat-v.org/plan_9_3rd_ed/1/plumb
//!   - http://man.cat-v.org/plan_9_3rd_ed/2/plumb
//!   - http://man.cat-v.org/plan_9_3rd_ed/4/plumb
//!   - http://man.cat-v.org/plan_9_3rd_ed/6/plumb
use crate::regex::Regex;
use std::{collections::HashMap, str::FromStr};

/// The deserialized form of a plumbing message sent by a client.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct PlumbingMessage {
    /// The application or service generating the message
    src: Option<String>,
    /// The destination "port" for the message
    dst: Option<String>,
    /// The working directory (used when data is a filename)
    wdir: Option<String>,
    /// Name=value pairs. Must not contain newlines
    attrs: HashMap<String, String>,
    /// The string content of the message itself
    data: String,
}

macro_rules! parse_field {
    ($line:expr, $field_name:expr, $prefix:expr, $field:expr) => {
        match ($line.strip_prefix($prefix), &mut $field) {
            (Some(val), None) => {
                $field = Some(val.to_string());
                continue;
            }
            (Some(_), Some(_)) => return Err(format!("duplicate {} field", $field_name)),
            (None, _) => (),
        }
    };
}

fn parse_attr_list(s: &str) -> Result<HashMap<String, String>, String> {
    let mut attrs = HashMap::new();
    for pair in s.split(' ') {
        match pair.split_once('=') {
            Some((k, v)) => {
                if k.is_empty() || v.is_empty() {
                    return Err(format!("malformed attrs: {pair:?}"));
                }
                attrs.insert(k.to_string(), v.to_string());
            }
            None => return Err(format!("malformed attrs: {pair:?}")),
        }
    }

    Ok(attrs)
}

impl FromStr for PlumbingMessage {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut msg = Self::default();
        let mut it = s.lines();
        let mut ndata = 0;

        for line in &mut it {
            parse_field!(line, "src", "src: ", msg.src);
            parse_field!(line, "dst", "dst: ", msg.dst);
            parse_field!(line, "wdir", "wdir: ", msg.wdir);

            match (line.strip_prefix("attrs: "), msg.attrs.is_empty()) {
                (Some(s), true) => {
                    msg.attrs = parse_attr_list(s)?;
                    continue;
                }
                (Some(_), false) => return Err("duplicate attrs field".to_string()),
                (None, _) => (),
            }

            match line.strip_prefix("ndata: ") {
                Some(n) => {
                    ndata = match n.parse() {
                        Ok(ndata) => ndata,
                        Err(_) => return Err(format!("invalid ndata field {n:?}")),
                    };
                    break;
                }
                None => return Err(format!("malformed message: {line:?}")),
            }
        }

        if ndata > 0 {
            let stripped_lines: Vec<&str> = it.collect();
            let joined = stripped_lines.join("\n");
            match joined.strip_prefix("data: ") {
                Some(data) => msg.data = data.to_string(),
                None => return Err("malformed message: missing data field".to_string()),
            }
            if msg.data.len() != ndata {
                return Err(format!(
                    "malformed data. Expected {ndata} bytes but received {}: {:?}",
                    msg.data.len(),
                    msg.data
                ));
            }
        }

        Ok(msg)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Pattern {
    AddAttrs(HashMap<String, String>),
    DelAttr(String),
    IsFile(String),
    IsDir(String),
    Is(Field, String),
    Matches(Field, Regex),
    Set(Field, String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Field {
    Src,
    Dst,
    Wdir,
    Data,
}

impl FromStr for Field {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "src" => Ok(Self::Src),
            "dst" => Ok(Self::Dst),
            "wdir" => Ok(Self::Wdir),
            "data" => Ok(Self::Data),
            s => Err(format!("unknown field: {s:?}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Action {
    To(String),
    Start(String),
}

/// A parsed plumbing rule for matching against incoming messages.
/// If all patterns match then the resulting actions are run.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Rule {
    patterns: Vec<Pattern>,
    actions: Vec<Action>,
}

impl FromStr for Rule {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut rule = Self::default();

        for line in s.lines() {
            if line.starts_with('#') {
                continue;
            }

            // actions and patterns that start with a fixed prefix
            if let Some(s) = line.strip_prefix("attr add ") {
                rule.patterns.push(Pattern::AddAttrs(parse_attr_list(s)?));
            } else if let Some(s) = line.strip_prefix("attr delete ") {
                rule.patterns
                    .push(Pattern::DelAttr(non_empty_string(s, line)?));
            } else if let Some(s) = line.strip_prefix("arg isfile ") {
                rule.patterns
                    .push(Pattern::IsFile(non_empty_string(s, line)?));
            } else if let Some(s) = line.strip_prefix("arg isdir ") {
                rule.patterns
                    .push(Pattern::IsDir(non_empty_string(s, line)?));
            } else if let Some(s) = line.strip_prefix("plumb to ") {
                rule.actions.push(Action::To(non_empty_string(s, line)?));
            } else if let Some(s) = line.strip_prefix("plumb start ") {
                rule.actions.push(Action::Start(non_empty_string(s, line)?));
            } else {
                // patterns of the form $field $op $value
                let (field, rest) = line
                    .split_once(' ')
                    .ok_or_else(|| format!("malformed rule line: {line:?}"))?;
                let field = Field::from_str(field)?;
                let (op, value) = rest
                    .split_once(' ')
                    .ok_or_else(|| format!("malformed rule line: {line:?}"))?;
                let value = non_empty_string(value, line)?;

                match op {
                    "is" => rule.patterns.push(Pattern::Is(field, value)),
                    "set" => rule.patterns.push(Pattern::Set(field, value)),
                    "matches" => rule.patterns.push(Pattern::Matches(
                        field,
                        Regex::compile(&value)
                            .map_err(|_| format!("malformed regex: {value:?}"))?,
                    )),
                    _ => return Err(format!("unknown rule operation {op:?}")),
                }
            }
        }

        if rule.patterns.is_empty() {
            Err("rule without patterns".to_string())
        } else if rule.actions.is_empty() {
            Err("rule without actions".to_string())
        } else {
            Ok(rule)
        }
    }
}

fn non_empty_string(s: &str, line: &str) -> Result<String, String> {
    if s.is_empty() {
        Err(format!("malformed rule line: {line:?}"))
    } else {
        Ok(s.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::dir_cases;

    #[test]
    fn parse_message_works() {
        let m = PlumbingMessage::from_str(include_str!(
            "../data/plumbing_tests/messages/valid/all-fields.txt"
        ))
        .unwrap();

        let expected = PlumbingMessage {
            src: Some("bash".to_string()),
            dst: Some("ad".to_string()),
            wdir: Some("/home/foo/bar".to_string()),
            attrs: [
                ("a".to_string(), "b".to_string()),
                ("c".to_string(), "d".to_string()),
            ]
            .into_iter()
            .collect(),
            data: "hello, world!".to_string(),
        };

        assert_eq!(m, expected);
    }

    #[dir_cases("data/plumbing_tests/messages/valid")]
    #[test]
    fn parse_valid_message(_: &str, content: &str) {
        let res = PlumbingMessage::from_str(content);
        assert!(res.is_ok(), "{res:?}");
    }

    #[dir_cases("data/plumbing_tests/messages/invalid")]
    #[test]
    fn parse_invalid_message(_: &str, content: &str) {
        let res = PlumbingMessage::from_str(content);
        assert!(res.is_err(), "{res:?}");
    }

    #[test]
    fn parse_rule_works() {
        let m = Rule::from_str(include_str!(
            "../data/plumbing_tests/rules/valid/everything.txt"
        ))
        .unwrap();

        let expected = Rule {
            patterns: vec![
                Pattern::AddAttrs([("a".to_string(), "b".to_string())].into_iter().collect()),
                Pattern::DelAttr("c".to_string()),
                Pattern::IsFile("$data".to_string()),
                Pattern::IsDir("/var/lib".to_string()),
                Pattern::Is(Field::Src, "ad".to_string()),
                Pattern::Is(Field::Dst, "editor".to_string()),
                Pattern::Set(Field::Wdir, "/foo/bar".to_string()),
                Pattern::Matches(Field::Data, Regex::compile(r#"(.+):(\d+):(\d+):"#).unwrap()),
                Pattern::Set(Field::Data, "$1:$2,$3".to_string()),
            ],
            actions: vec![
                Action::To("editor".to_string()),
                Action::Start("ad $file".to_string()),
            ],
        };

        assert_eq!(m, expected);
    }

    #[dir_cases("data/plumbing_tests/rules/valid")]
    #[test]
    fn parse_valid_rule(_: &str, content: &str) {
        let res = Rule::from_str(content);
        assert!(res.is_ok(), "{res:?}");
    }

    #[dir_cases("data/plumbing_tests/rules/invalid")]
    #[test]
    fn parse_invalid_rule(_: &str, content: &str) {
        let res = Rule::from_str(content);
        assert!(res.is_err(), "{res:?}");
    }
}
