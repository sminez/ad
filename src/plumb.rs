//! A plumbing interface for user defined "loading" of text inspired by plan 9's plumber.
//!
//! See the following papers and man pages for references on the original plan 9 system:
//!   - http://doc.cat-v.org/plan_9/4th_edition/papers/plumb
//!   - http://man.cat-v.org/plan_9_3rd_ed/1/plumb
//!   - http://man.cat-v.org/plan_9_3rd_ed/2/plumb
//!   - http://man.cat-v.org/plan_9_3rd_ed/4/plumb
//!   - http://man.cat-v.org/plan_9_3rd_ed/6/plumb
use std::{collections::HashMap, str::FromStr};

/// The deserialized form of a plumbing message sent by a client.
#[derive(Debug, Default)]
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
                (Some(vals), true) => {
                    let mut attrs = HashMap::new();
                    for pair in vals.split(' ') {
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
                    msg.attrs = attrs;
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

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::dir_cases;

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
}
