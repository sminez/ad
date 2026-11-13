//! A plumbing interface for user defined "loading" of text inspired by plan 9's plumber.
//!
//! See the following papers and man pages for references on the original plan 9 system:
//!   - <http://doc.cat-v.org/plan_9/4th_edition/papers/plumb>
//!   - <http://man.cat-v.org/plan_9_3rd_ed/1/plumb>
//!   - <http://man.cat-v.org/plan_9_3rd_ed/2/plumb>
//!   - <http://man.cat-v.org/plan_9_3rd_ed/4/plumber>
//!   - <http://man.cat-v.org/plan_9_3rd_ed/6/plumb>
use crate::regex::Regex;
use shellexpand::env_with_context_no_errors;
use std::{
    collections::BTreeMap,
    env, fs, io,
    process::{Command, Stdio},
    str::FromStr,
};
use tracing::debug;

/// An ordered list of plumbing rules to use whenever something is "loaded" within the editor.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct PlumbingRules {
    rules: Vec<Rule>,
    vars: BTreeMap<String, String>,
}

impl FromStr for PlumbingRules {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut prs = Self::default();

        for raw_block in s.split("\n\n") {
            let lines: Vec<_> = raw_block
                .trim()
                .lines()
                .filter(|l| !l.starts_with('#'))
                .collect();

            let mut block = lines.join("\n");
            if block.is_empty() {
                continue;
            }

            match block.split_once(' ') {
                // Parse variable declaration blocks
                Some((_, s)) if s.starts_with("=") => {
                    for line in block.lines() {
                        match line.split_once("=") {
                            Some((var, val)) => {
                                let mut k = non_empty_string(var.trim(), line)?;
                                k.insert(0, '$');
                                // Variables may refer to any variables declared before them
                                let v = non_empty_string(val.trim(), line)?;
                                let v = expand_vars(&v, &prs.vars, |_, v| v.to_string());
                                prs.vars.insert(k, v);
                            }
                            _ => return Err(format!("malformed line: {line:?}")),
                        }
                    }
                }

                // Apply variables and parse rule blocks
                _ => {
                    block = expand_vars(&block, &prs.vars, |_, v| v.to_string());
                    prs.rules.push(Rule::from_str(&block)?);
                }
            }
        }

        Ok(prs)
    }
}

impl PlumbingRules {
    /// Attempt to load plumbing rules from the default location
    pub fn try_load() -> Result<Self, String> {
        let home = env::var("HOME").unwrap();

        Self::try_load_from_path(&format!("{home}/.ad/plumbing.rules"))
    }

    /// Attempt to load plumbing rules from a specified file
    pub fn try_load_from_path(path: &str) -> Result<Self, String> {
        let s = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(Self::default()),
            Err(e) => return Err(format!("Unable to load plumbing rules: {e}")),
        };

        match Self::from_str(&s) {
            Ok(cfg) => Ok(cfg),
            Err(e) => Err(format!("Invalid plumbing rules: {e}")),
        }
    }

    /// Run the provided message through the plumbing rules to determine how it should be
    /// handled. If no rules match then None is returned and default handling for a load
    /// takes place instead. The returned message may differ from the one passed in if
    /// rules carry out rewrites.
    pub fn plumb(&mut self, msg: PlumbingMessage) -> Option<MatchOutcome> {
        let vars = msg.initial_vars();
        debug!("plumbing message: {msg:?}");

        for (n, rule) in self.rules.iter_mut().enumerate() {
            debug!("checking rule {n}");
            let mut rule_vars = vars.clone();
            if let Some(msg) = rule.try_match(msg.clone(), &mut rule_vars) {
                debug!("rule matched");
                return Some(msg);
            }
        }

        debug!("no matching rules");
        None
    }
}

/// The deserialized form of a plumbing message sent by a client.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct PlumbingMessage {
    /// The application or service generating the message
    pub src: Option<String>,
    /// The destination "port" for the message
    pub dst: Option<String>,
    /// The working directory (used when data is a filename)
    pub wdir: Option<String>,
    /// The character offset within 'data' where the user's cursor currently lies.
    /// If a rule specifies a 'narrows-to' rule it will be used to narrow initial data
    /// to only include the specified cursor, otherwise the match will be rejected.
    pub cur: usize,
    /// Name=value pairs. Must not contain newlines
    pub attrs: BTreeMap<String, String>,
    /// The string content of the message itself
    pub data: String,
}

impl PlumbingMessage {
    fn initial_vars(&self) -> BTreeMap<String, String> {
        let mut vars = BTreeMap::new();
        if let Some(s) = self.src.clone() {
            vars.insert("$src".to_string(), s);
        }
        if let Some(s) = self.dst.clone() {
            vars.insert("$dst".to_string(), s);
        }
        if let Some(s) = self.wdir.clone() {
            vars.insert("$wdir".to_string(), s);
        }
        vars.insert("$data".to_string(), self.data.clone());

        vars
    }
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

fn parse_attr_list(s: &str) -> Result<BTreeMap<String, String>, String> {
    let mut attrs = BTreeMap::new();
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
        let mut cur_set = false;

        for line in &mut it {
            parse_field!(line, "src", "src: ", msg.src);
            parse_field!(line, "dst", "dst: ", msg.dst);
            parse_field!(line, "wdir", "wdir: ", msg.wdir);

            match (line.strip_prefix("cur: "), &mut msg.cur) {
                (Some(_), _) if cur_set => return Err("duplicate cur field".to_string()),
                (Some(val), _) => {
                    msg.cur = match val.parse() {
                        Ok(cur) => cur,
                        Err(e) => return Err(format!("malformed cur field: {e}")),
                    };
                    cur_set = true;
                    continue;
                }
                (None, _) => (),
            }

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
    AddAttrs(BTreeMap<String, String>),
    DelAttr(String),
    DataFrom(String),
    IsFile(String),
    IsDir(String),
    Is(Field, String),
    Matches(Field, Regex),
    NarrowsTo(Regex),
    Set(Field, String),
}

impl Pattern {
    fn kind_str(&self) -> &'static str {
        match self {
            Self::AddAttrs(_) => "add-attrs",
            Self::DelAttr(_) => "del-attr",
            Self::DataFrom(_) => "data-from",
            Self::IsFile(_) => "is-file",
            Self::IsDir(_) => "is-dir",
            Self::Is(_, _) => "is",
            Self::Matches(_, _) => "matches",
            Self::NarrowsTo(_) => "narrows-to",
            Self::Set(_, _) => "set",
        }
    }

    fn match_and_update(
        &mut self,
        msg: &mut PlumbingMessage,
        vars: &mut BTreeMap<String, String>,
    ) -> bool {
        let apply_vars = |s: String| expand_vars(&s, vars, |_, v| v.to_string());

        let re_match_and_update =
            |f: Field, re: &mut Regex, vars: &mut BTreeMap<String, String>| {
                debug!("regex match against {}", f.name());
                let opt = match f {
                    Field::Src => msg.src.as_ref(),
                    Field::Dst => msg.dst.as_ref(),
                    Field::Wdir => msg.wdir.as_ref(),
                    Field::Data => Some(&msg.data),
                };
                let s = match opt {
                    Some(s) => s,
                    None => {
                        debug!("unable to match against {} (not set in message)", f.name());
                        return false;
                    }
                };

                if let Some(m) = re.captures(&s.as_str()) {
                    debug!("matched: updating vars");
                    vars.insert("$0".to_string(), m.match_text(&s.as_str()).into_owned());
                    for n in 1..10 {
                        match m.submatch_text(n, &s.as_str()) {
                            Some(txt) => {
                                vars.insert(format!("${n}"), txt.into_owned());
                            }
                            None => return true,
                        }
                    }
                    return true;
                }

                debug!("message data did not match the provided regex");
                false
            };

        debug!("checking {} pattern", self.kind_str());
        match self {
            Self::AddAttrs(attrs) => {
                debug!("adding attrs: {attrs:?}");
                msg.attrs.extend(
                    attrs
                        .clone()
                        .into_iter()
                        .map(|(k, v)| (apply_vars(k), apply_vars(v))),
                );
            }

            Self::DelAttr(a) => {
                debug!("removing attr: {a}");
                msg.attrs.remove(a);
            }

            Self::IsFile(s) => {
                debug!("checking if {s:?} is a file");
                let path = apply_vars(s.clone());
                match fs::metadata(&path) {
                    Ok(m) => {
                        if m.is_file() {
                            debug!("{path:?} exists and is a file");
                            vars.insert("$file".to_string(), path);
                        } else {
                            debug!("{path:?} exists but is not a file");
                            return false;
                        }
                    }

                    Err(e) => {
                        debug!("unable to check {path:?}: {e}");
                        return false;
                    }
                }
            }

            Self::IsDir(s) => {
                debug!("checking if {s:?} is a directory");
                let path = apply_vars(s.clone());
                match fs::metadata(&path) {
                    Ok(m) => {
                        if m.is_dir() {
                            debug!("{path:?} exists and is a directory");
                            vars.insert("$dir".to_string(), path);
                        } else {
                            debug!("{path:?} exists but is not a directory");
                            return false;
                        }
                    }

                    Err(e) => {
                        debug!("unable to check {path:?}: {e}");
                        return false;
                    }
                }
            }

            Self::Is(Field::Src, s) => {
                let res = msg.src.as_ref() == Some(s);
                debug!("checking src == {s:?}: {res}");
                return res;
            }
            Self::Is(Field::Dst, s) => {
                let res = msg.dst.as_ref() == Some(s);
                debug!("checking dst == {s:?}: {res}");
                return res;
            }
            Self::Is(Field::Wdir, s) => {
                let res = msg.wdir.as_ref() == Some(s);
                debug!("checking wdir == {s:?}: {res}");
                return res;
            }
            Self::Is(Field::Data, s) => {
                let res = &msg.data == s;
                debug!("checking data == {s:?}: {res}");
                return res;
            }

            Self::Set(Field::Src, s) => {
                let updated = apply_vars(s.clone());
                debug!("setting src to {updated:?}");
                msg.src = Some(updated.clone());
                vars.insert("$src".to_string(), updated);
            }
            Self::Set(Field::Dst, s) => {
                let updated = apply_vars(s.clone());
                debug!("setting dst to {updated:?}");
                msg.dst = Some(updated.clone());
                vars.insert("$dst".to_string(), updated);
            }
            Self::Set(Field::Wdir, s) => {
                let updated = apply_vars(s.clone());
                debug!("setting wdir to {updated:?}");
                msg.wdir = Some(updated.clone());
                vars.insert("$wdir".to_string(), updated);
            }
            Self::Set(Field::Data, s) => {
                let updated = apply_vars(s.clone());
                debug!("setting data to {updated:?}");
                msg.data = updated.clone();
                vars.insert("$data".to_string(), updated);
            }

            Self::Matches(f, re) => return re_match_and_update(*f, re, vars),

            Self::NarrowsTo(re) => {
                debug!(%msg.cur, "narrowing for provided cur");
                for m in re.captures_iter(&msg.data.as_str()) {
                    let (from, to) = m.loc();
                    // The match location is in bytes but cur is a character offset
                    let ch_from = msg.data[..from.0].chars().count();
                    let ch_to = ch_from + msg.data[from.0..to.0].chars().count();
                    if ch_from <= msg.cur && msg.cur <= ch_to {
                        debug!(%from, %to, "successfully narrowed");
                        msg.data = m.match_text(&msg.data.as_str()).into_owned();
                        msg.cur = 0; // consume the cursor as it is now invalid
                        return true;
                    }
                }

                return false; // unable to narrow to cur
            }

            Self::DataFrom(cmd) => {
                debug!("running {cmd:?} to set message data");
                let mut command = ShellCommand::new(cmd, vars).as_command();
                command.stderr(Stdio::null());
                let output = match command.output() {
                    Ok(output) => output,
                    Err(_) => return false,
                };
                msg.data = String::from_utf8(output.stdout).unwrap_or_default();
            }
        }

        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Field {
    Src,
    Dst,
    Wdir,
    Data,
}

impl Field {
    fn name(&self) -> &'static str {
        match self {
            Self::Src => "src",
            Self::Dst => "dst",
            Self::Wdir => "wdir",
            Self::Data => "data",
        }
    }
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

/// The result of a successful rule match that should be handled by ad.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchOutcome {
    /// A message that should be handled by ad
    Message(PlumbingMessage),
    /// A command that should be run instead of handling the message
    Run(ShellCommand),
}

/// A parsed plumbing rule for matching against incoming messages.
/// If all patterns match then the resulting actions are run until
/// one succeeds.
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
            } else if let Some(s) = line.strip_prefix("data from ") {
                rule.patterns
                    .push(Pattern::DataFrom(non_empty_string(s, line)?));
            } else if let Some(s) = line.strip_prefix("plumb to ") {
                rule.actions.push(Action::To(non_empty_string(s, line)?));
            } else if let Some(s) = line.strip_prefix("plumb start ") {
                rule.actions.push(Action::Start(non_empty_string(s, line)?));
            } else if let Some(s) = line.strip_prefix("data narrows ") {
                rule.patterns.push(Pattern::NarrowsTo(
                    Regex::compile(s).map_err(|e| format!("malformed regex ({e:?}): {s}"))?,
                ));
            } else {
                // patterns of the form $field $op $value
                let (field, rest) = line
                    .split_once(' ')
                    .ok_or_else(|| format!("malformed rule line: {line}"))?;
                let field = Field::from_str(field)?;
                let (op, value) = rest
                    .split_once(' ')
                    .ok_or_else(|| format!("malformed rule line: {line}"))?;
                let value = non_empty_string(value, line)?;

                match op {
                    "is" => rule.patterns.push(Pattern::Is(field, value)),
                    "set" => rule.patterns.push(Pattern::Set(field, value)),
                    "matches" => rule.patterns.push(Pattern::Matches(
                        field,
                        Regex::compile(&value)
                            .map_err(|e| format!("malformed regex ({e:?}): {value}"))?,
                    )),
                    _ => return Err(format!("unknown rule operation: {op}")),
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

impl Rule {
    fn try_match(
        &mut self,
        mut msg: PlumbingMessage,
        vars: &mut BTreeMap<String, String>,
    ) -> Option<MatchOutcome> {
        for p in self.patterns.iter_mut() {
            if !p.match_and_update(&mut msg, vars) {
                debug!("pattern failed to match");
                return None;
            }
        }

        for a in self.actions.iter() {
            match a {
                // TODO: when other ports are supported they will need handling here!
                Action::To(port) if port == "edit" => return Some(MatchOutcome::Message(msg)),
                Action::Start(cmd) => return Some(MatchOutcome::Run(ShellCommand::new(cmd, vars))),
                _ => continue,
            }
        }

        None
    }
}

/// A rewritten shell command where all plumbing variable references have been rewritten to instead
/// refer to environment variables that we provide when the command executes.
///
/// Handling things through environment variables this way ensures that the contents of any
/// plumbing variable are used verbatim rather than being interpreted by the shell.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShellCommand {
    script: String,
    env: Vec<(String, String)>,
}

impl ShellCommand {
    fn new(template: &str, vars: &BTreeMap<String, String>) -> Self {
        let mut env = Vec::new();

        // Collect all variable references from `template` and replace them with $PLUMB_var_name
        // shell variables which we can later provide as this commands environment when it
        // executes.
        let script = expand_vars(template, vars, |k, v| {
            let env_var = format!("PLUMB_{}", &k[1..]);
            let replacement_txt = format!("\"${{{env_var}}}\"");
            env.push((env_var, v.to_string()));

            replacement_txt
        });

        Self { script, env }
    }

    pub fn as_command(&self) -> Command {
        let mut cmd = Command::new("sh");
        cmd.args(["-c", &self.script])
            .envs(self.env.iter().cloned());

        cmd
    }
}

fn non_empty_string(s: &str, line: &str) -> Result<String, String> {
    if s.is_empty() {
        Err(format!("malformed rule line: {line:?}"))
    } else {
        Ok(s.to_string())
    }
}

fn expand_vars(
    s: &str,
    vars: &BTreeMap<String, String>,
    mut f: impl FnMut(&str, &str) -> String,
) -> String {
    env_with_context_no_errors(s, |name| {
        vars.get_key_value(&format!("${name}"))
            .map(|(k, v)| f(k, v))
    })
    .into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::{dir_cases, test_case};
    use simple_txtar::Archive;

    #[test]
    fn parse_default_rules_works() {
        let rules = PlumbingRules::from_str(include_str!("../data/plumbing.rules"));
        assert!(rules.is_ok(), "{rules:?}");
    }

    #[test]
    fn happy_path_plumb_works() {
        let mut rules = PlumbingRules::from_str(include_str!("../data/plumbing.rules")).unwrap();
        let m = PlumbingMessage {
            data: "data/plumbing.rules:5:17:".to_string(),
            ..Default::default()
        };

        let outcome = rules.plumb(m);
        let m = match outcome {
            Some(MatchOutcome::Message(m)) => m,
            _ => panic!("expected message, got {outcome:?}"),
        };

        let expected = PlumbingMessage {
            data: "data/plumbing.rules".to_string(),
            attrs: [("addr".to_string(), "5:17".to_string())]
                .into_iter()
                .collect(),
            ..Default::default()
        };

        assert_eq!(m, expected);
    }

    #[test_case("$home", "/h"; "short name")]
    #[test_case("$homedir", "/hd"; "longer name sharing a prefix")]
    #[test_case("${home}dir", "/hdir"; "braces separate name from text")]
    #[test_case("$0x", "$0x"; "names are matched greedily")]
    #[test_case("$0.pdf", "a.pdf"; "punctuation ends a name")]
    #[test_case("$HOME/$0", "$HOME/a"; "unknown variables are left as is")]
    #[test_case("cost: 5$", "cost: 5$"; "trailing dollar is left as is")]
    #[test]
    fn expand_vars_works(s: &str, expected: &str) {
        let vars: BTreeMap<String, String> = [("$home", "/h"), ("$homedir", "/hd"), ("$0", "a")]
            .into_iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();

        assert_eq!(expand_vars(s, &vars, |_, v| v.to_string()), expected);
    }

    #[test]
    fn plumb_start_passes_variables_via_env() {
        let mut rules =
            PlumbingRules::from_str("data matches .+\nplumb start xdg-open $0").unwrap();
        let m = PlumbingMessage {
            data: "https://x.co/?a&reboot".to_string(),
            ..Default::default()
        };

        let expected = ShellCommand {
            script: "xdg-open \"${PLUMB_0}\"".to_string(),
            env: vec![("PLUMB_0".to_string(), "https://x.co/?a&reboot".to_string())],
        };

        assert_eq!(rules.plumb(m), Some(MatchOutcome::Run(expected)));
    }

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
            cur: 0,
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

    #[dir_cases("data/plumbing_tests/match-tests")]
    #[test]
    fn match_tests(_fname: &str, content: &str) {
        let arr = Archive::from(content);
        let comment = arr.comment();
        if !comment.is_empty() {
            println!("{comment}"); // help with debugging
        }

        let mut rules =
            PlumbingRules::from_str(&arr.get("rules").expect("missing rules").content).unwrap();
        let initial =
            PlumbingMessage::from_str(&arr.get("initial").expect("missing initial").content)
                .unwrap();

        let raw_processed = &arr.get("processed").expect("missing processed").content;
        let processed = if raw_processed.is_empty() {
            None
        } else {
            Some(MatchOutcome::Message(
                PlumbingMessage::from_str(raw_processed).unwrap(),
            ))
        };

        let outcome = rules.plumb(initial);
        assert_eq!(outcome, processed);
    }
}
