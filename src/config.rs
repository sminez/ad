//! A minimal config file format for ad
use crate::{key::Input, mode::normal_mode, term::Color};
use std::{collections::BTreeMap, env, fs, io};

/// Editor level configuration
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub(crate) tabstop: usize,
    pub(crate) expand_tab: bool,
    pub(crate) auto_mount: bool,
    pub(crate) match_indent: bool,
    pub(crate) status_timeout: u64,
    pub(crate) minibuffer_lines: usize,
    pub(crate) find_command: String,
    pub(crate) colorscheme: ColorScheme,
    pub(crate) bindings: BTreeMap<Vec<Input>, String>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            tabstop: 4,
            expand_tab: true,
            auto_mount: false,
            match_indent: true,
            status_timeout: 3,
            minibuffer_lines: 8,
            find_command: "fd -t f".to_string(),
            colorscheme: ColorScheme::default(),
            bindings: BTreeMap::new(),
        }
    }
}

/// A colorscheme for the terminal UI
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ColorScheme {
    // ui
    pub(crate) bg: Color,
    pub(crate) fg: Color,
    pub(crate) dot_bg: Color,
    pub(crate) load_bg: Color,
    pub(crate) exec_bg: Color,
    pub(crate) bar_bg: Color,
    pub(crate) signcol_fg: Color,
    pub(crate) minibuffer_hl: Color,
    // syntax
    pub(crate) comment: Color,
    pub(crate) keyword: Color,
    pub(crate) control_flow: Color,
    pub(crate) definition: Color,
    pub(crate) punctuation: Color,
    pub(crate) string: Color,
}

impl Default for ColorScheme {
    fn default() -> Self {
        Self {
            // ui
            bg: "#1B1720".try_into().unwrap(),
            fg: "#E6D29E".try_into().unwrap(),
            dot_bg: "#336677".try_into().unwrap(),
            load_bg: "#957FB8".try_into().unwrap(),
            exec_bg: "#Bf616A".try_into().unwrap(),
            bar_bg: "#4E415C".try_into().unwrap(),
            signcol_fg: "#544863".try_into().unwrap(),
            minibuffer_hl: "#3E3549".try_into().unwrap(),
            // syntax
            comment: "#624354".try_into().unwrap(),
            keyword: "#Bf616A".try_into().unwrap(),
            control_flow: "#7E9CD8".try_into().unwrap(),
            definition: "#957FB8".try_into().unwrap(),
            punctuation: "#DCA561".try_into().unwrap(),
            string: "#61DCA5".try_into().unwrap(),
        }
    }
}

impl Config {
    /// Attempt to load a config file from the default location
    pub fn try_load() -> Result<Self, String> {
        let home = env::var("HOME").unwrap();

        let s = match fs::read_to_string(format!("{home}/.ad/init.conf")) {
            Ok(s) => s,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(Config::default()),
            Err(e) => return Err(format!("Unable to load config file: {e}")),
        };

        match Config::parse(&s) {
            Ok(cfg) => Ok(cfg),
            Err(e) => Err(format!("Invalid config file: {e}")),
        }
    }

    /// Attempt to parse the given file content as a Config file. If the file is invalid then an
    /// error message for the user is returned for displaying in the status bar.
    pub fn parse(contents: &str) -> Result<Self, String> {
        let mut cfg = Config::default();
        cfg.update_from(contents)?;

        Ok(cfg)
    }

    pub(crate) fn update_from(&mut self, input: &str) -> Result<(), String> {
        for line in input.lines() {
            let line = line.trim_end();
            if line.starts_with('#') || line.is_empty() {
                continue;
            }

            match line.strip_prefix("set ") {
                Some(line) => self.try_set_prop(line)?,
                None => match line.strip_prefix("map ") {
                    Some(line) => self.try_add_mapping(line)?,
                    None => {
                        return Err(format!(
                            "'{line}' should be 'set prop=val' or 'map ... => prog'"
                        ))
                    }
                },
            }
        }

        if !self.bindings.is_empty() {
            // Make sure that none of the user provided bindings clash with Normal mode
            // bindings as that will mean they never get run
            let nm = normal_mode();
            for keys in self.bindings.keys() {
                if nm.keymap.contains_key_or_prefix(keys) {
                    let mut s = String::new();
                    for k in keys {
                        if let Input::Char(c) = k {
                            s.push(*c);
                        }
                    }

                    return Err(format!("mapping '{s}' collides with a Normal mode mapping"));
                }
            }
        }

        Ok(())
    }

    pub(crate) fn try_set_prop(&mut self, input: &str) -> Result<(), String> {
        let (prop, val) = input
            .split_once('=')
            .ok_or_else(|| format!("'{input}' is not a 'set prop=val' statement"))?;

        match prop {
            // Strings
            "find-command" => self.find_command = val.trim().to_string(),

            // Numbers
            "tabstop" => self.tabstop = parse_usize(prop, val)?,
            "minibuffer-lines" => self.minibuffer_lines = parse_usize(prop, val)?,
            "status-timeout" => self.status_timeout = parse_usize(prop, val)? as u64,

            // Flags
            "expand-tab" => self.expand_tab = parse_bool(prop, val)?,
            "auto-mount" => self.auto_mount = parse_bool(prop, val)?,
            "match-indent" => self.match_indent = parse_bool(prop, val)?,

            // Colors
            "bg-color" => self.colorscheme.bg = parse_color(prop, val)?,
            "fg-color" => self.colorscheme.fg = parse_color(prop, val)?,
            "dot-bg-color" => self.colorscheme.dot_bg = parse_color(prop, val)?,
            "load-bg-color" => self.colorscheme.load_bg = parse_color(prop, val)?,
            "exec-bg-color" => self.colorscheme.exec_bg = parse_color(prop, val)?,
            "bar-bg-color" => self.colorscheme.bar_bg = parse_color(prop, val)?,
            "signcol-fg-color" => self.colorscheme.signcol_fg = parse_color(prop, val)?,
            "minibuffer-hl-color" => self.colorscheme.minibuffer_hl = parse_color(prop, val)?,
            "comment-color" => self.colorscheme.comment = parse_color(prop, val)?,
            "keyword-color" => self.colorscheme.keyword = parse_color(prop, val)?,
            "control-flow-color" => self.colorscheme.control_flow = parse_color(prop, val)?,
            "definition-color" => self.colorscheme.definition = parse_color(prop, val)?,
            "punctuation-color" => self.colorscheme.punctuation = parse_color(prop, val)?,
            "string-color" => self.colorscheme.string = parse_color(prop, val)?,

            _ => return Err(format!("'{prop}' is not a known config property")),
        }

        Ok(())
    }

    pub(crate) fn try_add_mapping(&mut self, input: &str) -> Result<(), String> {
        let (keys, prog) = input
            .split_once("=>")
            .ok_or_else(|| format!("'{input}' is not a 'map ... => prog' statement"))?;

        let keys: Vec<Input> = keys
            .split_whitespace()
            .filter_map(|s| {
                if s.len() == 1 {
                    let c = s.chars().next().unwrap();
                    if c.is_whitespace() {
                        None
                    } else {
                        Some(Input::Char(c))
                    }
                } else {
                    match s {
                        "<space>" => Some(Input::Char(' ')),
                        _ => None,
                    }
                }
            })
            .collect();

        self.bindings.insert(keys, prog.trim().to_string());

        Ok(())
    }
}

fn parse_usize(prop: &str, val: &str) -> Result<usize, String> {
    match val.parse() {
        Ok(num) => Ok(num),
        Err(_) => Err(format!("expected number for '{prop}' but found '{val}'")),
    }
}

fn parse_bool(prop: &str, val: &str) -> Result<bool, String> {
    match val {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(format!(
            "expected true/false for '{prop}' but found '{val}'"
        )),
    }
}

fn parse_color(prop: &str, val: &str) -> Result<Color, String> {
    Color::try_from(val)
        .map_err(|_| format!("expected #RRGGBB string for '{prop}' but found '{val}'"))
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_CONFIG: &str = include_str!("../data/init.conf");
    const CUSTOM_CONFIG: &str = "
# This is a comment


# Blank lines should be skipped
set tabstop=7

set expand-tab=false
set match-indent=false

map G G => my-prog
";

    // This should be our default so we are just verifying that we have not diverged from
    // what is in the repo.
    #[test]
    fn parse_of_example_config_works() {
        let cfg = Config::parse(EXAMPLE_CONFIG).unwrap();
        let bindings: BTreeMap<Vec<Input>, String> = [
            (vec![Input::Char(' '), Input::Char('F')], "fmt".to_string()),
            (vec![Input::Char('>')], "indent".to_string()),
            (vec![Input::Char('<')], "unindent".to_string()),
        ]
        .into_iter()
        .collect();

        let expected = Config {
            bindings,
            ..Default::default()
        };

        assert_eq!(cfg, expected);
    }

    #[test]
    fn custom_vals_work() {
        let cfg = Config::parse(CUSTOM_CONFIG).unwrap();

        let expected = Config {
            tabstop: 7,
            expand_tab: false,
            match_indent: false,
            bindings: [(
                vec![Input::Char('G'), Input::Char('G')],
                "my-prog".to_string(),
            )]
            .into_iter()
            .collect(),
            ..Default::default()
        };

        assert_eq!(cfg, expected);
    }
}
