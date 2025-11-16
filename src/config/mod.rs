//! A minimal config file format for ad
use crate::{
    buffer::Buffer,
    editor::{Action, Actions},
    key::{Arrow, Input},
    syntax::TK_DEFAULT,
    term::{Color, Styles},
    trie::Trie,
    util::parent_dir_containing,
};
use serde::{Deserialize, Deserializer, de};
use std::{collections::HashMap, env, fs, iter::successors, ops::Deref, path::Path};
use tracing::error;

mod raw;

use raw::{RawColorScheme, RawConfig};

pub const DEFAULT_CONFIG: &str = include_str!("../../data/config.toml");

pub(crate) fn config_path() -> String {
    let home = env::var("HOME").unwrap();
    format!("{home}/.ad/config.toml")
}

/// Editor level configuration
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub editor: EditorConfig,
    pub filesystem: FsysConfig,
    pub tree_sitter: TsConfig,
    pub colorscheme: ColorScheme,
    pub filetypes: HashMap<String, FtypeConfig>,
    pub keys: KeyBindings,
}

impl Default for Config {
    fn default() -> Self {
        RawConfig::default()
            .resolve(&config_path(), "")
            .expect("default config is broken")
    }
}

impl Deref for Config {
    type Target = EditorConfig;

    fn deref(&self) -> &Self::Target {
        &self.editor
    }
}

impl Config {
    /// Attempt to load a config file from a specified location.
    /// If there are any errors while loading and parsing the file then they
    /// are reported as a formatted string for displaying to the user.
    pub fn try_load_from_path(path: &str, home: &str) -> Result<Self, String> {
        match fs::read_to_string(path) {
            Ok(s) => Self::try_load_from_str(&s, path, home),
            Err(e) => Err(format!("unable to load config file: {e}")),
        }
    }

    /// Attempt to load a config file from its raw file contents as a string.
    /// If there are any errors while loading and parsing the file then they
    /// are reported as a formatted string for displaying to the user.
    pub fn try_load_from_str(s: &str, path: &str, home: &str) -> Result<Self, String> {
        let raw: RawConfig = match toml::from_str(s) {
            Ok(cfg) => cfg,
            Err(e) => {
                error!("malformed config file: {e}");
                return Err(format!("malformed config file: {e}"));
            }
        };
        let res = raw.resolve(path, home);
        if let Err(err) = res.as_ref() {
            error!("malformed config file: {err}");
        }

        res
    }

    /// Attempt to load a config file from the default location.
    ///
    /// If the config file doesn't currently exist then the default config
    /// will be written to disk.
    /// If there are any errors while loading and parsing the file then they
    /// are reported as a formatted string for displaying to the user.
    pub fn try_load() -> Result<Self, String> {
        let home = env::var("HOME").unwrap();
        let path = config_path();

        if matches!(fs::exists(&path), Ok(false))
            && fs::create_dir_all(format!("{home}/.ad")).is_ok()
            && let Err(e) = fs::write(&path, DEFAULT_CONFIG)
        {
            error!("unable to write default config file: {e}");
        }

        Self::try_load_from_path(&path, &home)
    }
}

/// For an explicitly provided path and first line of a file, check to see if we know the
/// correct language associated with the file and associated [FtypeConfig].
pub fn ftype_config_for_path_and_first_line<'a>(
    path: &Path,
    first_line: &str,
    filetypes: &'a HashMap<String, FtypeConfig>,
) -> Option<(&'a String, &'a FtypeConfig)> {
    let fname = path.file_name()?.to_string_lossy();
    let os_ext = path.extension().unwrap_or_default();
    let ext = os_ext.to_str().unwrap_or_default();

    filetypes.iter().find(|(_, c)| {
        c.filenames.iter().any(|f| *f == fname)
            || c.extensions.iter().any(|e| e == ext)
            || c.first_lines.iter().any(|l| first_line.starts_with(l))
    })
}

/// Top level configuration for the editor
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EditorConfig {
    pub show_splash: bool,
    pub tabstop: usize,
    pub expand_tab: bool,
    pub match_indent: bool,
    pub lsp_autostart: bool,
    pub status_timeout: u64,
    pub double_click_ms: u64,
    pub minibuffer_lines: usize,
    pub find_command: String,
}

impl Default for EditorConfig {
    fn default() -> Self {
        Self {
            show_splash: true,
            tabstop: 4,
            expand_tab: true,
            match_indent: true,
            lsp_autostart: true,
            status_timeout: 3,
            double_click_ms: 200,
            minibuffer_lines: 8,
            find_command: "fd --hidden".to_string(),
        }
    }
}

/// Configuration for the 9p filesystem interface
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct FsysConfig {
    pub enabled: bool,
    pub auto_mount: bool,
}

impl Default for FsysConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            auto_mount: false,
        }
    }
}

/// A colorscheme for rendering the UI.
///
/// UI elements are available as properties and syntax stylings are available as a map of string
/// tag to [Styles] that should be applied.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ColorScheme {
    pub bg: Color,
    pub fg: Color,
    pub bar_bg: Color,
    pub signcol_fg: Color,
    pub minibuffer_hl: Color,
    pub syntax: HashMap<String, Styles>,
}

impl Default for ColorScheme {
    fn default() -> Self {
        RawColorScheme::default().resolve(&mut Vec::new())
    }
}

impl ColorScheme {
    /// Determine UI [Styles] to be applied for a given syntax tag.
    ///
    /// If the full tag does not have associated styling but its dotted prefix does then the
    /// styling of the prefix is used, otherwise default styling will be used ([TK_DEFAULT]).
    ///
    /// For key "foo.bar.baz" this will return the first value found out of the following keyset:
    ///   - "foo.bar.baz"
    ///   - "foo.bar"
    ///   - "foo"
    ///   - [TK_DEFAULT]
    pub fn styles_for(&self, tag: &str) -> &Styles {
        successors(Some(tag), |s| Some(s.rsplit_once('.')?.0))
            .find_map(|k| self.syntax.get(k))
            .or(self.syntax.get(TK_DEFAULT))
            .expect("to have default styles")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct TsConfig {
    pub parser_dir: String,
    pub syntax_query_dir: String,
}

impl Default for TsConfig {
    fn default() -> Self {
        let home = env::var("HOME").unwrap();

        TsConfig {
            parser_dir: format!("{home}/.ad/tree-sitter/parsers"),
            syntax_query_dir: format!("{home}/.ad/tree-sitter/queries"),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Deserialize)]
pub struct FtypeConfig {
    #[serde(default)]
    pub extensions: Vec<String>,
    #[serde(default)]
    pub first_lines: Vec<String>,
    #[serde(default)]
    pub filenames: Vec<String>,
    #[serde(default)]
    pub re_syntax: Vec<(String, String)>,
    #[serde(default)]
    pub lsp: Option<LspConfig>,
}

/// Configuration for running a given language server
#[derive(Debug, Default, Clone, PartialEq, Eq, Deserialize)]
pub struct LspConfig {
    /// The command to run to start the language server
    pub command: String,
    /// Additional arguments to pass to the language server command
    #[serde(default)]
    pub args: Vec<String>,
    /// Files or directories to search for in order to determine the project root
    pub roots: Vec<String>,
    /// Additional initialization options to be passed when the server is started
    #[serde(default)]
    pub init_opts: Option<serde_json::Value>,
}

impl LspConfig {
    pub fn root_for_dir<'a>(&self, d: &'a Path) -> Option<&'a Path> {
        for root in self.roots.iter() {
            if let Some(p) = parent_dir_containing(d, root) {
                return Some(p);
            }
        }

        None
    }

    pub fn root_for_buffer<'a>(&self, b: &'a Buffer) -> Option<&'a Path> {
        self.root_for_dir(b.dir()?)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct KeyBindings {
    #[serde(default, deserialize_with = "de_serde_trie")]
    pub normal: Trie<Input, Actions>,
    #[serde(default, deserialize_with = "de_serde_trie")]
    pub insert: Trie<Input, Actions>,
}

impl Default for KeyBindings {
    fn default() -> Self {
        KeyBindings {
            normal: Trie::try_from_iter(Vec::new()).unwrap(),
            insert: Trie::try_from_iter(Vec::new()).unwrap(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(untagged)]
pub enum KeyAction {
    Execute { run: String },
    Keys { send_keys: Inputs },
}

impl KeyAction {
    fn into_actions(self) -> Actions {
        match self {
            Self::Execute { run } => Actions::Single(Action::ExecuteString { s: run }),
            Self::Keys { send_keys } => Actions::Single(Action::SendKeys { ks: send_keys.0 }),
        }
    }
}

/// Raw inputs to be sent through to the main editor event loop
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(try_from = "String")]
pub struct Inputs(pub(crate) Vec<Input>);

impl TryFrom<String> for Inputs {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let inputs = value
            .split_whitespace()
            .map(try_input_from_str_template)
            .collect::<Result<Vec<_>, _>>()?;

        if inputs.is_empty() {
            return Err("empty key inputs string".to_owned());
        }

        Ok(Self(inputs))
    }
}

/// Only supporting a subset of inputs for now
fn try_input_from_str_template(s: &str) -> Result<Input, String> {
    if s.len() == 1 {
        Ok(Input::Char(s.chars().next().unwrap()))
    } else if let Some(suffix) = s.strip_prefix("C-A-") {
        if suffix.len() == 1 {
            Ok(Input::CtrlAlt(suffix.chars().next().unwrap()))
        } else if suffix == "<space>" {
            Ok(Input::CtrlAlt(' '))
        } else {
            Err(format!("invalid send_key value: C-A-{suffix}"))
        }
    } else if let Some(suffix) = s.strip_prefix("C-") {
        if suffix.len() == 1 {
            Ok(Input::Ctrl(suffix.chars().next().unwrap()))
        } else if suffix == "<space>" {
            Ok(Input::Ctrl(' '))
        } else {
            Err(format!("invalid send_key value: C-{suffix}"))
        }
    } else if let Some(suffix) = s.strip_prefix("A-") {
        if suffix.len() == 1 {
            Ok(Input::Alt(suffix.chars().next().unwrap()))
        } else if suffix == "<space>" {
            Ok(Input::Alt(' '))
        } else {
            Err(format!("invalid send_key value: A-{suffix}"))
        }
    } else {
        let i = match s {
            "<backspace>" => Input::Backspace,
            "<delete>" => Input::Del,
            "<end>" => Input::End,
            "<esc>" => Input::Esc,
            "<home>" => Input::Home,
            "<page-down>" => Input::PageDown,
            "<page-up>" => Input::PageUp,
            "<space>" => Input::Char(' '),
            "<tab>" => Input::Tab,
            "<left>" => Input::Arrow(Arrow::Left),
            "<right>" => Input::Arrow(Arrow::Right),
            "<up>" => Input::Arrow(Arrow::Up),
            "<down>" => Input::Arrow(Arrow::Down),
            _ => return Err(format!("unknown key {s}")),
        };

        Ok(i)
    }
}

fn de_serde_trie<'de, D>(deserializer: D) -> Result<Trie<Input, Actions>, D::Error>
where
    D: Deserializer<'de>,
{
    let raw_map: HashMap<String, KeyAction> = Deserialize::deserialize(deserializer)?;
    if raw_map.is_empty() {
        return Err(de::Error::custom("empty key map"));
    }

    let raw = raw_map
        .into_iter()
        .map(|(k, action)| {
            Inputs::try_from(k)
                .map(|Inputs(keys)| (keys, action.into_actions()))
                .map_err(de::Error::custom)
        })
        .collect::<Result<Vec<_>, _>>()?;

    Trie::try_from_iter(raw).map_err(de::Error::custom)
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test]
    fn default_config_is_valid() {
        Config::default(); // will panic if default config is invalid
    }

    #[test_case("A", &[Input::Char('A')]; "single letter key")]
    #[test_case("5", &[Input::Char('5')]; "single digit key")]
    #[test_case("A-y", &[Input::Alt('y')]; "alt letter")]
    #[test_case("C-y", &[Input::Ctrl('y')]; "control letter")]
    #[test_case("C-A-y", &[Input::CtrlAlt('y')]; "control alt letter")]
    #[test_case("A-<space>", &[Input::Alt(' ')]; "alt space")]
    #[test_case("C-<space>", &[Input::Ctrl(' ')]; "control space")]
    #[test_case("C-A-<space>", &[Input::CtrlAlt(' ')]; "control alt space")]
    #[test_case("<backspace>", &[Input::Backspace]; "backspace")]
    #[test_case("<delete>", &[Input::Del]; "delete")]
    #[test_case("<end>", &[Input::End]; "end")]
    #[test_case("<esc>", &[Input::Esc]; "escape")]
    #[test_case("<home>", &[Input::Home]; "home")]
    #[test_case("<page-up>", &[Input::PageUp]; "page up")]
    #[test_case("<page-down>", &[Input::PageDown]; "page down")]
    #[test_case("<space>", &[Input::Char(' ')]; "space")]
    #[test_case("<tab>", &[Input::Tab]; "tab")]
    #[test_case("<left>", &[Input::Arrow(Arrow::Left)]; "left")]
    #[test_case("<right>", &[Input::Arrow(Arrow::Right)]; "right")]
    #[test_case("<up>", &[Input::Arrow(Arrow::Up)]; "up")]
    #[test_case("<down>", &[Input::Arrow(Arrow::Down)]; "down")]
    #[test_case("A B C", &[Input::Char('A'), Input::Char('B'), Input::Char('C')]; "sequence")]
    #[test]
    fn inputs_try_from_string_works(raw: &str, expected: &[Input]) {
        let inputs = Inputs::try_from(raw.to_owned()).unwrap();
        assert_eq!(&inputs.0, expected);
    }

    #[test]
    fn inputs_try_from_empty_string_errors() {
        assert_eq!(
            &Inputs::try_from(String::new()).unwrap_err(),
            "empty key inputs string"
        );
    }

    #[test]
    fn parsing_an_empty_keymap_errors() {
        let res: Result<KeyBindings, _> = toml::from_str("[normal]");
        assert!(res.is_err());
    }
}
