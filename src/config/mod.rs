//! A minimal config file format for ad
use crate::{
    buffer::Buffer,
    editor::{Action, Actions},
    key::Input,
    term::{Color, Styles},
    trie::Trie,
    ts::TK_DEFAULT,
    util::parent_dir_containing,
};
use serde::{de, Deserialize, Deserializer};
use std::{collections::HashMap, env, fs, io, iter::successors, ops::Deref, path::Path};
use tracing::{error, warn};

mod raw;

use raw::{RawConfig, RawColorScheme};

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
    pub languages: HashMap<String, LangConfig>,
    pub keys: KeyBindings,
}

impl Default for Config {
    fn default() -> Self {
        let (cfg, errs) = RawConfig::default().resolve("");
        assert!(errs.is_none(), "default config is broken");

        cfg
    }
}

impl Deref for Config {
    type Target = EditorConfig;

    fn deref(&self) -> &Self::Target {
        &self.editor
    }
}

impl Config {
    /// Attempt to load a config file from the default location
    pub fn try_load() -> Result<Self, String> {
        let home = env::var("HOME").unwrap();
        let path = config_path();

        match fs::read_to_string(&path) {
            Ok(s) => {
                let raw: RawConfig = match toml::from_str(&s) {
                    Ok(cfg) => cfg,
                    Err(e) => {
                        error!("malformed config file: {e}");
                        return Err(format!("Malformed config file: {e}"));
                    }
                };
                let (cfg, err) = raw.resolve(&home);
                if let Some(err) = err {
                    error!("malformed config: {err}");
                }

                Ok(cfg)
            }

            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                if fs::create_dir_all(format!("{home}/.ad")).is_ok() {
                    if let Err(e) = fs::write(path, DEFAULT_CONFIG) {
                        error!("unable to write default config file: {e}");
                    }
                }

                Ok(Config::default())
            }

            Err(e) => return Err(format!("Unable to load config file: {e}")),
        }
    }

    /// Check to see if there is a known tree-sitter configuration for this buffer
    pub fn ts_lang_for_buffer(&self, b: &Buffer) -> Option<&str> {
        let path = b.path()?;
        let fname = path.file_name()?.to_string_lossy();
        let os_ext = path.extension().unwrap_or_default();
        let ext = os_ext.to_str().unwrap_or_default();
        let first_line = b.line(0).map(|l| l.to_string()).unwrap_or_default();

        self.languages
            .iter()
            .find(|(_, c)| {
                c.filenames.iter().any(|f| *f == fname)
                    || c.extensions.iter().any(|e| e == ext)
                    || c.first_lines.iter().any(|l| first_line.starts_with(l))
            })
            .map(|(name, _)| name.as_str())
    }

    pub(crate) fn update_from(&mut self, input: &str) -> Result<(), String> {
        warn!("ignoring runtime config update: {input}");

        Err("runtime config updates are not currently supported".to_owned())
    }
}

/// Top level configuration for the editor
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EditorConfig {
    pub show_splash: bool,
    pub tabstop: usize,
    pub expand_tab: bool,
    pub match_indent: bool,
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
            status_timeout: 3,
            double_click_ms: 200,
            minibuffer_lines: 8,
            find_command: "fd -t f".to_string(),
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
/// tag to [Style]s that should be applied.
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
pub struct LangConfig {
    #[serde(default)]
    pub extensions: Vec<String>,
    #[serde(default)]
    pub first_lines: Vec<String>,
    #[serde(default)]
    pub filenames: Vec<String>,
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
    pub fn root_for_buffer<'a>(&self, b: &'a Buffer) -> Option<&'a Path> {
        let d = b.dir()?;
        for root in self.roots.iter() {
            if let Some(p) = parent_dir_containing(d, root) {
                return Some(p);
            }
        }

        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct KeyBindings {
    #[serde(default, deserialize_with = "de_serde_trie")]
    pub normal: Trie<Input, KeyAction>,
    #[serde(default, deserialize_with = "de_serde_trie")]
    pub insert: Trie<Input, KeyAction>,
}

impl Default for KeyBindings {
    fn default() -> Self {
        KeyBindings {
            normal: Trie::from_pairs(Vec::new()).unwrap(),
            insert: Trie::from_pairs(Vec::new()).unwrap(),
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
    pub fn as_actions(&self) -> Actions {
        match self {
            Self::Execute { run } => Actions::Single(Action::ExecuteString { s: run.clone() }),
            Self::Keys { send_keys } => Actions::Single(Action::SendKeys {
                ks: send_keys.0.clone(),
            }),
        }
    }
}

/// Raw inputs to be sent through to the main editor event loop
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(try_from = "String")]
pub struct Inputs(Vec<Input>);

impl TryFrom<String> for Inputs {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let mut inputs = Vec::new();

        for s in value.split_whitespace() {
            inputs.push(Input::try_from_str_template(s)?);
        }

        Ok(Self(inputs))
    }
}

fn de_serde_trie<'de, D>(deserializer: D) -> Result<Trie<Input, KeyAction>, D::Error>
where
    D: Deserializer<'de>,
{
    let raw_map: HashMap<String, KeyAction> = Deserialize::deserialize(deserializer)?;
    let mut raw = Vec::with_capacity(raw_map.len());

    for (k, action) in raw_map.into_iter() {
        let keys: Vec<Input> = k
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

        raw.push((keys, action));
    }

    Trie::from_pairs(raw).map_err(de::Error::custom)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_loads() {
        Config::default(); // will panic if default config is invalid
    }
}
