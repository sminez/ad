//! A minimal config file format for ad
use crate::{
    buffer::Buffer,
    editor::{Action, Actions},
    key::Input,
    term::{Color, Styles},
    trie::Trie,
    util::parent_dir_containing,
};
use serde::{
    de::{self, DeserializeOwned, MapAccess, Visitor},
    Deserialize, Deserializer,
};
use std::{
    collections::HashMap, env, fmt, fs, io, iter::successors, marker::PhantomData, path::Path,
};
use tracing::{error, warn};

pub const DEFAULT_CONFIG: &str = include_str!("../data/config.toml");

pub const TK_DEFAULT: &str = "default";
pub const TK_DOT: &str = "dot";
pub const TK_LOAD: &str = "load";
pub const TK_EXEC: &str = "exec";

pub(crate) fn config_path() -> String {
    let home = env::var("HOME").unwrap();
    format!("{home}/.ad/config.toml")
}

/// Editor level configuration
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub show_splash: bool,
    pub tabstop: usize,
    pub expand_tab: bool,
    pub match_indent: bool,
    pub status_timeout: u64,
    pub double_click_ms: u64,
    pub minibuffer_lines: usize,
    pub find_command: String,

    #[serde(default)]
    pub filesystem: FsysConfig,
    #[serde(default, deserialize_with = "path_or_struct")]
    pub colorscheme: ColorScheme,
    #[serde(default)]
    pub tree_sitter: TsConfig,
    #[serde(default)]
    pub languages: Vec<LangConfig>,
    #[serde(default)]
    pub keys: KeyBindings,
}

impl Default for Config {
    fn default() -> Self {
        toml::from_str(DEFAULT_CONFIG).unwrap()
    }
}

impl Config {
    /// Attempt to load a config file from the default location
    pub fn try_load() -> Result<Self, String> {
        let home = env::var("HOME").unwrap();
        let path = config_path();

        let mut cfg = match fs::read_to_string(&path) {
            Ok(s) => match toml::from_str(&s) {
                Ok(cfg) => cfg,
                Err(e) => {
                    error!("invalid config file: {e}");
                    return Err(format!("Invalid config file: {e}"));
                }
            },

            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                if fs::create_dir_all(format!("{home}/.ad")).is_ok() {
                    if let Err(e) = fs::write(path, DEFAULT_CONFIG) {
                        error!("unable to write default config file: {e}");
                    }
                }

                Config::default()
            }

            Err(e) => return Err(format!("Unable to load config file: {e}")),
        };

        // Use default colorscheme's background color if none is specified
        for style in cfg.colorscheme.syntax.values_mut() {
            style.fg = style.fg.or(Some(cfg.colorscheme.fg));
            style.bg = style.bg.or(Some(cfg.colorscheme.bg));
        }

        // Replace "~/" shorthand notation in paths with the user's $HOME
        for s in [
            &mut cfg.tree_sitter.parser_dir,
            &mut cfg.tree_sitter.syntax_query_dir,
        ] {
            if s.starts_with("~/") {
                *s = s.replacen("~", &home, 1);
            }
        }

        Ok(cfg)
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
            .find(|c| {
                c.filenames.iter().any(|f| *f == fname)
                    || c.extensions.iter().any(|e| e == ext)
                    || c.first_lines.iter().any(|l| first_line.starts_with(l))
            })
            .map(|c| c.name.as_str())
    }

    pub(crate) fn update_from(&mut self, input: &str) -> Result<(), String> {
        warn!("ignoring runtime config update: {input}");

        Err("runtime config updates are not currently supported".to_owned())
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
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
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
        let bg: Color = "#1B1720".try_into().unwrap();
        let fg: Color = "#E6D29E".try_into().unwrap();
        let dot_bg: Color = "#336677".try_into().unwrap();
        let load_bg: Color = "#957FB8".try_into().unwrap();
        let exec_bg: Color = "#Bf616A".try_into().unwrap();
        let comment: Color = "#624354".try_into().unwrap();
        let constant: Color = "#FF9E3B".try_into().unwrap();
        let function: Color = "#957FB8".try_into().unwrap();
        let keyword: Color = "#Bf616A".try_into().unwrap();
        let module: Color = "#2D4F67".try_into().unwrap();
        let punctuation: Color = "#9CABCA".try_into().unwrap();
        let string: Color = "#61DCA5".try_into().unwrap();
        let type_: Color = "#7E9CD8".try_into().unwrap();
        let variable: Color = "#DCA561".try_into().unwrap();

        #[rustfmt::skip]
        let syntax = [
            (TK_DEFAULT,    Styles { fg: Some(fg), bg: Some(bg), ..Default::default() }),
            (TK_DOT,        Styles { fg: Some(fg), bg: Some(dot_bg), ..Default::default() }),
            (TK_LOAD,       Styles { fg: Some(fg), bg: Some(load_bg), ..Default::default() }),
            (TK_EXEC,       Styles { fg: Some(fg), bg: Some(exec_bg), ..Default::default() }),
            ("character",   Styles { fg: Some(string), bold: true, ..Default::default() }),
            ("comment",     Styles { fg: Some(comment), italic: true, ..Default::default() }),
            ("constant",    Styles { fg: Some(constant), ..Default::default() }),
            ("function",    Styles { fg: Some(function), ..Default::default() }),
            ("keyword",     Styles { fg: Some(keyword), ..Default::default() }),
            ("module",      Styles { fg: Some(module), ..Default::default() }),
            ("punctuation", Styles { fg: Some(punctuation), ..Default::default() }),
            ("string",      Styles { fg: Some(string), ..Default::default() }),
            ("type",        Styles { fg: Some(type_), ..Default::default() }),
            ("variable",    Styles { fg: Some(variable), ..Default::default() }),
        ]
        .map(|(s, v)| (s.to_string(), v))
        .into_iter()
        .collect();

        Self {
            bg,
            fg,
            bar_bg: "#4E415C".try_into().unwrap(),
            signcol_fg: "#544863".try_into().unwrap(),
            minibuffer_hl: "#3E3549".try_into().unwrap(),
            syntax,
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct LangConfig {
    pub name: String,
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
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
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

/// Helper for supporting specifying a path to an aditional file containing part of the config as
/// well as the contents of the config inline.
fn path_or_struct<'de, T, D>(deserializer: D) -> Result<T, D::Error>
where
    T: DeserializeOwned,
    D: Deserializer<'de>,
{
    struct StringOrStruct<T>(PhantomData<fn() -> T>);

    impl<'de, T> Visitor<'de> for StringOrStruct<T>
    where
        T: DeserializeOwned,
    {
        type Value = T;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("string or map")
        }

        fn visit_str<E: de::Error>(self, value: &str) -> Result<T, E> {
            let res = if value.starts_with("~/") {
                let home = env::var("HOME").map_err(|e| E::custom(e.to_string()))?;
                fs::read_to_string(value.replacen("~", &home, 1))
            } else {
                fs::read_to_string(value)
            };

            let raw = res.map_err(|e| E::custom(e.to_string()))?;
            toml::from_str(&raw).map_err(|e| E::custom(e.to_string()))
        }

        fn visit_map<M: MapAccess<'de>>(self, map: M) -> Result<T, M::Error> {
            Deserialize::deserialize(de::value::MapAccessDeserializer::new(map))
        }
    }

    deserializer.deserialize_any(StringOrStruct(PhantomData))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_loads() {
        Config::default(); // will panic if default config is invalid
    }
}
