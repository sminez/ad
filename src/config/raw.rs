//! The raw parsing structs for reading a config file from disk before validating it and converting
//! it into our internal data types.
use crate::{
    config::{
        ColorScheme, Config, DEFAULT_CONFIG, EditorConfig, FsysConfig, FtypeConfig, KeyBindings,
        LspConfig, TsConfig,
    },
    syntax::{TK_DEFAULT, TK_DOT, TK_EXEC, TK_LOAD},
    term::{Color, Styles},
};
use serde::{Deserialize, de::DeserializeOwned};
use std::{
    collections::HashMap,
    fs, io,
    path::{Path, PathBuf},
};
use toml::Table;

#[derive(Debug, PartialEq, Deserialize)]
pub(super) struct RawConfig {
    editor: Option<RawEditorConfig>,
    filesystem: Option<FsysConfig>,
    tree_sitter: Option<TsConfig>,
    colorscheme: Option<PathOrStruct<RawColorScheme>>,
    filetypes: Option<PathOrStruct<HashMap<String, RawLangConfig>>>,
    keys: Option<PathOrStruct<KeyBindings>>,
}

impl Default for RawConfig {
    fn default() -> Self {
        toml::from_str(DEFAULT_CONFIG).unwrap()
    }
}

impl RawConfig {
    /// Resolve the config that we need for running the editor and report any errors that
    /// resulted in defaults being used.
    pub(super) fn resolve(self, config_path: &str, home: &str) -> Result<Config, String> {
        let mut errs = Vec::new();
        let phome = PathBuf::from(home);
        let config_path = PathBuf::from(config_path);
        let config_dir = config_path.parent().unwrap();

        let editor = self.editor.map(|raw| raw.resolve()).unwrap_or_default();
        let filesystem = self.filesystem.unwrap_or_default();
        let tree_sitter = self.tree_sitter.unwrap_or_default();
        let colorscheme = self
            .colorscheme
            .unwrap_or_default()
            .into_inner(config_dir, &phome, &mut errs)
            .resolve(&mut errs);
        let raw_filetypes = self
            .filetypes
            .map(|pos| pos.into_inner(config_dir, &phome, &mut errs))
            .unwrap_or_default();

        let filetypes = raw_filetypes
            .into_iter()
            .map(|(ftype, raw)| {
                let conf = raw.resolve(config_dir, &phome, &mut errs);
                (ftype, conf)
            })
            .collect();

        let keys = self
            .keys
            .map(|pos| pos.into_inner(config_dir, &phome, &mut errs))
            .unwrap_or_default();

        if !errs.is_empty() {
            return Err(errs.join("\n"));
        }

        let mut cfg = Config {
            editor,
            filesystem,
            tree_sitter,
            colorscheme,
            filetypes,
            keys,
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
                *s = s.replacen("~", home, 1);
            }
        }

        Ok(cfg)
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
struct RawEditorConfig {
    show_splash: Option<bool>,
    tabstop: Option<usize>,
    expand_tab: Option<bool>,
    match_indent: Option<bool>,
    lsp_autostart: Option<bool>,
    status_timeout: Option<u64>,
    double_click_ms: Option<u64>,
    minibuffer_lines: Option<usize>,
    find_command: Option<String>,
}

macro_rules! set_if_some {
    ($field:ident, $raw:ident, $cfg:ident) => {
        if let Some(val) = $raw.$field {
            $cfg.$field = val;
        }
    };
}

impl RawEditorConfig {
    fn resolve(self) -> EditorConfig {
        let mut cfg = EditorConfig::default();
        set_if_some!(show_splash, self, cfg);
        set_if_some!(tabstop, self, cfg);
        set_if_some!(expand_tab, self, cfg);
        set_if_some!(match_indent, self, cfg);
        set_if_some!(lsp_autostart, self, cfg);
        set_if_some!(status_timeout, self, cfg);
        set_if_some!(double_click_ms, self, cfg);
        set_if_some!(minibuffer_lines, self, cfg);
        set_if_some!(find_command, self, cfg);

        cfg
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub(super) struct RawColorScheme {
    #[serde(default)]
    palette: HashMap<String, Color>,
    bg: String,
    fg: String,
    bar_bg: String,
    signcol_fg: String,
    minibuffer_hl: String,
    syntax: HashMap<String, RawStyles>,
}

impl Default for RawColorScheme {
    fn default() -> Self {
        let bg = "#1B1720".to_string();
        let fg = "#E6D29E".to_string();
        let dot_bg = "#336677".to_string();
        let load_bg = "#957FB8".to_string();
        let exec_bg = "#Bf616A".to_string();
        let comment = "#624354".to_string();
        let constant = "#FF9E3B".to_string();
        let function = "#957FB8".to_string();
        let keyword = "#Bf616A".to_string();
        let module = "#2D4F67".to_string();
        let punctuation = "#9CABCA".to_string();
        let string = "#61DCA5".to_string();
        let type_ = "#7E9CD8".to_string();
        let variable = "#DCA561".to_string();

        #[rustfmt::skip]
        let syntax = [
            (TK_DEFAULT,    RawStyles { fg: Some(fg.clone()), bg: Some(bg.clone()), ..Default::default() }),
            (TK_DOT,        RawStyles { fg: Some(fg.clone()), bg: Some(dot_bg), ..Default::default() }),
            (TK_LOAD,       RawStyles { fg: Some(fg.clone()), bg: Some(load_bg), ..Default::default() }),
            (TK_EXEC,       RawStyles { fg: Some(fg.clone()), bg: Some(exec_bg), ..Default::default() }),
            ("character",   RawStyles { fg: Some(string.clone()), bold: true, ..Default::default() }),
            ("comment",     RawStyles { fg: Some(comment), italic: true, ..Default::default() }),
            ("constant",    RawStyles { fg: Some(constant), ..Default::default() }),
            ("function",    RawStyles { fg: Some(function), ..Default::default() }),
            ("keyword",     RawStyles { fg: Some(keyword), ..Default::default() }),
            ("module",      RawStyles { fg: Some(module), ..Default::default() }),
            ("punctuation", RawStyles { fg: Some(punctuation), ..Default::default() }),
            ("string",      RawStyles { fg: Some(string), ..Default::default() }),
            ("type",        RawStyles { fg: Some(type_), ..Default::default() }),
            ("variable",    RawStyles { fg: Some(variable), ..Default::default() }),
        ]
        .map(|(s, v)| (s.to_string(), v))
        .into_iter()
        .collect();

        Self {
            palette: Default::default(),
            bg,
            fg,
            bar_bg: "#4E415C".to_string(),
            signcol_fg: "#544863".to_string(),
            minibuffer_hl: "#3E3549".to_string(),
            syntax,
        }
    }
}

impl RawColorScheme {
    /// Attempt to map all palette references into colors and parse raw hex color strings.
    /// The concatenation of all encountered errors are returned in the event of there being
    /// any parse errors rather than short circuiting at the first error.
    pub(super) fn resolve(self, errs: &mut Vec<String>) -> ColorScheme {
        ColorScheme {
            bg: try_color(&self.bg, &self.palette, errs),
            fg: try_color(&self.fg, &self.palette, errs),
            bar_bg: try_color(&self.bar_bg, &self.palette, errs),
            signcol_fg: try_color(&self.signcol_fg, &self.palette, errs),
            minibuffer_hl: try_color(&self.minibuffer_hl, &self.palette, errs),
            syntax: self
                .syntax
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        Styles {
                            fg: v.fg.map(|s| try_color(&s, &self.palette, errs)),
                            bg: v.bg.map(|s| try_color(&s, &self.palette, errs)),
                            bold: v.bold,
                            italic: v.italic,
                            underline: v.underline,
                        },
                    )
                })
                .collect(),
        }
    }
}

fn try_color(s: &String, palette: &HashMap<String, Color>, errs: &mut Vec<String>) -> Color {
    if let Some(color) = palette.get(s) {
        return *color;
    }

    match Color::try_from(s.as_ref()) {
        Ok(color) => color,
        Err(e) => {
            errs.push(e);
            Color::default()
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Deserialize)]
struct RawStyles {
    #[serde(default)]
    fg: Option<String>,
    #[serde(default)]
    bg: Option<String>,
    #[serde(default)]
    bold: bool,
    #[serde(default)]
    italic: bool,
    #[serde(default)]
    underline: bool,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Deserialize)]
pub struct RawLangConfig {
    #[serde(default)]
    pub extensions: Vec<String>,
    #[serde(default)]
    pub first_lines: Vec<String>,
    #[serde(default)]
    pub filenames: Vec<String>,
    #[serde(default)]
    pub re_syntax: Option<String>,
    #[serde(default)]
    pub lsp: Option<LspConfig>,
}

impl RawLangConfig {
    fn resolve(self, config_path: &Path, home: &Path, errs: &mut Vec<String>) -> FtypeConfig {
        let re_syntax = match self.re_syntax {
            None => Vec::new(),
            Some(path) => match try_read::<Table>(&path, config_path, home) {
                Err(e) => {
                    errs.push(format!("  {path}: {e}"));
                    Vec::new()
                }
                Ok(table) => {
                    let mut pairs = Vec::with_capacity(table.len());
                    for (k, v) in table.into_iter() {
                        match v.as_str() {
                            Some(s) => pairs.push((k, s.to_string())),
                            None => {
                                errs.push(format!(
                                    "  invalid re syntax for key {k}: value must be a string"
                                ));
                                pairs.clear();
                                break;
                            }
                        }
                    }

                    pairs
                }
            },
        };

        FtypeConfig {
            extensions: self.extensions,
            first_lines: self.first_lines,
            filenames: self.filenames,
            re_syntax,
            lsp: self.lsp,
        }
    }
}

/// Helper for allowing users to specify a path to an alternate config file for a given section
/// rather than providing it inline.
#[derive(Debug, PartialEq, Deserialize)]
#[serde(untagged)]
pub(super) enum PathOrStruct<T> {
    Path(String),
    Struct(T),
}

impl<T> Default for PathOrStruct<T>
where
    T: Default,
{
    fn default() -> Self {
        Self::Struct(Default::default())
    }
}

impl<T> PathOrStruct<T>
where
    T: Default + DeserializeOwned,
{
    fn into_inner(self, config_path: &Path, home: &Path, errs: &mut Vec<String>) -> T {
        let path = match self {
            Self::Struct(t) => return t,
            Self::Path(p) => p,
        };

        match try_read(&path, config_path, home) {
            Ok(t) => t,
            Err(e) => {
                errs.push(format!("  {path}: {e}"));
                T::default()
            }
        }
    }
}

fn try_read<T>(raw: &str, config_path: &Path, home: &Path) -> io::Result<T>
where
    T: Default + DeserializeOwned,
{
    let mut path = match raw.strip_prefix("~/") {
        Some(tail) => home.join(tail),
        None => PathBuf::from(raw),
    };
    if path.is_relative() {
        path = config_path.join(path);
    }
    let content = fs::read_to_string(path)?;

    toml::from_str(&content).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::dir_cases;

    #[dir_cases("data/config_tests/valid")]
    #[test]
    fn valid_config_parses(path: &str, content: &str) {
        let raw: RawConfig = toml::from_str(content).unwrap();

        // The test cases should all be attempting to set these keys
        assert!(raw.colorscheme.is_some(), "no colorscheme set");
        assert!(raw.filetypes.is_some(), "no filetypes set");
        assert!(raw.keys.is_some(), "no keys set");

        let res = raw.resolve(path, "");
        assert!(res.is_ok(), "{path} {res:?}");
    }

    #[dir_cases("data/colorschemes")]
    #[test]
    fn colorschemes_parse(path: &str, content: &str) {
        let raw: RawColorScheme = toml::from_str(content).unwrap();
        let mut errs = Vec::new();
        let _cs = raw.resolve(&mut errs);
        assert!(errs.is_empty(), "{path} {errs:?}");
    }
}
