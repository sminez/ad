//! A minimal config file format for ad

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub(crate) tabstop: usize,
    pub(crate) expand_tab: bool,
    pub(crate) match_indent: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            tabstop: 4,
            expand_tab: true,
            match_indent: true,
        }
    }
}

impl Config {
    /// Attempt to parse the given file content as a Config file. If the file is invalid then an
    /// error message for the user is returned for displaying in the status bar.
    pub fn parse(contents: &str) -> Result<Self, String> {
        let mut cfg = Config::default();

        for line in contents.lines() {
            let line = line.trim_end();

            if line.starts_with('#') || line.is_empty() {
                continue;
            }

            match line.strip_prefix("set ") {
                None => return Err(format!("'{line}' is not a 'set prop=val' command")),
                Some(line) => cfg.try_set_prop(line)?,
            }
        }

        Ok(cfg)
    }

    pub(crate) fn try_set_prop(&mut self, input: &str) -> Result<(), String> {
        let (prop, val) = match input.split_once('=') {
            None => return Err(format!("'{input}' is not a 'set prop=val' command")),
            Some(parts) => parts,
        };

        match prop {
            "tabstop" => {
                self.tabstop = match val.parse() {
                    Ok(num) => num,
                    Err(_) => return Err(format!("'{val}' is not valid tabstop")),
                };
            }

            "expand-tab" => match val {
                "true" => self.expand_tab = true,
                "false" => self.expand_tab = false,
                _ => return Err(format!("expand-tab: expected true/false, got '{val}'")),
            },

            "match-indent" => match val {
                "true" => self.match_indent = true,
                "false" => self.match_indent = false,
                _ => return Err(format!("match-indent: expected true/false, got '{val}'")),
            },

            _ => return Err(format!("'{prop}' is not a known config property")),
        }

        Ok(())
    }
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
";

    // This should be our default so we are just verifying that we have not diverged from
    // what is in the repo.
    #[test]
    fn parse_of_example_config_works() {
        let cfg = Config::parse(EXAMPLE_CONFIG).unwrap();

        let expected = Config {
            tabstop: 4,
            expand_tab: true,
            match_indent: true,
        };

        assert_eq!(cfg, expected);
    }

    #[test]
    fn custom_vals_for_all_props_works() {
        let cfg = Config::parse(CUSTOM_CONFIG).unwrap();

        let expected = Config {
            tabstop: 7,
            expand_tab: false,
            match_indent: false,
        };

        assert_eq!(cfg, expected);
    }
}
