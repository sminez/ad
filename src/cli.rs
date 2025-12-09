//! CLI parser
//! See main.rs for the usage of the parsed arguments
use lexopt::{Parser, prelude::*};
use std::{fs, path::PathBuf};

pub const USAGE: &str = "\
usage: ad [options] [file ...]     Edit file(s)

options:
  -e, --expression <script>        Execute edit script on file(s)
  -f, --script-file <script-file>  Execute edit script loaded from a script-file on file(s)
  -9p [-A aname] read [path]       Read the contents of a file on a 9p file server
  -9p [-A aname] write [path]      Write the contents of stdin to a file on a 9p file server
  -9p [-A aname] ls [path]         List the contents of a directory on a 9p file server
  -l, --list-sessions              List the current open editor 9p sessions
  --rm-sockets                     Remove all unresponsive ad 9p sockets from the default namespace directory
  -c, --config <path>              Load config from the specified path
  --default-config                 Force default config instead of loading the user config file
  -h, --help                       Print this help message
  -v, --version                    Print version information
";

#[derive(Debug)]
pub enum CliAction {
    OpenEditor {
        files: Vec<PathBuf>,
    },
    RunScript {
        script: String,
        files: Vec<PathBuf>,
    },
    NineP {
        aname: String,
        cmd: Cmd9p,
        path: String,
    },
    ListSessions,
    RmSockets,
    ShowHelp,
    ShowVersion,
}

#[derive(Debug)]
pub enum Cmd9p {
    Read,
    Write,
    List,
}

#[derive(Debug)]
pub struct ParsedArgs {
    pub action: CliAction,
    pub config_source: ConfigSource,
}

#[derive(Debug)]
pub enum ConfigSource {
    Default,
    User,
    Custom(PathBuf),
}

impl ParsedArgs {
    pub fn try_parse() -> Result<Self, String> {
        let mut parser = Parser::from_env();

        Self::try_from_parser(&mut parser).map_err(|e| e.to_string())
    }

    fn try_from_parser(parser: &mut Parser) -> Result<Self, lexopt::Error> {
        let mut action: Option<CliAction> = None;
        let mut config_source = ConfigSource::User;
        let mut config_set = false;

        loop {
            // If we've already parsed a valid action and there are arguments remaining then the
            // command line as a whole is invalid.
            if action.is_some()
                && let Some(raw_args) = parser.try_raw_args()
            {
                match raw_args.peek() {
                    Some(arg) => return Err(lexopt::Error::UnexpectedArgument(arg.to_os_string())),
                    None => break,
                }
            }

            if let Some(true) = is_9p_option(parser) {
                action = Some(parse_9p(parser)?);
                continue;
            }

            match parser.next()? {
                Some(arg) => match arg {
                    Short('e') | Long("expression") => {
                        let script = parser
                            .value()?
                            .into_string()
                            .map_err(lexopt::Error::NonUnicodeValue)?;
                        let files: Vec<PathBuf> = match parser.values() {
                            Ok(vals) => vals.map(PathBuf::from).collect(),
                            Err(_) => Vec::new(),
                        };
                        action = Some(CliAction::RunScript { script, files });
                    }

                    Short('f') | Long("script-file") => {
                        let fname = parser.value()?;
                        match fs::read_to_string(&fname) {
                            Ok(script) => {
                                let files: Vec<PathBuf> = match parser.values() {
                                    Ok(vals) => vals.map(PathBuf::from).collect(),
                                    Err(_) => Vec::new(),
                                };
                                action = Some(CliAction::RunScript { script, files });
                            }

                            Err(e) => {
                                return Err(lexopt::Error::from(format!(
                                    "unable to load script file from {}: {e}",
                                    fname.to_string_lossy()
                                )));
                            }
                        }
                    }

                    Short('l') | Long("list-sessions") => action = Some(CliAction::ListSessions),
                    Short('v') | Long("version") => action = Some(CliAction::ShowVersion),
                    Short('h') | Long("help") => action = Some(CliAction::ShowHelp),
                    Long("rm-sockets") => action = Some(CliAction::RmSockets),

                    Short('c') | Long("config") => {
                        if config_set {
                            return Err(lexopt::Error::from("config source already specified"));
                        }
                        let path = PathBuf::from(parser.value()?);
                        if !path.exists() {
                            return Err(lexopt::Error::from(format!(
                                "config path does not exist: {}",
                                path.display()
                            )));
                        } else if !path.is_file() {
                            return Err(lexopt::Error::from(format!(
                                "config path is not a file: {}",
                                path.display()
                            )));
                        }
                        config_source = ConfigSource::Custom(path);
                        config_set = true;
                    }

                    Long("default-config") => {
                        if config_set {
                            return Err(lexopt::Error::from("config source already specified"));
                        }
                        config_source = ConfigSource::Default;
                        config_set = true;
                    }

                    Value(fname) if action.is_none() => {
                        let files: Vec<PathBuf> = match parser.values() {
                            Ok(vals) => std::iter::once(fname)
                                .chain(vals)
                                .map(PathBuf::from)
                                .collect(),
                            Err(_) => vec![PathBuf::from(fname)],
                        };

                        action = Some(CliAction::OpenEditor { files });
                    }

                    _ => return Err(arg.unexpected()),
                },

                None => break,
            }
        }

        Ok(ParsedArgs {
            action: action.unwrap_or_else(|| CliAction::OpenEditor { files: Vec::new() }),
            config_source,
        })
    }
}

fn is_9p_option(parser: &mut Parser) -> Option<bool> {
    let mut raw = parser.try_raw_args()?;
    let arg = raw.peek()?.to_str()?;

    if arg == "-9p" {
        raw.next(); // consume the -9p arg
        Some(true)
    } else {
        Some(false)
    }
}

fn parse_9p(parser: &mut Parser) -> Result<CliAction, lexopt::Error> {
    let arg = parser.next()?.ok_or(lexopt::Error::MissingValue {
        option: Some("9p".into()),
    })?;

    let mut aname = String::new();

    let next = match arg {
        Short('A') => {
            aname = parser
                .value()?
                .into_string()
                .map_err(lexopt::Error::NonUnicodeValue)?;
            parser.next()?
        }
        Value(val) => Some(Value(val)),
        _ => return Err(arg.unexpected()),
    };

    let cmd = match next {
        Some(arg) => match arg {
            Value(cmd) => match cmd.to_str() {
                Some("read") => Cmd9p::Read,
                Some("write") => Cmd9p::Write,
                Some("ls") => Cmd9p::List,
                _ => return Err(Value(cmd).unexpected()),
            },
            _ => return Err(arg.unexpected()),
        },
        None => return Err(lexopt::Error::from("no command provided for -9p")),
    };

    let path = match parser.next()? {
        Some(Value(s)) => s.into_string().map_err(lexopt::Error::NonUnicodeValue)?,
        Some(arg) => return Err(arg.unexpected()),
        None => return Err(lexopt::Error::from("no path provided for -9p")),
    };

    Ok(CliAction::NineP { aname, cmd, path })
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test_case(""; "no args at all")]
    #[test_case("foo.txt"; "single file")]
    #[test_case("foo.txt bar.json"; "multiple files")]
    #[test_case("-e 'script' foo.txt"; "edit script")]
    #[test_case("-e 'script'"; "edit script with no files")]
    #[test_case("--expression 'script' foo.txt"; "edit script long")]
    #[test_case("--expression 'script'"; "edit script with no files long")]
    #[test_case("-f README.md foo.txt"; "script file")] // needs to be a real file
    #[test_case("-f README.md"; "script file with no files")]
    #[test_case("--script-file README.md foo.txt"; "script file long")] // needs to be a real file
    #[test_case("--script-file README.md"; "script file with no files long")]
    #[test_case("-9p read ad/buffers/index"; "9p read")]
    #[test_case("-9p write ad/buffers/1/dot"; "9p write")]
    #[test_case("-9p ls ad/buffers"; "9p ls")]
    #[test_case("-9p -A foo read ad/buffers/index"; "9p read with aname")]
    #[test_case("-9p -A foo write ad/buffers/1/dot"; "9p write with aname")]
    #[test_case("-9p -A foo ls ad/buffers"; "9p ls with aname")]
    #[test_case("-h"; "short help")]
    #[test_case("--help"; "long help")]
    #[test_case("-v"; "short version")]
    #[test_case("--version"; "long version")]
    #[test_case("-c README.md"; "config short")]
    #[test_case("--config README.md"; "config long")]
    #[test_case("--default-config"; "default config")]
    #[test_case("-c README.md foo.txt"; "config with file")]
    #[test_case("--default-config foo.txt"; "default config with file")]
    #[test]
    fn valid_args(cmd_line: &str) {
        let it = cmd_line.split_whitespace().map(|s| s.to_string());
        let mut parser = Parser::from_args(it);
        let res = ParsedArgs::try_from_parser(&mut parser);

        assert!(res.is_ok(), "{res:?}");
    }

    // actually invalid argument cases
    #[test_case("-e"; "edit script with no script")]
    #[test_case("--expression"; "edit script with no script long")]
    #[test_case("-f"; "script file with no script file")]
    #[test_case("--script-file"; "script file with no script file long")]
    #[test_case("-f foo.txt"; "script file with unknown script file")]
    #[test_case("--script-file foo.txt"; "script file with unknown script file long")]
    #[test_case("-c"; "config with no path")]
    #[test_case("--config"; "config with no path long")]
    #[test_case("-c nonexistent.toml"; "config with nonexistent path")]
    #[test_case("--config nonexistent.toml"; "config with nonexistent path long")]
    #[test_case("--default-config -c README.md"; "both default and custom config")]
    #[test_case("-c README.md --default-config"; "both custom and default config")]
    #[test_case("-c README.md -c README.md"; "duplicate config flag")]
    #[test]
    fn invalid_args(cmd_line: &str) {
        let it = cmd_line.split_whitespace().map(|s| s.to_string());
        let mut parser = Parser::from_args(it);
        let res = ParsedArgs::try_from_parser(&mut parser);

        assert!(res.is_err(), "{res:?}");
    }
}
