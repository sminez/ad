//! CLI parser
//! See main.rs for the usage of the parsed arguments
use crate::VERSION;
use std::{env, fs};

const USAGE: &str = "\
usage: ad [options] [file ...]     Edit file(s)

options:
  -e, --expression <script>        Execute edit script on file(s)
  -f, --script-file <script-file>  Execute edit script loaded from a script-file on file(s)
  -9p [-A aname] read [path]       Read the contents of a file on a 9p file server
  -9p [-A aname] write [path]      Write the contents of stdin to a file on a 9p file server
  -9p [-A aname] ls [path]         List the contents of a directory on a 9p file server
  -l, --list-sessions              List the current open editor 9p sessions
  --rm-sockets                     Remove all ad 9p sockets from the default namespace directory
  -h, --help                       Print this help message
  -v, --version                    Print version information
";

#[derive(Debug)]
pub enum Args {
    OpenEditor { files: Vec<String> },
    RunScript { script: String, files: Vec<String> },
    NineP { args: Vec<String> },
    ListSessions,
    RmSockets,
}

impl Args {
    pub fn try_parse() -> Result<Self, (String, i32)> {
        let args = env::args().skip(1);
        Self::try_parse_iter(args)
    }

    fn try_parse_iter(mut args: impl Iterator<Item = String>) -> Result<Self, (String, i32)> {
        match args.next().as_deref() {
            Some("-e" | "--expression") => match args.next() {
                Some(script) => Ok(Args::RunScript {
                    script,
                    files: args.collect(),
                }),
                None => Err(("no script provided".to_string(), 1)),
            },

            Some("-f" | "--script-file") => match args.next() {
                Some(fname) => match fs::read_to_string(&fname) {
                    Ok(script) => Ok(Args::RunScript {
                        script,
                        files: args.collect(),
                    }),
                    Err(e) => Err((format!("unable to load script file from {fname}: {e}"), 1)),
                },
                None => Err(("no script file provided".to_string(), 1)),
            },

            Some("-9p") => Ok(Args::NineP {
                args: args.collect(),
            }),

            Some("-l" | "--list-sessions") => Ok(Args::ListSessions),
            Some("--rm-sockets") => Ok(Args::RmSockets),

            Some("-h" | "--help") => Err((USAGE.to_string(), 0)),
            Some("-v" | "--version") => Err((format!("ad v{VERSION}"), 0)),

            Some(fname) => {
                let mut files = vec![fname.to_string()];
                files.extend(args);

                Ok(Args::OpenEditor { files })
            }
            None => Ok(Args::OpenEditor { files: Vec::new() }),
        }
    }
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
    #[test]
    fn valid_args(cmd_line: &str) {
        let it = cmd_line.split_whitespace().map(|s| s.to_string());
        let res = Args::try_parse_iter(it);
        assert!(res.is_ok(), "{res:?}");
    }

    // help and version return as error cases rather than constructing args
    #[test_case("-h"; "short help")]
    #[test_case("--help"; "long help")]
    #[test_case("-v"; "short version")]
    #[test_case("--version"; "long version")]
    // actually invalid argument cases
    #[test_case("-e"; "edit script with no script")]
    #[test_case("--expression"; "edit script with no script long")]
    #[test_case("-f"; "script file with no script file")]
    #[test_case("--script-file"; "script file with no script file long")]
    #[test_case("-f foo.txt"; "script file with unknown script file")]
    #[test_case("--script-file foo.txt"; "script file with unknown script file long")]
    #[test]
    fn invalid_args(cmd_line: &str) {
        let it = cmd_line.split_whitespace().map(|s| s.to_string());
        let res = Args::try_parse_iter(it);
        assert!(res.is_err(), "{res:?}");
    }
}
