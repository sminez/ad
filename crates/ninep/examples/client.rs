//! A simple demo of the 9p client interface as a CLI.
//!
//! usage: client [-A aname] [command] [socket path]/[path]
//!
//! commands:
//!   ls          List the contents of a directory
//!   lsl         List the contents of a directory in long mode
//!   mkdir       Create a new empty directory
//!   new         Create a new empty file
//!   read        Read the contents of a file
//!   rm          Remove a file
//!   write       Write the contents of stdin to a file
use lexopt::{Parser, prelude::*};
use ninep::{
    fs::{Mode, Perm},
    sync::client::Client,
};
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let Args {
        aname,
        cmd,
        socket_path,
        path,
    } = Args::try_parse()?;

    let mut client = Client::new_unix_with_explicit_path("user", socket_path, aname)?;

    match cmd {
        Cmd::List => {
            for stat in client.read_dir(path)?.into_iter() {
                println!("{}", stat.name);
            }
        }

        Cmd::LongList => {
            for stat in client.read_dir(path)?.into_iter() {
                println!("{stat}");
            }
        }

        Cmd::MkDir => {
            let (dir, name) = path.rsplit_once('/').unwrap_or(("/", path.as_str()));
            client.create(
                dir,
                name,
                Perm::DIRECTORY | Perm::any_read() | Perm::any_write() | Perm::any_exec(),
                Mode::READ,
            )?;
        }

        Cmd::New => {
            let (dir, name) = path.rsplit_once('/').unwrap_or(("/", path.as_str()));
            client.create(
                dir,
                name,
                Perm::FILE | Perm::any_read() | Perm::any_write() | Perm::any_exec(),
                Mode::READ,
            )?;
        }

        Cmd::Read => {
            for line in client.iter_lines(path)? {
                println!("{line}");
            }
        }

        Cmd::Remove => {
            client.remove(path)?;
        }

        Cmd::Write => {
            let mut content = String::new();
            io::stdin().read_to_string(&mut content)?;
            client.write_str(path, 0, &content)?;
        }
    }

    Ok(())
}

#[derive(Debug)]
pub struct Args {
    aname: String,
    cmd: Cmd,
    socket_path: String,
    path: String,
}

#[derive(Debug)]
pub enum Cmd {
    List,
    LongList,
    MkDir,
    New,
    Read,
    Remove,
    Write,
}

impl Args {
    pub fn try_parse() -> Result<Self, lexopt::Error> {
        let mut parser = Parser::from_env();

        let arg = parser.next()?.ok_or(lexopt::Error::MissingValue {
            option: Some("command".into()),
        })?;

        let mut aname = "".to_string();

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
                    Some("ls") => Cmd::List,
                    Some("lsl") => Cmd::LongList,
                    Some("mkdir") => Cmd::MkDir,
                    Some("new") => Cmd::New,
                    Some("read") => Cmd::Read,
                    Some("rm") => Cmd::Remove,
                    Some("write") => Cmd::Write,
                    _ => return Err(Value(cmd).unexpected()),
                },
                _ => return Err(arg.unexpected()),
            },
            None => return Err(lexopt::Error::from("no command provided")),
        };

        let path = match parser.next()? {
            Some(Value(s)) => s.into_string().map_err(lexopt::Error::NonUnicodeValue)?,
            Some(arg) => return Err(arg.unexpected()),
            None => return Err(lexopt::Error::from("no path provided for")),
        };

        let (socket_path, path) = match path.split_once('/') {
            Some((socket_path, path)) => (socket_path, path),
            None => (path.as_str(), "/"),
        };

        Ok(Args {
            aname,
            cmd,
            socket_path: socket_path.to_string(),
            path: path.to_string(),
        })
    }
}
