//! A minimal 9p client that is ad aware.
//!
//! The public functions exposed by this module are intended for use as CLI actions and exit the
//! process on error rather than returning results. For a client that can be used from external
//! programs see the `ninep` crate for generic clients and the `ad_client` crate for ad specific
//! clients.
use ninep::{
    fs::{Mode, Perm},
    sansio::server::socket_dir,
    sync::client::{Client, Error, Result},
};
use std::{
    env, fs,
    io::{self, Read},
    process::exit,
};

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

impl Cmd {
    pub fn try_from_str(s: &str) -> Option<Self> {
        let cmd = match s {
            "ls" => Cmd::List,
            "lsl" => Cmd::LongList,
            "mkdir" => Cmd::MkDir,
            "new" => Cmd::New,
            "read" => Cmd::Read,
            "rm" => Cmd::Remove,
            "write" => Cmd::Write,
            _ => return None,
        };

        Some(cmd)
    }
}

/// Make a single 9p client request to the specified server.
pub fn oneshot_9p(cmd: Cmd, raw_path: String, aname: String) {
    if let Err(e) = oneshot_9p_inner(cmd, raw_path, aname) {
        eprintln!("{e}");
        exit(1);
    }
}

fn oneshot_9p_inner(cmd: Cmd, raw_path: String, aname: String) -> Result<()> {
    let (client, path) = client_and_path(&raw_path, &aname)?;

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
            let (dir, name) = path.rsplit_once('/').unwrap_or(("/", path));
            client.create(
                dir,
                name,
                Perm::DIRECTORY | Perm::any_read() | Perm::any_write() | Perm::any_exec(),
                Mode::READ,
            )?;
        }

        Cmd::New => {
            let (dir, name) = path.rsplit_once('/').unwrap_or(("/", path));
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

        Cmd::Remove => client.remove(path)?,

        Cmd::Write => {
            let mut content = String::new();
            io::stdin().read_to_string(&mut content)?;
            client.write_str(path, 0, &content)?;
        }
    }

    Ok(())
}

fn err(s: impl Into<String>) -> Error {
    Error::Rerror { ename: s.into() }
}

fn client_and_path<'a>(raw_path: &'a str, aname: &str) -> Result<(Client, &'a str)> {
    let (ns, path) = match raw_path.split_once('/') {
        Some((ns, path)) => (ns, path),
        None => (raw_path, "/"),
    };

    if ns != "ad" {
        return Ok((Client::new_unix(ns, aname)?, path));
    }

    // Depending on the requested namespace and the presence or absence of an "AD_PID" env var we
    // may need to adjust the ns to include an ad PID
    let mut ns = ns.to_string();
    if let Ok(pid) = env::var("AD_PID") {
        ns.push('-');
        ns.push_str(&pid);
    } else {
        // If there is only a single running ad instance then we can attach to that, otherwise we
        // need to error out and prompt the user to select the appropriate instance they want to
        // connect to.
        let mut ad_sockets = open_9p_sockets()?;

        match ad_sockets.len() {
            1 => ns = ad_sockets.remove(0),
            0 => {
                return Err(
                    io::Error::new(io::ErrorKind::NotFound, "No such file or directory").into(),
                );
            }
            _ => {
                return Err(err(format!(
                    "please specify which ad instance to connect to:\n{}",
                    ad_sockets.join("\n")
                )));
            }
        }
    };

    Ok((Client::new_unix(ns, aname)?, path))
}

fn open_9p_sockets() -> io::Result<Vec<String>> {
    let mut ad_sockets = Vec::new();
    for entry in fs::read_dir(socket_dir())? {
        let entry = entry?;
        let fname = entry.file_name();
        if let Some(s) = fname.to_str()
            && s.starts_with("ad-")
        {
            ad_sockets.push(s.to_string());
        }
    }

    Ok(ad_sockets)
}

/// List open ad sessions based on their 9p socket details
pub fn list_open_sessions() {
    fn inner() -> io::Result<()> {
        let mut had_unresponsive = false;

        for ns in open_9p_sockets()?.into_iter() {
            let client = match Client::new_unix(&ns, "") {
                Ok(client) => client,
                Err(e) => {
                    println!("{ns}\tunresponsive: {e}");
                    had_unresponsive = true;
                    continue;
                }
            };
            let id = client.read_str("buffers/current")?;
            let fname = client.read_str(format!("buffers/{id}/filename"))?;
            println!("{ns}\t{fname}");
        }

        if had_unresponsive {
            println!("\nYou can remove unresponsive sockets using --rm-sockets");
        }

        Ok(())
    }

    if let Err(e) = inner() {
        eprintln!("unable to list open editor sessions: {e}");
        exit(1);
    }
}

/// Clear up unresponsive ad 9p sockets
pub fn remove_open_sockets() {
    fn inner() -> io::Result<()> {
        let d = socket_dir();
        for ns in open_9p_sockets()?.into_iter() {
            if Client::new_unix(&ns, "").is_err() {
                let path = d.join(ns);
                println!("removing unresponsive ad socket at {}", path.display());
                fs::remove_file(path)?;
            }
        }

        Ok(())
    }

    if let Err(e) = inner() {
        eprintln!("unable to remove open 9p sockets: {e}");
        exit(1);
    }
}
