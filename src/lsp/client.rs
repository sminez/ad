//! Built-in minimal LSP support for ad
//!
//! This is not a general purpose client and it is not aiming to support all LSP features.
use crate::lsp::{
    Req,
    capabilities::PositionEncoding,
    rpc::{Message, RequestId},
};
use std::{
    ffi::OsStr,
    io::{self, BufRead, BufReader},
    process::{ChildStdin, Command, Stdio},
    sync::mpsc::Sender,
    thread::{JoinHandle, spawn},
};
use tracing::error;

/// A tagged message from an LSP
#[derive(Debug)]
pub struct LspMessage {
    pub lsp_id: usize,
    pub msg: Message,
}

#[derive(Debug, Clone, Copy)]
pub enum Status {
    Initializing,
    Running,
}

/// A simple LSP client that talks to a single lsp server subprocess over stdin / stdout
#[derive(Debug)]
pub struct LspClient {
    pub(super) status: Status,
    pub(super) id: usize,
    pub(super) cmd: String,
    pub(super) position_encoding: PositionEncoding,
    stdin: ChildStdin,
    read_thread: JoinHandle<io::Result<()>>,
    err_thread: JoinHandle<io::Result<()>>,
    next_id: i32,
}

impl LspClient {
    /// Spawn a new lsp server running the given command.
    ///
    /// Stdin for the server is held within the client and can be used via the [LspClient::write]
    /// method to communicate with the server. Messages coming from the server are sent over `tx`
    /// for centeral processing in the main editor event loop and errors are logged.
    pub fn new<I, S>(lsp_id: usize, cmd: &str, args: I, tx: Sender<Req>) -> io::Result<Self>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let mut proc = Command::new(cmd)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdin = proc.stdin.take().unwrap();
        let mut stdout = BufReader::new(proc.stdout.take().unwrap());
        let stderr = BufReader::new(proc.stderr.take().unwrap());

        let read_thread = spawn(move || {
            while let Some(msg) = Message::read(&mut stdout)? {
                if let Err(e) = tx.send(Req::Message(LspMessage { lsp_id, msg })) {
                    return Err(io::Error::other(e));
                }
            }
            Ok(())
        });

        let cmd_ = cmd.to_string();
        let err_thread = spawn(move || {
            for line in stderr.lines() {
                error!("LSP({cmd_}): {}", line?);
            }

            Ok(())
        });

        Ok(Self {
            status: Status::Initializing,
            id: lsp_id,
            cmd: cmd.to_string(),
            position_encoding: PositionEncoding::Utf32,
            stdin,
            read_thread,
            err_thread,
            next_id: 0,
        })
    }

    pub(super) fn next_id(&mut self) -> RequestId {
        let id = self.next_id;
        self.next_id += 1;

        RequestId::Number(id)
    }

    pub(super) fn write(&mut self, msg: Message) -> io::Result<()> {
        msg.write(&mut self.stdin)
    }

    pub(super) fn join(self) {
        match self.read_thread.join() {
            Ok(res) => {
                if let Err(e) = res {
                    error!("LSP({}): stdout thread error: {e}", self.cmd);
                }
            }
            Err(_) => error!(
                "LSP({}): stdout thread failed to shut down cleanly",
                self.cmd
            ),
        }
        match self.err_thread.join() {
            Ok(res) => {
                if let Err(e) = res {
                    error!("LSP({}): stderr thread error: {e}", self.cmd);
                }
            }
            Err(_) => error!(
                "LSP({}): stderr thread failed to shut down cleanly",
                self.cmd
            ),
        }
    }
}
