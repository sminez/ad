//! A simple REPL that syncs an rc shell session with an ad buffer.
//!
//! - "Execute" is defined to be "send as input to the shell".
//! - Hitting return at the end of the buffer will send that line to the shell.
//! - Running "clear" will clear the ad buffer
//! - Running "exit" will close the shell subprocess as well as the ad buffer
use ad_client::{
    EventOutcome, Result, Source,
    sync::{Client, EventFilter},
};
use anyhow::Context;
use std::{
    env,
    fs::File,
    io::{self, Write, copy},
    process::exit,
    thread::spawn,
};
use subprocess::{Exec, Job, Redirection};

const PROMPT: &str = "% ";

fn main() -> anyhow::Result<()> {
    let mut client = match Client::new() {
        Ok(client) => client,
        Err(e) => {
            eprintln!("unable to connect to ad\n{e}");
            exit(1);
        }
    };

    let buffer_id = client
        .open_in_new_window("+repl")
        .context("unable to create +repl window")?;
    let mut env_vars: Vec<(String, String)> = env::vars().collect();
    env_vars.push(("prompt".into(), PROMPT.into()));

    let mut child = Exec::cmd("rc")
        .arg("-i")
        .stdin(Redirection::Pipe)
        .stdout(Redirection::Pipe)
        .stderr(Redirection::Merge)
        .env_extend(env_vars)
        .start()
        .context("unable to spawn rc")?;

    let stdin = child.stdin.take().unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut w = client
        .body_writer(buffer_id)
        .context("unable to create body writer")?;

    spawn(move || {
        _ = copy(&mut stdout, &mut w);
    });

    client
        .run_event_filter(
            buffer_id,
            Filter {
                child,
                buffer_id,
                stdin,
            },
        )
        .context("event filter died")?;

    Ok(())
}

struct Filter {
    child: Job,
    buffer_id: usize,
    stdin: File,
}

impl Drop for Filter {
    fn drop(&mut self) {
        _ = self.child.kill();
    }
}

impl Filter {
    fn clear_buffer(&mut self, client: &mut Client) -> io::Result<()> {
        client.write_xaddr(self.buffer_id, ",")?;
        client.write_xdot(self.buffer_id, PROMPT)?;
        client.write_addr(self.buffer_id, "$")?;
        client.ctl("mark-clean", "")?;

        Ok(())
    }

    fn send_input(&mut self, input: &str, client: &mut Client) -> Result<EventOutcome> {
        match input.trim() {
            "clear" => {
                self.clear_buffer(client)?;
                return Ok(EventOutcome::Handled);
            }

            "exit" => {
                client.ctl("db!", "")?;
                self.child.kill()?;
                exit(0);
            }

            _ => (),
        }

        self.stdin.write_all(input.as_bytes())?;
        if !input.ends_with("\n") {
            self.stdin.write_all(b"\n")?;
        }

        Ok(EventOutcome::Handled)
    }
}

impl EventFilter for Filter {
    fn handle_insert(
        &mut self,
        src: Source,
        _from: usize,
        _to: usize,
        txt: &str,
        client: &mut Client,
    ) -> Result<EventOutcome> {
        client.mark_clean()?;

        if src == Source::Fsys {
            // This is us writing to the body so move dot to EOF
            client.write_addr(self.buffer_id, "$")?;
            return Ok(EventOutcome::Handled);
        }

        if txt == "\n" {
            client.write_xaddr(self.buffer_id, "$")?;
            let xaddr = client.read_xaddr(self.buffer_id)?;
            let addr = client.read_addr(self.buffer_id)?;

            if xaddr == addr {
                client.write_xaddr(self.buffer_id, "$-1")?;
                let raw = client.read_xdot(self.buffer_id)?;
                return self.send_input(strip_prompt(&raw), client);
            }
        }

        Ok(EventOutcome::Handled)
    }

    fn handle_delete(
        &mut self,
        _src: Source,
        _from: usize,
        _to: usize,
        client: &mut Client,
    ) -> Result<EventOutcome> {
        client.mark_clean()?;

        Ok(EventOutcome::Handled)
    }

    fn handle_execute(
        &mut self,
        _src: Source,
        _from: usize,
        _to: usize,
        txt: &str,
        client: &mut Client,
    ) -> Result<EventOutcome> {
        let s = strip_prompt(txt).trim();
        client.append_to_body(self.buffer_id, &format!("\n{PROMPT}{s}\n"))?;
        let outcome = self.send_input(s, client)?;

        Ok(outcome)
    }
}

#[inline]
fn strip_prompt(s: &str) -> &str {
    s.strip_prefix(PROMPT).unwrap_or(s)
}
