//! A simple REPL that syncs an rc shell session with an ad buffer.
//!
//! - "Execute" is defined to be "send as input to the shell".
//! - Hitting return at the end of the buffer will send that line to the shell.
//! - Running "clear" will clear the ad buffer
//! - Running "exit" will close the shell subprocess as well as the ad buffer
use ad_client::{
    EventData, EventOutcome, Result, Source,
    sync::{BufferClient, Client, EventFilter},
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
    let client = match Client::new() {
        Ok(client) => client,
        Err(e) => {
            eprintln!("unable to connect to ad\n{e}");
            exit(1);
        }
    };

    let client = client
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
        .body_writer()
        .context("unable to create body writer")?;

    spawn(move || {
        _ = copy(&mut stdout, &mut w);
    });

    client
        .run_event_filter(Filter { child, stdin })
        .context("event filter died")?;

    Ok(())
}

struct Filter {
    child: Job,
    stdin: File,
}

impl Drop for Filter {
    fn drop(&mut self) {
        _ = self.child.kill();
    }
}

impl Filter {
    fn clear_buffer(&mut self, client: &BufferClient) -> io::Result<()> {
        client.write_xaddr(",")?;
        client.write_xdot(PROMPT)?;
        client.write_addr("$")?;
        client.ctl("mark-clean", "")?;

        Ok(())
    }

    fn send_input(&mut self, input: &str, client: &BufferClient) -> Result<EventOutcome> {
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
    fn on_insert(&mut self, data: EventData<'_>, client: &BufferClient) -> Result<EventOutcome> {
        client.mark_clean()?;

        if data.source == Source::Fsys {
            // This is us writing to the body so move dot to EOF
            client.write_addr("$")?;
            return Ok(EventOutcome::Handled);
        }

        if data.txt == "\n" {
            client.write_xaddr("$")?;
            let xaddr = client.read_xaddr()?;
            let addr = client.read_addr()?;

            if xaddr == addr {
                client.write_xaddr("$-1")?;
                let raw = client.read_xdot()?;
                return self.send_input(strip_prompt(&raw), client);
            }
        }

        Ok(EventOutcome::Handled)
    }

    fn on_delete(&mut self, _data: EventData<'_>, client: &BufferClient) -> Result<EventOutcome> {
        client.mark_clean()?;

        Ok(EventOutcome::Handled)
    }

    fn on_execute(
        &mut self,
        data: EventData<'_>,
        arg: Option<EventData<'_>>,
        client: &BufferClient,
    ) -> Result<EventOutcome> {
        if arg.is_some() {
            return Ok(EventOutcome::Passthrough);
        }

        let s = strip_prompt(data.txt).trim();
        client.append_to_body(&format!("\n{PROMPT}{s}\n"))?;
        let outcome = self.send_input(s, client)?;

        Ok(outcome)
    }
}

#[inline]
fn strip_prompt(s: &str) -> &str {
    s.strip_prefix(PROMPT).unwrap_or(s)
}
