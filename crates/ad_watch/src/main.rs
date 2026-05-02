use ad_client::{LogEvent, sync::Client};
use anyhow::Context;
use std::{
    env,
    io::{BufRead, BufReader, Write},
    process::exit,
    thread::spawn,
};
use subprocess::{Popen, PopenConfig, Redirection};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("no command provided to ad-watch");
        exit(1);
    }

    let dir = env::current_dir()
        .context("unable to determine working directory")?
        .display()
        .to_string();

    let mut client = match Client::new() {
        Ok(client) => client,
        Err(e) => {
            eprintln!("unable to connect to ad\n{e}");
            exit(1);
        }
    };

    let buffer_id = client
        .open_in_new_window(format!("{dir}/+watch"))
        .context("unable to open +watch buffer")?;

    clear_and_rerun(&mut client, buffer_id, &args)?;

    for evt in client.log_events()? {
        match evt? {
            LogEvent::Close(id) if id == buffer_id => break,

            LogEvent::Save(id) => {
                let fname = client
                    .read_filename(id)
                    .context("unable to read filename of saved buffer")?;
                if fname.starts_with(&dir) {
                    clear_and_rerun(&mut client, buffer_id, &args)?;
                }
            }

            _ => (),
        }
    }

    Ok(())
}

fn clear_and_rerun(client: &mut Client, id: usize, args: &[String]) -> anyhow::Result<()> {
    client.clear(id).context("unable to clear buffer")?;
    client.mark_clean().context("unable to mark buffer clean")?;

    let mut child = Popen::create(
        args,
        PopenConfig {
            stdout: Redirection::Pipe,
            stderr: Redirection::Merge,
            ..Default::default()
        },
    )
    .context("unable to run command")?;
    let stdout = BufReader::new(child.stdout.take().unwrap());
    let mut w = client
        .body_writer(id)
        .context("unable to create body writer")?;

    spawn(move || {
        for res in stdout.lines() {
            let mut line = match res {
                Ok(line) => line,
                Err(_) => break,
            };

            line.push('\n');
            if line.contains('\r') {
                line = line.replace("\r\n", "\n").replace("\r", "\n");
            }

            _ = w.write_all(line.as_bytes());
            _ = w.mark_clean();
        }
    });

    Ok(())
}
