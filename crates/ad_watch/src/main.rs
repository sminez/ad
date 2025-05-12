use ad_client::{Client, LogEvent};
use std::{
    env,
    io::{self, BufRead, BufReader, Write},
    process::exit,
    thread::spawn,
};
use subprocess::{Popen, PopenConfig, Redirection};

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("no command provided to watch");
        exit(1);
    }

    let dir = env::current_dir()?.display().to_string();

    let mut client = match Client::new() {
        Ok(client) => client,
        Err(e) => {
            eprintln!("unable to connect to ad\n{e}");
            exit(1);
        }
    };

    client.open_in_new_window(format!("{dir}/+watch"))?;
    let buffer_id = client.current_buffer()?;
    let int_id: usize = buffer_id.parse().unwrap();

    clear_and_rerun(&mut client, &buffer_id, &args)?;

    for evt in client.log_events()? {
        match evt? {
            LogEvent::Close(id) if id == int_id => break,

            LogEvent::Save(id) => {
                let fname = client.read_filename(&id.to_string())?;
                if fname.starts_with(&dir) {
                    clear_and_rerun(&mut client, &buffer_id, &args)?;
                }
            }

            _ => (),
        }
    }

    Ok(())
}

fn clear_and_rerun(client: &mut Client, id: &str, args: &[String]) -> io::Result<()> {
    client.write_xaddr(id, ",")?;
    client.write_xdot(id, "\n")?;
    client.ctl("mark-clean", "")?;

    let mut child = Popen::create(
        args,
        PopenConfig {
            stdout: Redirection::Pipe,
            stderr: Redirection::Merge,
            ..Default::default()
        },
    )
    .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    let stdout = BufReader::new(child.stdout.take().unwrap());
    let mut w = client.body_writer(id)?;

    spawn(move || {
        for res in stdout.lines() {
            match res {
                Ok(mut line) => {
                    line.push('\n');
                    if line.contains('\r') {
                        line = line.replace("\r\n", "\n").replace("\r", "\n");
                    }

                    _ = w.write_all(line.as_bytes());
                }
                Err(_) => break,
            }
        }
    });

    Ok(())
}
