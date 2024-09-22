//! A simple demo of the 9p client interface
use ninep::client::UnixClient;
use std::io;

fn main() -> io::Result<()> {
    let mut client = UnixClient::new_unix("acme", "")?;

    for line in client.iter_lines("43/event")? {
        print!("{line}");
    }

    Ok(())
}
