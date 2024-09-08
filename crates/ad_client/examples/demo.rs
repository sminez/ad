//! A simple demo of the client behaviour
use ad_client::Client;
use std::io;

fn main() -> io::Result<()> {
    let mut client = Client::new()?;
    client.echo("hello, world!")?;
    client.open("README.md")?;

    Ok(())
}
