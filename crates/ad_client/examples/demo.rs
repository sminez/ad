//! A simple demo of the client behaviour
use ad_client::sync::Client;
use std::io;

fn main() -> io::Result<()> {
    let client = Client::new()?;
    client.echo("hello, world!")?;
    client.open("README.md")?;

    Ok(())
}
