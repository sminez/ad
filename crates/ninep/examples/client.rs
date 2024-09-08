//! A simple demo of the 9p client interface
use ninep::client::Client;

fn main() -> std::io::Result<()> {
    let mut client = Client::new_unix("ad", "")?;

    println!("contents of dot: {:?}", client.read_str("buffers/1/dot")?);
    println!("stat of buffers/1: {:#?}", client.stat("buffers/1")?);

    Ok(())
}
