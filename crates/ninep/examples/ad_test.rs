//! A simple demo of the 9p client interface
use ninep::client::UnixClient;
use std::io;

fn main() -> io::Result<()> {
    let mut client = UnixClient::new_unix("ad", "")?;

    for line in client.iter_lines("buffers/1/event")? {
        print!("{line}");
        if line.contains("README.md") {
            println!(">> Passing back load for README.md");
            _ = client.write_str("buffers/1/event", 0, &line);
        }
    }

    Ok(())
}
