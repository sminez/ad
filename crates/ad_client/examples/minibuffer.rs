//! A simple demo of the client behaviour
use ad_client::sync::Client;
use std::io;

fn main() -> io::Result<()> {
    let client = Client::new()?;
    let resp =
        client.minibuffer_select("favourite food?", ["fish & chips", "pizza", "ice cream"])?;
    println!("{resp:?}");

    Ok(())
}
