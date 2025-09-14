//! A simple demo of the client behaviour
use ad_client::list_open_sessions;

fn main() -> std::io::Result<()> {
    for session in list_open_sessions()?.into_iter() {
        println!("{session:#?}");
    }

    Ok(())
}
