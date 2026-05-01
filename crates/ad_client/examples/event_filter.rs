use ad_client::{
    Outcome, Source,
    sync::{Client, EventFilter},
};
use std::io;

fn main() -> io::Result<()> {
    let mut client = Client::new()?;
    client.open(".")?;
    let buffer = client.current_buffer()?;
    client.run_event_filter(&buffer, Filter)?;

    Ok(())
}

struct Filter;

impl EventFilter for Filter {
    fn handle_load(
        &mut self,
        _src: Source,
        from: usize,
        to: usize,
        txt: &str,
        _client: &mut Client,
    ) -> io::Result<Outcome> {
        println!("got load: {from}->{to} {txt:?}");
        match txt {
            "README.md" => Ok(Outcome::Passthrough),
            _ => {
                println!("  > suppressing load of {txt}");
                Ok(Outcome::Handled)
            }
        }
    }
}
