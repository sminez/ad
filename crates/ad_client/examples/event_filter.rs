use ad_client::{
    EventOutcome, Result, Source,
    sync::{Client, EventFilter},
};
use std::io;

fn main() -> io::Result<()> {
    let client = Client::new()?;
    client.open(".")?;
    let bufid = client.current_buffer()?;
    client.run_event_filter(bufid, Filter)?;

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
        _client: &Client,
    ) -> Result<EventOutcome> {
        println!("got load: {from}->{to} {txt:?}");
        match txt {
            "README.md" => Ok(EventOutcome::Passthrough),
            _ => {
                println!("  > suppressing load of {txt}");
                Ok(EventOutcome::Handled)
            }
        }
    }
}
