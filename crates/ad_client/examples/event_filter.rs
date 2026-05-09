use ad_client::{
    EventData, EventOutcome, Result,
    sync::{BufferClient, Client, EventFilter},
};
use std::io;

fn main() -> io::Result<()> {
    let client = Client::new()?;
    client.open(".")?;
    let bufid = client.current_buffer()?;
    client.for_buffer(bufid).run_event_filter(Filter)?;

    Ok(())
}

struct Filter;

impl EventFilter for Filter {
    fn on_load(&mut self, data: EventData<'_>, _client: &BufferClient) -> Result<EventOutcome> {
        println!("got load: {}->{} {:?}", data.ch_from, data.ch_to, data.txt);
        match data.txt {
            "README.md" => Ok(EventOutcome::Passthrough),
            _ => {
                println!("  > suppressing load of {}", data.txt);
                Ok(EventOutcome::Handled)
            }
        }
    }
}
