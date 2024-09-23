use ad_client::{Client, EventFilter, Outcome};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
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
        from: usize,
        to: usize,
        txt: &str,
        _client: &mut Client,
    ) -> Result<Outcome, Box<dyn std::error::Error>> {
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
