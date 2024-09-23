//! Handling of event filtering
use crate::Client;
use ad_event::{FsysEvent, Kind};
use std::error::Error;

/// Outcome of handling an event within an [EventFilter]
#[derive(Debug)]
pub enum Outcome {
    /// The event should be passed back to ad
    Passthrough,
    /// The event should not be passed back to ad
    Handled,
    /// The event should be passed back to ad and then the filter should exit
    PassthroughAndExit,
    /// The event should not be passed back to ad and the filter should exit
    Exit,
}

// FIXME: once the tagging of events in the editor is working correctly this should have methods
// that are specificy to the source (Mouse / Keyboard / Fsys) rather than just the kind.

/// An event filter takes control over a buffer's events file and handles processing the events
/// that come through. Any events without a corresponding handler are written back to ad for
/// internal processing.
#[allow(unused_variables)]
pub trait EventFilter {
    /// Handle text being inserted into the buffer body
    fn handle_insert(
        &mut self,
        from: usize,
        to: usize,
        txt: &str,
        client: &mut Client,
    ) -> Result<Outcome, Box<dyn Error>> {
        Ok(Outcome::Passthrough)
    }

    /// Handle text being deleted from the buffer body
    fn handle_delete(
        &mut self,
        from: usize,
        to: usize,
        client: &mut Client,
    ) -> Result<Outcome, Box<dyn Error>> {
        Ok(Outcome::Passthrough)
    }

    /// Handle a load event in the body
    fn handle_load(
        &mut self,
        from: usize,
        to: usize,
        txt: &str,
        client: &mut Client,
    ) -> Result<Outcome, Box<dyn Error>> {
        Ok(Outcome::Passthrough)
    }

    /// Handle an execute event in the body
    fn handle_execute(
        &mut self,
        from: usize,
        to: usize,
        txt: &str,
        client: &mut Client,
    ) -> Result<Outcome, Box<dyn Error>> {
        Ok(Outcome::Passthrough)
    }
}

pub(crate) fn run_filter<F>(
    buffer: &str,
    mut filter: F,
    client: &mut Client,
) -> Result<(), Box<dyn Error>>
where
    F: EventFilter,
{
    let mut buf = String::new();
    for line in client.event_lines(buffer)? {
        buf.push_str(&line);
        let evts = match FsysEvent::try_from_str(&buf) {
            Ok(evts) => evts,
            Err(_) => continue,
        };
        buf.clear();

        for evt in evts.into_iter() {
            let outcome = match evt.kind {
                Kind::LoadBody => filter.handle_load(evt.ch_from, evt.ch_to, &evt.txt, client)?,
                Kind::ExecuteBody => {
                    filter.handle_execute(evt.ch_from, evt.ch_to, &evt.txt, client)?
                }
                Kind::InsertBody => {
                    filter.handle_insert(evt.ch_from, evt.ch_to, &evt.txt, client)?
                }
                Kind::DeleteBody => filter.handle_delete(evt.ch_from, evt.ch_to, client)?,
                _ => Outcome::Passthrough,
            };

            match outcome {
                Outcome::Handled => (),
                Outcome::Passthrough => client.write_event(buffer, &evt.as_event_file_line())?,
                Outcome::PassthroughAndExit => {
                    client.write_event(buffer, &evt.as_event_file_line())?;
                    return Ok(());
                }
                Outcome::Exit => return Ok(()),
            }
        }
    }

    Ok(())
}
