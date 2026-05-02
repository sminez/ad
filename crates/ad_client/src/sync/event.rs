//! Handling of event filtering
use crate::{EventOutcome, Result, sync::Client};
use ad_event::{FsysEvent, Kind, Source};
use std::io;

/// An event filter takes control over a buffer's events file and handles processing the events
/// that come through. Any events without a corresponding handler are written back to ad for
/// internal processing.
#[expect(unused_variables)]
pub trait EventFilter {
    /// Handle text being inserted into the buffer body
    fn handle_insert(
        &mut self,
        src: Source,
        from: usize,
        to: usize,
        txt: &str,
        client: &mut Client,
    ) -> Result<EventOutcome> {
        Ok(EventOutcome::Handled)
    }

    /// Handle text being deleted from the buffer body
    fn handle_delete(
        &mut self,
        src: Source,
        from: usize,
        to: usize,
        client: &mut Client,
    ) -> Result<EventOutcome> {
        Ok(EventOutcome::Handled)
    }

    /// Handle a load event in the body
    fn handle_load(
        &mut self,
        src: Source,
        from: usize,
        to: usize,
        txt: &str,
        client: &mut Client,
    ) -> Result<EventOutcome> {
        Ok(EventOutcome::Passthrough)
    }

    /// Handle an execute event in the body
    fn handle_execute(
        &mut self,
        src: Source,
        from: usize,
        to: usize,
        txt: &str,
        client: &mut Client,
    ) -> Result<EventOutcome> {
        Ok(EventOutcome::Passthrough)
    }
}

pub(super) fn run_filter<F>(buffer: usize, mut filter: F, client: &mut Client) -> Result<()>
where
    F: EventFilter,
{
    for line in client.event_lines(buffer)? {
        let evt = FsysEvent::try_from_str(&line).map_err(io::Error::other)?;

        let outcome = match evt.kind {
            Kind::LoadBody => {
                filter.handle_load(evt.source, evt.ch_from, evt.ch_to, &evt.txt, client)?
            }
            Kind::ExecuteBody => {
                filter.handle_execute(evt.source, evt.ch_from, evt.ch_to, &evt.txt, client)?
            }
            Kind::InsertBody => {
                filter.handle_insert(evt.source, evt.ch_from, evt.ch_to, &evt.txt, client)?
            }
            Kind::DeleteBody => filter.handle_delete(evt.source, evt.ch_from, evt.ch_to, client)?,
            _ => EventOutcome::Passthrough,
        };

        match outcome {
            EventOutcome::Handled => (),
            EventOutcome::Passthrough => client.write_event(buffer, &evt.as_event_file_line())?,
            EventOutcome::PassthroughAndExit => {
                client.write_event(buffer, &evt.as_event_file_line())?;
                return Ok(());
            }
            EventOutcome::Exit => return Ok(()),
        }
    }

    Ok(())
}
