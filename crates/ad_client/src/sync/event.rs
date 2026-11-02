//! Handling of event filtering
use crate::{EventData, EventOutcome, FilterState, Result, sync::BufferClient};
use ad_event::Kind;

/// An event filter takes control over a buffer's events file and handles processing the events
/// that come through. Any events without a corresponding handler are written back to ad for
/// internal processing.
#[expect(unused_variables)]
pub trait EventFilter {
    /// Handle text being inserted into the buffer body
    fn on_insert(&mut self, data: EventData<'_>, client: &BufferClient) -> Result<EventOutcome> {
        Ok(EventOutcome::Handled)
    }

    /// Handle text being deleted from the buffer body
    fn on_delete(&mut self, data: EventData<'_>, client: &BufferClient) -> Result<EventOutcome> {
        Ok(EventOutcome::Handled)
    }

    /// Handle a load event in the body
    fn on_load(&mut self, data: EventData<'_>, client: &BufferClient) -> Result<EventOutcome> {
        Ok(EventOutcome::Passthrough)
    }

    /// Handle an execute event in the body.
    fn on_execute(
        &mut self,
        data: EventData<'_>,
        chorded_arg: Option<EventData<'_>>,
        client: &BufferClient,
    ) -> Result<EventOutcome> {
        Ok(EventOutcome::Passthrough)
    }
}

pub(super) fn run_filter<F>(mut filter: F, client: &BufferClient) -> Result<()>
where
    F: EventFilter,
{
    let mut state = FilterState::default();

    loop {
        for line in client.event_lines()? {
            let evt = match state.next_event(&line)? {
                Some(evt) => evt,
                None => continue,
            };
            let data = EventData::from(&evt);

            let outcome = match evt.kind {
                Kind::InsertBody | Kind::InsertScratch => filter.on_insert(data, client)?,
                Kind::DeleteBody | Kind::DeleteScratch => filter.on_delete(data, client)?,
                Kind::LoadBody | Kind::LoadScratch => filter.on_load(data, client)?,
                Kind::ExecuteBody | Kind::ExecuteScratch => {
                    filter.on_execute(data, state.chorded_arg(&evt), client)?
                }
                Kind::ChordedArgument => unreachable!("chorded arguments are held by FilterState"),
            };

            let step = state.handle_outcome(&evt, outcome);
            if let Some(s) = step.write_back {
                client.write_event(&s)?;
            }
            if step.exit {
                return Ok(());
            }
        }
    }
}
