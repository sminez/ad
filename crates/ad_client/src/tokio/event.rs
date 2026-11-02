//! Handling of event filtering
use crate::{EventData, EventOutcome, FilterState, Result, tokio::BufferClient};
use ad_event::Kind;

/// An event filter takes control over a buffer's events file and handles processing the events
/// that come through. Any events without a corresponding handler are written back to ad for
/// internal processing.
#[expect(unused_variables)]
pub trait AsyncEventFilter {
    /// Handle text being inserted into the buffer body
    fn on_insert(
        &mut self,
        data: EventData<'_>,
        client: &BufferClient,
    ) -> impl Future<Output = Result<EventOutcome>> + Send {
        async { Ok(EventOutcome::Handled) }
    }

    /// Handle text being deleted from the buffer body
    fn on_delete(
        &mut self,
        data: EventData<'_>,
        client: &BufferClient,
    ) -> impl Future<Output = Result<EventOutcome>> + Send {
        async { Ok(EventOutcome::Handled) }
    }

    /// Handle a load event in the body
    fn on_load(
        &mut self,
        data: EventData<'_>,
        client: &BufferClient,
    ) -> impl Future<Output = Result<EventOutcome>> + Send {
        async { Ok(EventOutcome::Passthrough) }
    }

    /// Handle an execute event in the body.
    fn on_execute(
        &mut self,
        data: EventData<'_>,
        chorded_arg: Option<EventData<'_>>,
        client: &BufferClient,
    ) -> impl Future<Output = Result<EventOutcome>> + Send {
        async { Ok(EventOutcome::Passthrough) }
    }
}

pub(super) async fn run_filter<F>(mut filter: F, client: &BufferClient) -> Result<()>
where
    F: AsyncEventFilter,
{
    let mut state = FilterState::default();

    loop {
        let mut stream = client.event_lines().await?;

        while let Some(line) = stream.next().await {
            let evt = match state.next_event(&line)? {
                Some(evt) => evt,
                None => continue,
            };
            let data = EventData::from(&evt);

            let outcome = match evt.kind {
                Kind::InsertBody | Kind::InsertScratch => filter.on_insert(data, client).await?,
                Kind::DeleteBody | Kind::DeleteScratch => filter.on_delete(data, client).await?,
                Kind::LoadBody | Kind::LoadScratch => filter.on_load(data, client).await?,
                Kind::ExecuteBody | Kind::ExecuteScratch => {
                    filter
                        .on_execute(data, state.chorded_arg(&evt), client)
                        .await?
                }
                Kind::ChordedArgument => unreachable!("chorded arguments are held by FilterState"),
            };

            let step = state.handle_outcome(&evt, outcome);
            if let Some(s) = step.write_back {
                client.write_event(&s).await?;
            }
            if step.exit {
                return Ok(());
            }
        }
    }
}
