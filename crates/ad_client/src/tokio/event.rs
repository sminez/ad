//! Handling of event filtering
use crate::{
    EventData, EventOutcome, Result,
    tokio::{BufferClient, Client},
};
use ad_event::{FsysEvent, Kind};
use std::io;

/// An event filter takes control over a buffer's events file and handles processing the events
/// that come through. Any events without a corresponding handler are written back to ad for
/// internal processing.
#[expect(unused_variables)]
pub trait AsyncEventFilter {
    /// Handle text being inserted into the buffer body
    fn on_insert(
        &mut self,
        data: EventData<'_>,
        client: &Client,
    ) -> impl Future<Output = Result<EventOutcome>> + Send {
        async { Ok(EventOutcome::Handled) }
    }

    /// Handle text being deleted from the buffer body
    fn on_delete(
        &mut self,
        data: EventData<'_>,
        client: &Client,
    ) -> impl Future<Output = Result<EventOutcome>> + Send {
        async { Ok(EventOutcome::Handled) }
    }

    /// Handle a load event in the body
    fn on_load(
        &mut self,
        data: EventData<'_>,
        client: &Client,
    ) -> impl Future<Output = Result<EventOutcome>> + Send {
        async { Ok(EventOutcome::Passthrough) }
    }

    /// Handle an execute event in the body.
    fn on_execute(
        &mut self,
        data: EventData<'_>,
        chorded_arg: Option<EventData<'_>>,
        client: &Client,
    ) -> impl Future<Output = Result<EventOutcome>> + Send {
        async { Ok(EventOutcome::Passthrough) }
    }
}

pub(super) async fn run_filter<F>(mut filter: F, client: &BufferClient) -> Result<()>
where
    F: AsyncEventFilter,
{
    let mut arg: Option<FsysEvent> = None;

    loop {
        let mut stream = client.event_lines().await?;

        while let Some(line) = stream.next().await {
            let evt = FsysEvent::try_from_str(&line).map_err(io::Error::other)?;
            let data = EventData::from(&evt);

            if let Some(e) = arg.as_mut() {
                e.kind = evt.kind; // ensure that the from_scratch flag is correct
            }

            let outcome = match evt.kind {
                Kind::InsertBody | Kind::InsertScratch => filter.on_insert(data, client).await?,
                Kind::DeleteBody | Kind::DeleteScratch => filter.on_delete(data, client).await?,
                Kind::LoadBody | Kind::LoadScratch => filter.on_load(data, client).await?,
                Kind::ExecuteBody | Kind::ExecuteScratch => {
                    filter
                        .on_execute(data, arg.as_ref().map(Into::into), client)
                        .await?
                }

                Kind::ChordedArgument => {
                    arg = Some(evt.clone());
                    client.write_event(&evt.as_event_file_line()).await?;

                    continue;
                }
            };

            match outcome {
                EventOutcome::Handled => (),
                EventOutcome::Passthrough => client.write_event(&evt.as_event_file_line()).await?,
                EventOutcome::PassthroughAndExit => {
                    client.write_event(&evt.as_event_file_line()).await?;
                    return Ok(());
                }
                EventOutcome::Exit => return Ok(()),
            }
        }
    }
}
