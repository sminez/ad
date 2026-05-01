//! Handling of event filtering
use crate::{Outcome, tokio::Client};
use ad_event::{FsysEvent, Kind, Source};
use ninep::tokio::client::Result;
use std::io;

/// An event filter takes control over a buffer's events file and handles processing the events
/// that come through. Any events without a corresponding handler are written back to ad for
/// internal processing.
#[expect(unused_variables)]
pub trait AsyncEventFilter {
    /// Handle text being inserted into the buffer body
    fn handle_insert(
        &mut self,
        src: Source,
        from: usize,
        to: usize,
        txt: &str,
        client: &mut Client,
    ) -> impl Future<Output = io::Result<Outcome>> + Send {
        async { Ok(Outcome::Handled) }
    }

    /// Handle text being deleted from the buffer body
    fn handle_delete(
        &mut self,
        src: Source,
        from: usize,
        to: usize,
        client: &mut Client,
    ) -> impl Future<Output = io::Result<Outcome>> + Send {
        async { Ok(Outcome::Handled) }
    }

    /// Handle a load event in the body
    fn handle_load(
        &mut self,
        src: Source,
        from: usize,
        to: usize,
        txt: &str,
        client: &mut Client,
    ) -> impl Future<Output = io::Result<Outcome>> + Send {
        async { Ok(Outcome::Passthrough) }
    }

    /// Handle an execute event in the body
    fn handle_execute(
        &mut self,
        src: Source,
        from: usize,
        to: usize,
        txt: &str,
        client: &mut Client,
    ) -> impl Future<Output = io::Result<Outcome>> + Send {
        async { Ok(Outcome::Passthrough) }
    }
}

pub(super) async fn run_filter<F>(buffer: &str, mut filter: F, client: &mut Client) -> Result<()>
where
    F: AsyncEventFilter,
{
    let mut stream = client.event_lines(buffer).await?;

    while let Some(line) = stream.next().await {
        let evt = FsysEvent::try_from_str(&line).map_err(io::Error::other)?;

        let outcome = match evt.kind {
            Kind::LoadBody => {
                filter
                    .handle_load(evt.source, evt.ch_from, evt.ch_to, &evt.txt, client)
                    .await?
            }
            Kind::ExecuteBody => {
                filter
                    .handle_execute(evt.source, evt.ch_from, evt.ch_to, &evt.txt, client)
                    .await?
            }
            Kind::InsertBody => {
                filter
                    .handle_insert(evt.source, evt.ch_from, evt.ch_to, &evt.txt, client)
                    .await?
            }
            Kind::DeleteBody => {
                filter
                    .handle_delete(evt.source, evt.ch_from, evt.ch_to, client)
                    .await?
            }
            _ => Outcome::Passthrough,
        };

        match outcome {
            Outcome::Handled => (),
            Outcome::Passthrough => {
                client
                    .write_event(buffer, &evt.as_event_file_line())
                    .await?
            }
            Outcome::PassthroughAndExit => {
                client
                    .write_event(buffer, &evt.as_event_file_line())
                    .await?;
                return Ok(());
            }
            Outcome::Exit => return Ok(()),
        }
    }

    Ok(())
}
