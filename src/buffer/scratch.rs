use crate::{
    Config,
    buffer::{Buffer, SCRATCH_ID},
    fsys::InputFilter,
};
use parking_lot::RwLock;
use std::sync::Arc;

const SCRATCH_NAME: &str = "*scratch*";

/// An anonymous buffer that sits outside of the main buffer state and acts as though it is the
/// active buffer for the purposes of Load/Execute.
#[derive(Debug)]
pub struct ScratchBuf {
    main: Buffer,
    transient: Option<Buffer>,
}

impl ScratchBuf {
    pub fn new(config: Arc<RwLock<Config>>) -> Self {
        Self {
            main: Buffer::new_virtual(SCRATCH_ID, SCRATCH_NAME, "", config),
            transient: None,
        }
    }

    pub fn buffer(&self) -> &Buffer {
        self.transient.as_ref().unwrap_or(&self.main)
    }

    pub fn buffer_mut(&mut self) -> &mut Buffer {
        self.transient.as_mut().unwrap_or(&mut self.main)
    }

    pub fn clear(&mut self) {
        self.main.clear();
    }

    pub fn set_transient(
        &mut self,
        name: impl Into<String>,
        content: impl Into<String>,
        config: Arc<RwLock<Config>>,
    ) {
        self.transient = Some(Buffer::new_virtual(SCRATCH_ID, name, content, config));
    }

    pub fn clear_transient(&mut self) {
        self.transient = None;
    }

    pub fn set_input_filter(&mut self, filter: Option<InputFilter>) {
        // Deliberately self.main rather than self.buffer_mut() as we don't support attaching an
        // input filter to transient scratch buffers
        self.main.input_filter = filter;
    }
}
