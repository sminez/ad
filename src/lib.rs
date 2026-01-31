//! ad :: the adaptable editor
#![warn(
    clippy::complexity,
    clippy::correctness,
    clippy::style,
    future_incompatible,
    missing_debug_implementations,
    // missing_docs,
    rust_2018_idioms,
    rustdoc::all,
    clippy::undocumented_unsafe_blocks
)]
// Required for testing rendering behaviour.
// As of https://github.com/rust-lang/rust/issues/140281 this needs to be at the crate level
#![allow(text_direction_codepoint_in_literal)]

use std::{process, sync::OnceLock};

pub use ad_event::Source;

pub mod buffer;
pub mod cli;
pub mod config;
pub mod dot;
pub mod editor;
pub mod exec;
pub mod fsys;
pub mod input;
pub mod key;
pub mod log;
pub mod lsp;
pub mod mode;
mod parse;
pub mod plumb;
pub mod regex;
pub mod syntax;
pub mod system;
pub mod trie;
pub mod ui;
pub mod util;
pub mod ziplist;

pub use cli::{CliAction, Cmd9p, ConfigSource, ParsedArgs, USAGE};
pub use config::Config;
pub use editor::{Editor, EditorMode};
pub use exec::{Edit, Program};
pub use log::LogBuffer;
pub use plumb::PlumbingRules;

/// The environment variable to set to control logging within ad
pub const LOG_LEVEL_ENV_VAR: &str = "AD_LOG";
/// The current version of the editor
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub(crate) const UNNAMED_BUFFER: &str = "[No Name]";
pub(crate) const MAX_NAME_LEN: usize = 50;

pub(crate) static PID: OnceLock<u32> = OnceLock::new();

pub(crate) fn pid() -> u32 {
    *PID.get_or_init(process::id)
}

/// Helper for accessing config stored on self as an `Arc<Mutex<Config>>`
#[macro_export]
macro_rules! config_handle {
    ($self:expr) => {{
        match $self.config.read() {
            Ok(config) => config,
            Err(err) => {
                $self.config.clear_poison();
                err.into_inner()
            }
        }
    }};
}

/// Wrapper around panic! to allow for additional logic
#[macro_export]
macro_rules! die {
    ($template:expr $(, $arg:expr)*) => {{
        panic!($template $(, $arg)*)
    }};

}
