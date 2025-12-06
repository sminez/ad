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

use libc::termios as Termios;
use std::{io::Write, process, sync::OnceLock};

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
pub mod term;
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

use term::{disable_alternate_screen, disable_bracketed_paste, disable_mouse_support, set_termios};

/// The environment variable to set to control logging within ad
pub const LOG_LEVEL_ENV_VAR: &str = "AD_LOG";
/// The current version of the editor
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub(crate) const UNNAMED_BUFFER: &str = "[No Name]";
pub(crate) const MAX_NAME_LEN: usize = 50;

pub(crate) static ORIGINAL_TERMIOS: OnceLock<Termios> = OnceLock::new();

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

/// Helper for panicking the program but first ensuring that we have restored the
/// terminal state in the same way that we do when the Editor is dropped cleanly
#[macro_export]
macro_rules! die {
    ($template:expr $(, $arg:expr)*) => {{
        $crate::restore_terminal_state(&mut ::std::io::stdout());
        panic!($template $(, $arg)*)
    }};

}

/// Restore the terminal state to what we had originally before starting our UI.
pub(crate) fn restore_terminal_state(so: &mut impl Write) {
    disable_alternate_screen(so);
    disable_mouse_support(so);
    disable_bracketed_paste(so);
    let t = match ORIGINAL_TERMIOS.get() {
        Some(t) => t,
        None => return,
    };
    set_termios(*t);
}
