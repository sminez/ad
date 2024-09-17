//! ad :: the adaptable editor
#![warn(
    clippy::complexity,
    clippy::correctness,
    clippy::style,
    future_incompatible,
    missing_debug_implementations,
    missing_docs,
    rust_2018_idioms,
    rustdoc::all,
    clippy::undocumented_unsafe_blocks
)]

use libc::termios as Termios;
use std::{
    io::Stdout,
    sync::{OnceLock, RwLock},
};

pub mod buffer;
pub mod config;
pub mod dot;
pub mod editor;
pub mod exec;
pub mod fsys;
pub mod ftype;
pub mod key;
pub mod log;
pub mod mode;
pub mod regex;
pub mod term;
pub mod trie;
pub mod util;

pub use buffer::GapBuffer;
pub use config::Config;
pub use editor::{Editor, EditorMode};
pub use exec::{CachedStdin, Edit, Program};
pub use log::LogBuffer;

use term::{disable_alternate_screen, disable_mouse_support, set_termios};

/// The environment variable to set to control logging within ad
pub const LOG_LEVEL_ENV_VAR: &str = "AD_LOG";

pub(crate) const VERSION: &str = env!("CARGO_PKG_VERSION");
pub(crate) const UNNAMED_BUFFER: &str = "[No Name]";
pub(crate) const MAX_NAME_LEN: usize = 20;

pub(crate) static ORIGINAL_TERMIOS: OnceLock<Termios> = OnceLock::new();

/// Global config values which are only ever updated from the main editor thread.
/// This is handled as a static OnceLock rather than being a property on the
/// Editor itself as it avoids having to thread the Config struct as a parameter
/// through to everywhere that it is needed outside of the main Editor methods.
pub(crate) static CONFIG: OnceLock<RwLock<Config>> = OnceLock::new();

pub(crate) fn set_config(cfg: Config) {
    _ = CONFIG.set(RwLock::new(cfg));
}

pub(crate) fn replace_config(cfg: Config) {
    *CONFIG
        .get_or_init(|| RwLock::new(Config::default()))
        .write()
        .unwrap() = cfg;
}

pub(crate) fn update_config(input: &str) -> Result<(), String> {
    let mut guard = CONFIG
        .get_or_init(|| RwLock::new(Config::default()))
        .write()
        .unwrap();

    guard.update_from(input)
}

/// Get a read-only handle to the global Config data
#[macro_export]
macro_rules! config_handle {
    () => {{
        $crate::CONFIG
            .get_or_init(|| std::sync::RwLock::new($crate::Config::default()))
            .read()
            .unwrap()
    }};
}

/// Helper for panicing the program but first ensuring that we have restored the
/// terminal state in the same way that we do when the Editor is dropped cleanly
#[macro_export]
macro_rules! die {
    ($template:expr $(, $arg:expr)*) => {{
        $crate::restore_terminal_state(&mut ::std::io::stdout());
        panic!($template $(, $arg)*)
    }};

}

/// Restore the terminal state to what we had originally before starting our UI.
///
/// # Panics
/// This will panic if ORIGINAL_TERMIOS has not been set.
pub(crate) fn restore_terminal_state(so: &mut Stdout) {
    disable_alternate_screen(so);
    disable_mouse_support(so);
    set_termios(*ORIGINAL_TERMIOS.get().unwrap());
}
