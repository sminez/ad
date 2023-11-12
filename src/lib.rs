//! ad :: the adaptable editor
use std::sync::{OnceLock, RwLock};

pub mod buffer;
pub mod config;
pub mod editor;
pub mod exec;
pub mod fsys;
pub mod key;
pub mod mode;
pub mod regex;
pub mod term;
pub mod trie;
pub mod util;

pub use config::Config;
pub use editor::Editor;
pub use exec::{CachedStdin, IterableStream, Program};

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const UNNAMED_BUFFER: &str = "[No Name]";
pub const MAX_NAME_LEN: usize = 20;

/// Global config values which are only ever updated from the main editor thread.
/// This is handled as a static OnceLock rather than being a property on the
/// Editor itself as it avoids having to thread the Config struct as a parameter
/// through to everywhere that it is needed outside of the main Editor methods.
pub(crate) static CONFIG: OnceLock<RwLock<Config>> = OnceLock::new();

pub(crate) fn init_config(cfg: Config) {
    _ = CONFIG.set(RwLock::new(cfg));
}

pub(crate) fn update_config(input: &str) -> Result<(), String> {
    let mut guard = CONFIG
        .get_or_init(|| RwLock::new(Config::default()))
        .write()
        .unwrap();

    guard.try_set_prop(input)
}

/// Get a read-only handle to the global Config data
#[macro_export]
macro_rules! config {
    () => {{
        $crate::CONFIG
            .get_or_init(|| std::sync::RwLock::new($crate::Config::default()))
            .read()
            .unwrap()
    }};
}

/// Helper for panicing the program but first ensuring that the screen is cleared
#[macro_export]
macro_rules! die {
    ($template:expr $(, $arg:expr)*) => {{
        $crate::term::clear_screen(&mut ::std::io::stdout());
        panic!($template $(, $arg)*)
    }};

}
