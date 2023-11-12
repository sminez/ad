//! ad :: the adaptable editor
use std::sync::{Mutex, OnceLock};

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

// TODO: move these to config
pub const STATUS_TIMEOUT: u64 = 5;
pub const MINI_BUFFER_HEIGHT: usize = 10;

pub(crate) static CONFIG: OnceLock<Mutex<Config>> = OnceLock::new();

pub(crate) fn init_config(cfg: Config) {
    _ = CONFIG.set(Mutex::new(cfg));
}

pub(crate) fn update_config(input: &str) -> Result<(), String> {
    let mut guard = CONFIG
        .get_or_init(|| Mutex::new(Config::default()))
        .lock()
        .unwrap();

    guard.try_set_prop(input)
}

#[macro_export]
macro_rules! cfg {
    () => {{
        $crate::CONFIG
            .get_or_init(|| std::sync::Mutex::new($crate::Config::default()))
            .lock()
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
