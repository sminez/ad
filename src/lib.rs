//! ad :: the adaptable editor
pub mod buffer;
pub mod editor;
pub mod key;
pub mod term;

pub const VERSION: &str = "1.0.0";
pub const UNNAMED_BUFFER: &str = "[No Name]";
pub const TAB_STOP: usize = 4;
pub const STATUS_TIMEOUT: u64 = 5;
pub const MAX_NAME_LEN: usize = 20;
