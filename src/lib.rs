//! ad :: the adaptable editor
pub mod buffer;
pub mod editor;
pub mod key;
pub mod term;

pub(crate) use term::die;

pub const VERSION: &str = "1.0.0";
pub const TAB_STOP: usize = 4;
