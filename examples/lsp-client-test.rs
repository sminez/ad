//! Testing out new client behaviour
use ad_editor::{Config, lsp::LspManager};
use std::sync::mpsc::channel;

// Problems:
// - The current API is tightly coupled to the editor state in order to make things simpler to work
//   with from the Buffers and Editor structs.
// - This means that creating an independent LSP client for exploratory work is actually pretty
//   tricky when

fn main() {
    let cfg = Config::try_load().unwrap();
    let (tx, rx) = channel();

    let lsp_man = LspManager::spawn(cfg.languages, tx);
}
