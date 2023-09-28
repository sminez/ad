use ad::Editor;
use std::{env::args, io};

fn main() -> io::Result<()> {
    let mut e = Editor::new();

    let args: Vec<String> = args().collect();
    if args.len() > 1 {
        for fname in &args[1..] {
            e.open_file(fname)?;
        }
    }

    e.set_status_message("HELP: C-s: save | C-q: quit");

    while e.running() {
        e.refresh_screen();
        let k = e.read_key();
        e.handle_keypress(k)?;
    }

    Ok(())
}
