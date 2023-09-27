use ad::editor::Editor;
use std::{env::args, io};

fn main() -> io::Result<()> {
    let mut e = Editor::new();

    let args: Vec<String> = args().collect();
    if args.len() == 2 {
        e.open_file(&args[1])?;
    }

    while e.running {
        e.refresh_screen().unwrap();
        let k = e.read_key();
        e.handle_keypress(k)?;
    }

    Ok(())
}
