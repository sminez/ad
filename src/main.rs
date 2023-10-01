use ad::Editor;
use std::env::args;

fn main() {
    let mut e = Editor::new();

    let args: Vec<String> = args().collect();
    if args.len() > 1 {
        for fname in &args[1..] {
            e.open_file(fname);
        }
    }

    e.run()
}
