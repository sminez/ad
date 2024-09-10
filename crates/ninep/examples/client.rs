//! A simple demo of the 9p client interface
use ninep::{client::Client, fs::FileType};
use std::io;

fn main() -> io::Result<()> {
    let mut client = Client::new_unix("ninep-server", "")?;
    tree(&mut client, "", 0)?;

    for line in client.iter_lines("blocking")? {
        print!("{line}");
    }

    Ok(())
}

fn tree(client: &mut Client, path: &str, depth: usize) -> io::Result<()> {
    for stat in client.read_dir(path)? {
        let name = stat.fm.name;
        println!("{:indent$}{name}", "", indent = depth * 2);
        if stat.fm.ty == FileType::Directory {
            let child = if path.is_empty() {
                name
            } else {
                format!("{path}/{name}")
            };
            tree(client, &child, depth + 1)?;
        }
    }

    Ok(())
}
