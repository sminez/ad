//! A simple demo of the 9p client interface
use ninep::{fs::FileType, tokio::client::UnixClient};
use std::io;

#[tokio::main]
async fn main() -> io::Result<()> {
    let mut client = UnixClient::new_unix("ninep-server", "").await?;
    tree(&mut client, "", 0).await?;

    let mut stream = client.stream_lines("blocking").await?;
    while let Some(line) = stream.next().await {
        println!("{line}");
    }

    Ok(())
}

async fn tree(client: &mut UnixClient, path: &str, depth: usize) -> io::Result<()> {
    for stat in client.read_dir(path).await? {
        let name = stat.fm.name;
        println!("{:indent$}{name}", "", indent = depth * 2);
        if stat.fm.ty == FileType::Directory {
            let child = if path.is_empty() {
                name
            } else {
                format!("{path}/{name}")
            };
            Box::pin(tree(client, &child, depth + 1)).await?;
        }
    }

    Ok(())
}
