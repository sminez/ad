//! A simple demo of the filesystem interface
use ad::fsys::{AdFs, BufId, Message, Req};
use std::{
    sync::mpsc::{channel, Receiver, Sender},
    thread::{spawn, JoinHandle},
};

fn main() {
    // In the real code the other halves of these channels are handed off to the editor
    let (mtx, _) = channel();
    let (_, brx) = channel();
    let fs = AdFs::new(mtx, brx);

    println!("mounting at {}", fs.mount_path());
    let fs_handle = fs.run_threaded();

    fs_handle.join().unwrap()
}

fn mock_editor_thread(mrx: Receiver<Message>, btx: Sender<BufId>) -> JoinHandle<()> {
    use Req::*;

    spawn(move || {
        btx.send(BufId::Add(1)).unwrap();
        btx.send(BufId::Add(2)).unwrap();

        loop {
            let Message { req, tx } = mrx.recv().unwrap();

            let s = match req {
                ReadBufferName { id: 1 } => "my-first-buffer.txt",
                ReadBufferDot { id: 1 } => "contents of dot",
                ReadBufferAddr { id: 1 } => "3:5,3:16",
                ReadBufferBody { id: 1 } => {
                    "this is the full file\nwith lines\nand the contents of dot"
                }

                ReadBufferName { id: 2 } => "demo-buffer.rs",
                ReadBufferDot { id: 2 } => "some other dot contents",
                ReadBufferAddr { id: 2 } => "5:12,5:29",
                ReadBufferBody { id: 2 } => "clearly this is just test data",

                _ => continue,
            };

            tx.send(s.to_string()).unwrap()
        }
    })
}
