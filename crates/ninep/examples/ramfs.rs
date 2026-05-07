//! A simple in-memory filesystem that allows clients to read and write arbitrary file paths.
use ninep::{
    sync::server::Server,
    util::{hook::HookFs, ram::RamFs},
};
use std::env::{args, current_dir};

fn main() {
    let chatty = args().nth(1).as_deref() == Some("--chatty");
    let fs = HookFs::new(RamFs::new(env!("USER"), "group"), move |op| {
        if chatty {
            println!("{op:?}");
        }

        Ok(())
    });

    let s = Server::new(fs);
    let socket_path = current_dir().unwrap().join("ramfs");

    println!("starting ram-fs file server at {}", socket_path.display());
    if s.serve_socket_with_custom_path(socket_path).join().is_err() {
        eprintln!("server thread died");
    }
}
