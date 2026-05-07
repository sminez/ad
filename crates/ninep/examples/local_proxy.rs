use ninep::{
    sync::server::Server,
    util::{hook::HookFs, local_proxy::LocalProxyFs},
};
use std::env::{args, current_dir};

fn main() {
    let path = args().nth(1).expect("must provide a path to bind to");
    let chatty = args().nth(2).as_deref() == Some("--chatty");

    let fs = HookFs::new(LocalProxyFs::new(path).unwrap(), move |op| {
        if chatty {
            println!("{op:?}");
        }

        Ok(())
    });

    let s = Server::new(fs);
    let socket_path = current_dir().unwrap().join("proxy-fs");

    println!(
        "starting local-proxy-fs file server at {}",
        socket_path.display()
    );
    if s.serve_socket_with_custom_path(socket_path).join().is_err() {
        eprintln!("server thread died");
    }
}
