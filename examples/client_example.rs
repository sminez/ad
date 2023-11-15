//! https://groups.google.com/g/plan9port-dev/c/Zef5lM0HgnM
use ad::ninep::protocol::{Format9p, Rmessage, Tattach, Tdata, Tmessage, Tstat, Tversion, Twalk};
use std::os::unix::net::UnixStream;

// Binding to 127.0.0.1:2781
// t-message: Tmessage { tag: 65535, content: Version(Tversion { msize: 8192, version: "9P2000" }) }
// r-message: Rmessage { tag: 65535, content: Version(Rversion { msize: 8192, version: "9P2000" }) }

// t-message: Tmessage { tag: 0, content: Auth(Tauth { afid: 0, uname: "innes.andersonmorrison", aname: "" }) }
// r-message: Rmessage { tag: 0, content: Error(Rerror { ename: "authentication not required" }) }

// t-message: Tmessage { tag: 0, content: Attach(Tattach { fid: 0, afid: 4294967295, uname: "innes.andersonmorrison", aname: "" }) }
// r-message: Rmessage { tag: 0, content: Attach(Rattach { aqid: Qid { ty: 128, version: 0, path: 0 } }) }

// t-message: Tmessage { tag: 0, content: Walk(Twalk { fid: 0, new_fid: 1, wnames: [] }) }
// r-message: Rmessage { tag: 0, content: Walk(Rwalk { wqids: [] }) }

// t-message: Tmessage { tag: 0, content: Stat(Tstat { fid: 1 }) }
// r-message: Rmessage { tag: 0, content: Stat(Rstat { stat: RawStat { size: 54, ty: 0, dev: 0, qid: Qid { ty: 128, version: 0, path: 0 }, mode: 2147483968, atime: 1700041099, mtime: 1700041099, length: 0, name: "/", uid: "ad", gid: "ad", muid: "ad" } }) }

// t-message: Tmessage { tag: 0, content: Clunk(Tclunk { fid: 1 }) }
// r-message: Rmessage { tag: 0, content: Clunk(Rclunk) }

// t-message: Tmessage { tag: 0, content: Clunk(Tclunk { fid: 0 }) }
// r-message: Rmessage { tag: 0, content: Clunk(Rclunk) }

fn main() {
    let mntp = "/tmp/ns.innes.andersonmorrison.:0/acme";
    let mut socket = UnixStream::connect(mntp).unwrap();

    send(
        u16::MAX,
        Tdata::Version(Tversion {
            msize: u16::MAX as u32,
            version: "9P2000".to_string(),
        }),
        &mut socket,
    );

    send(
        0,
        Tdata::Attach(Tattach {
            fid: 0,
            afid: 4294967295,
            uname: "innes.andersonmorrison".to_string(),
            aname: "".to_string(),
        }),
        &mut socket,
    );

    send(
        0,
        Tdata::Walk(Twalk {
            fid: 0,
            new_fid: 1,
            wnames: vec![],
        }),
        &mut socket,
    );

    send(1, Tdata::Stat(Tstat { fid: 1 }), &mut socket);
}

fn send(tag: u16, content: Tdata, socket: &mut UnixStream) {
    let t = Tmessage { tag, content };
    println!("t-message: {t:?}");
    t.write_to(socket).unwrap();

    let r = Rmessage::read_from(socket).unwrap();
    println!("r-message: {r:?}\n");
}
