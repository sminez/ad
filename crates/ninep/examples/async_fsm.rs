//! This is a little exploration of using async/await + a dummy Waker to simplify writing sans-io
//! state machine code.
use futures::pending;
use std::{
    cell::RefCell,
    future::{Future, IntoFuture},
    io::Read,
    pin::pin,
    rc::Rc,
    sync::Arc,
    task::{Context, Poll, Wake, Waker},
};

fn main() {
    // "Hello, 世界" with a u16 length header
    //
    // From [INTRO(5)](http://man.cat-v.org/plan_9/5/intro):
    //   Data items of larger or variable lengths are represented by a two-byte field specifying
    //   a count, n, followed by n bytes of data. Text strings are represented this way, with
    //   the text itself stored as a UTF-8 encoded sequence of Unicode charac- ters (see utf(6)).
    //
    //   Text strings in 9P messages are not NUL- terminated: n counts the bytes of UTF-8 data,
    //   which include no final zero byte.  The NUL character is illegal in all text strings
    //   in 9P, and is therefore excluded from file names, user names, and so on.
    let data = &[
        0x0d, 0x00, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0xe4, 0xb8, 0x96, 0xe7, 0x95, 0x8c,
    ];
    let r = StringReader::default();
    let val = read_9p_sync_from_bytes(r, data);

    println!("got val: {val:?}");
}

/// A no-op waker that is just used to create a [Context] in order to poll the [Read9p] future.
struct StubWaker;
impl Wake for StubWaker {
    fn wake(self: Arc<Self>) {}
    fn wake_by_ref(self: &Arc<Self>) {}
}

/// A real impl of a reader function would source the bytes here from IO rather than reading from a Vec.
fn read_9p_sync_from_bytes<R: Read9p>(r: R, data: &[u8]) -> R::T {
    let waker = Waker::from(Arc::new(StubWaker));
    let mut context = Context::from_waker(&waker);
    let mut offset = 0;

    let mut s = r.state();
    let mut fut = pin!(r.read().into_future());

    loop {
        match fut.as_mut().poll(&mut context) {
            Poll::Ready(val) => {
                return val;
            }
            Poll::Pending => {
                let n = s.bytes_needed();
                println!("{n} bytes requested");
                s.set_buf(data[offset..offset + n].to_vec());
                offset += n;
            }
        }
    }
}

#[derive(Default, Debug, Clone)]
struct State(Rc<RefCell<StateInner>>);
impl State {
    fn request_bytes(&mut self, n: usize) {
        self.0.borrow_mut().n = n;
    }

    fn bytes_needed(&self) -> usize {
        self.0.borrow().n
    }

    fn set_buf(&mut self, buf: Vec<u8>) {
        self.0.borrow_mut().buf = buf;
    }

    fn buf(&self) -> Vec<u8> {
        self.0.borrow().buf.clone()
    }
}

#[derive(Default, Debug)]
struct StateInner {
    n: usize,
    buf: Vec<u8>,
}

#[allow(async_fn_in_trait)]
trait Read9p {
    type T;

    fn state(&self) -> State;
    async fn read(self) -> Self::T;
}

#[derive(Default)]
struct StringReader {
    s: State,
}

impl Read9p for StringReader {
    type T = String;

    fn state(&self) -> State {
        self.s.clone()
    }

    async fn read(mut self) -> String {
        let n = size_of::<u16>();
        self.s.request_bytes(n);
        pending!();

        let data = self.s.buf()[0..n].try_into().unwrap();
        let len = u16::from_le_bytes(data) as usize;
        self.s.request_bytes(len);
        pending!();

        let mut s = String::with_capacity(len);
        self.s.buf().as_slice().read_to_string(&mut s).unwrap();

        s
    }
}
