//! This is a little exploration of using async/await + a dummy Waker to simplify writing sans-io
//! state machine code.
use std::{
    cell::UnsafeCell,
    future::{Future, IntoFuture},
    io::{Cursor, Read},
    pin::{pin, Pin},
    sync::Arc,
    task::{Context, Poll, Wake, Waker},
};
use tokio::io::{AsyncRead, AsyncReadExt};

#[tokio::main]
async fn main() {
    // "Hello, 世界" with a u16 length header
    //
    // In 9p, data items of larger or variable lengths are represented by a two-byte field
    // specifying a count, n, followed by n bytes of data. Text strings are represented this way,
    // with the text itself stored as a UTF-8 encoded sequence of Unicode characters without a
    // trailing null byte.
    let mut cur = Cursor::new(vec![
        0x0d, 0x00, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0xe4, 0xb8, 0x96, 0xe7, 0x95, 0x8c,
    ]);

    println!(">> reading using std::io::Read");
    let s: String = read_9p_sync(&mut cur);
    println!("  got val: {s:?}\n");

    cur.set_position(0);

    println!(">> reading using tokio::io::AsyncRead");
    let s: String = read_9p_async(&mut cur).await;
    println!("  got val: {s:?}");
}

fn read_9p_sync<T, R>(r: &mut R) -> T
where
    T: Read9p,
    R: Read,
{
    let waker = Waker::from(Arc::new(StubWaker));
    let mut context = Context::from_waker(&waker);
    let s = State::default();

    // SAFETY: assumes the impl of Read9p is a valid future for us to poll
    let mut fut = unsafe { pin!(T::read(s.clone()).into_future()) };
    loop {
        match fut.as_mut().poll(&mut context) {
            Poll::Ready(val) => return val,
            Poll::Pending => unsafe {
                let n = (*s.0.get()).n;
                println!("{n} bytes requested");
                let mut buf = vec![0; n];
                r.read_exact(&mut buf).unwrap();
                (*s.0.get()).buf = Some(buf);
            },
        }
    }
}

async fn read_9p_async<T, R>(r: &mut R) -> T
where
    T: Read9p,
    R: AsyncRead + Unpin,
{
    let waker = Waker::from(Arc::new(StubWaker));
    let mut context = Context::from_waker(&waker);
    let s = State::default();

    // SAFETY: assumes the impl of Read9p is a valid future for us to poll
    let mut fut = unsafe { pin!(T::read(s.clone()).into_future()) };
    loop {
        match fut.as_mut().poll(&mut context) {
            Poll::Ready(val) => return val,
            Poll::Pending => unsafe {
                let n = (*s.0.get()).n;
                println!("{n} bytes requested");
                let mut buf = vec![0; n];
                r.read_exact(&mut buf).await.unwrap();
                (*s.0.get()).buf = Some(buf);
            },
        }
    }
}

/// A no-op waker that is just used to create a [Context] in order to poll the [Read9p] future.
struct StubWaker;
impl Wake for StubWaker {
    fn wake(self: Arc<Self>) {}
    fn wake_by_ref(self: &Arc<Self>) {}
}

/// Helper struct for awaiting a Future that returns pending once so we can return control to the
/// poll loop and perform IO.
struct Yield(bool);
impl Future for Yield {
    type Output = ();
    fn poll(mut self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<()> {
        if self.0 {
            Poll::Ready(())
        } else {
            self.0 = true;
            Poll::Pending
        }
    }
}

/// Shared state between a [NineP] impl and a parent read loop that is performing IO.
#[derive(Default, Debug, Clone)]
pub struct State(pub(crate) Arc<UnsafeCell<StateInner>>);

// SAFETY: StateInner is only accessable in this crate
unsafe impl Send for State {}
// SAFETY: StateInner is only accessable in this crate
unsafe impl Sync for State {}

#[derive(Default, Debug)]
struct StateInner {
    n: usize,
    buf: Option<Vec<u8>>,
}

/// Request a specific number of bytes from the parent poll loop and then yield to that poll loop
/// so it can perform IO and provide the requested data.
macro_rules! request_bytes {
    ($s:expr, $n:expr) => {{
        (*$s.0.get()).n = $n;
        Yield(false).await;
        (*$s.0.get()).buf.take().unwrap()
    }};
}

/// # Safety
/// The read method of this trait requires that you only yield view the [request_bytes] macro.
unsafe trait Read9p {
    /// # Safety
    /// Implementations of `read` need to ensure that the only await points they contain are
    /// from calls to the [request_bytes] macro.
    unsafe fn read(state: State) -> impl Future<Output = Self> + Send;
}

#[allow(async_fn_in_trait)]
unsafe impl Read9p for String {
    async unsafe fn read(state: State) -> Self {
        let n = size_of::<u16>();
        let buf = request_bytes!(state, n);
        let data = buf[0..n].try_into().unwrap();
        let len = u16::from_le_bytes(data) as usize;
        let buf = request_bytes!(state, len);

        String::from_utf8(buf).unwrap()
    }
}
