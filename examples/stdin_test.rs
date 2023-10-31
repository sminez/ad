use ad::{CachedStdin, IterableStream};

fn main() {
    let stdin = CachedStdin::new();

    for (n, ch) in stdin.iter_between(0, usize::MAX) {
        println!("{n}: >{ch}<, {}", ch.len_utf8());
    }

    println!("FULL: >{}<", stdin.contents());
}
