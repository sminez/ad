use ad_editor::{buffer::GapBuffer, regex::Regex};
use criterion::{Criterion, criterion_group};
use std::hint::black_box;

fn inputs(n: usize) -> (GapBuffer, Regex) {
    let mut s = "🦊".repeat(n);
    s.push('\n');
    s = s.repeat(100);
    s.push_str("fooooooooobar");

    let r = Regex::compile("fo*bar").unwrap(); // typos:ignore
    let mut gb = GapBuffer::from(s);
    gb.make_contiguous();

    (gb, r)
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("RE_FAST");

    let (gb, mut r) = inputs(100);
    group.bench_function("leading 100 normal", |b| {
        b.iter(|| assert!(r.matches(black_box(&gb))))
    });
    group.bench_function("leading 100 fast", |b| {
        b.iter(|| assert!(r.matches(black_box(&gb))))
    });

    let (gb, mut r) = inputs(1000);
    group.bench_function("leading 1000 fast", |b| {
        b.iter(|| assert!(r.matches(black_box(&gb))))
    });

    group.bench_function("leading 1000 normal", |b| {
        b.iter(|| assert!(r.matches(black_box(&gb))))
    });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
