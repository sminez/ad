// This is the pathological case that Russ Cox covers in his article which leads
// to exponential behaviour in backtracking based implementations.
//
// The graph from the article can be found here:
//   https://swtch.com/~rsc/regexp/grep1p.png
use ad::regex::Regex;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn rsc_inputs(n: usize) -> (String, Regex) {
    let s = "a".repeat(n);
    let mut re = "a?".repeat(n);
    re.push_str(&s);
    let r = Regex::compile(&re).unwrap();

    (s, r)
}

fn rsc_pathological_case(n: usize) {
    let (s, mut r) = rsc_inputs(n);
    assert!(r.match_str(&s).is_some());
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("rsc 100");

    group.bench_function("with compile", |b| {
        b.iter(|| rsc_pathological_case(black_box(100)))
    });

    let (s, mut r) = rsc_inputs(100);
    group.bench_function("without compile", |b| {
        b.iter(|| assert!(r.match_str(black_box(&s)).is_some()))
    });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
