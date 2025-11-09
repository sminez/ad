// This is the pathological case that Russ Cox covers in his article which leads
// to exponential behaviour in backtracking based implementations.
//
// The graph from the article can be found here:
//   https://swtch.com/~rsc/regexp/grep1p.png

use ad_editor::regex::Regex;
use criterion::{Criterion, criterion_group};
use std::hint::black_box;

fn rsc_inputs(n: usize) -> (String, Regex) {
    let s = "a".repeat(n);
    let mut re = "a?".repeat(n);
    re.push_str(&s);
    let r = Regex::compile(&re).unwrap();

    (s, r)
}

fn rsc_pathological_case(n: usize) {
    let (s, r) = rsc_inputs(n);
    assert!(r.matches(&s.as_str()));
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("rsc 100");

    group.bench_function("with compile", |b| {
        b.iter(|| rsc_pathological_case(black_box(100)))
    });

    let (s, r) = rsc_inputs(100);
    group.bench_function("without compile", |b| {
        b.iter(|| assert!(r.matches(black_box(&s.as_str()))))
    });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
