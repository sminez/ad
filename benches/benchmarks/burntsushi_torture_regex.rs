// This is a variation on the torture test used by Burntsushi in their repo looking at
// how to translate rsc's C code implementing the Thompson NFA algorithm into Rust.
//
// For now I'm using a much smaller input than the one Burntsushi uses (475 alternations
// of in the regex itself and a haystack of 43,000 repetitions of 'abc' before the final
// Z) because my VM is clearly far to slow at the moment and a single match takes multiple
// seconds.
//
// As I get the performance improved I'll take a look at ramping up the torture.
//
//  https://github.com/BurntSushi/rsc-regexp/blob/master/torture-test
use ad::regex::Regex;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn burntsushi_inputs(n_alts: usize, n_reps: usize) -> (String, Regex) {
    let mut re = "(abc)*d|".repeat(n_alts);
    re.push_str("(abc)*Z");

    let mut s = "abc".repeat(n_reps);
    s.push('Z');
    let r = Regex::compile(&re).unwrap();

    (s, r)
}

fn burntsushi_pathological_case(n_alts: usize, n_reps: usize) {
    let (s, mut r) = burntsushi_inputs(n_alts, n_reps);
    assert!(r.match_str(&s).is_some());
}

fn criterion_benchmark(c: &mut Criterion) {
    // > The full torture test Burntsushi used would be 475, 43000
    let n_alts = 100;
    let n_reps = 5000;
    let mut group = c.benchmark_group(format!("burntsushi {n_alts} alts, {n_reps} reps"));

    group.bench_function("with compile", |b| {
        b.iter(|| burntsushi_pathological_case(black_box(n_alts), black_box(n_reps)))
    });

    let (s, mut r) = burntsushi_inputs(n_alts, n_reps);
    group.bench_function("without compile", |b| {
        b.iter(|| assert!(r.match_str(black_box(&s)).is_some()))
    });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
