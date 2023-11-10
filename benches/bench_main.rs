use criterion::criterion_main;

mod benchmarks;

criterion_main! {
    benchmarks::rsc_pathological_regex::benches,
    benchmarks::burntsushi_torture_regex::benches,
}
