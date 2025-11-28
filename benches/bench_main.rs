use criterion::criterion_main;

mod benchmarks;

criterion_main! {
    benchmarks::rsc_pathological_regex::benches,
    benchmarks::burntsushi_torture_regex::benches,
    benchmarks::tui_render::benches,
    benchmarks::ts_update::benches,
    benchmarks::exec::benches,
    benchmarks::re_fast_match::benches,
    benchmarks::gap_buffer::benches,
}
