//! Benchmarking for the structural regular expression engine
use ad_editor::{
    buffer::{Buffer, GapBuffer},
    exec::{Program, SystemRunner},
};
use criterion::{Criterion, criterion_group};
use std::{
    env,
    hint::black_box,
    io::{self, Write},
};

// Starting point:
//   - around 3.4ms execution time but skewed left
//   - there's a small outlier cluster for both benchmarks
//   - execute on Buffer needs to track edits
//   - execute_on_string needs to construct the GapBuffer

const SCRIPT: &str = include_str!("../../examples/scripts/fancy_impl_blocks.ad");
const ZIPLIST: &str = include_str!("../../src/ziplist.rs");
const GAP_BUFFER: &str = include_str!("../../src/buffer/internal.rs");

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Structural regex");

    let prog = Program::try_parse(SCRIPT).expect("valid test script");
    let mut w = StdoutSink(Vec::with_capacity(10 * 1024));
    let mut runner = SystemRunner::new(env::current_dir().unwrap());

    for (name, s) in [("ziplist", ZIPLIST), ("internal", GAP_BUFFER)] {
        let mut buf = Buffer::new_unnamed(0, s, Default::default());
        let mut gb = GapBuffer::from(s);

        group.bench_function(format!("fancy impl blocks {name} (buffer)"), |b| {
            b.iter(|| prog.execute(black_box(&mut buf), &mut runner, name, black_box(&mut w)));
        });

        group.bench_function(format!("fancy impl blocks {name} (gap buffer)"), |b| {
            b.iter(|| prog.execute(black_box(&mut gb), &mut runner, name, black_box(&mut w)));
        });
    }

    group.finish();
}

criterion_group!(benches, criterion_benchmark);

struct StdoutSink(Vec<u8>);

impl Write for StdoutSink {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.0.write_all(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        let res = self.0.flush();
        self.0.clear();

        res
    }
}
