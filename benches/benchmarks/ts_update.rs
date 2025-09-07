// Update times for running TreeSitter syntax highlighting
use ad_editor::{Config, buffer::Buffer, dot::TextObject, editor::Action};
use ad_event::Source;
use criterion::{Criterion, criterion_group};
use std::{
    env::current_dir,
    sync::{Arc, Mutex},
};

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("TS update");

    let repo_root = current_dir().unwrap();
    let path = repo_root.join("src/term.rs").canonicalize().unwrap();
    let config = Arc::new(Mutex::new(Config::try_load().unwrap()));

    let mut buf = Buffer::new_from_canonical_file_path(0, path.clone(), config.clone()).unwrap();
    buf.update_ts_state(0, 70);

    group.bench_function("update without edit", |b| {
        b.iter(|| {
            buf.update_ts_state(0, 70);
        })
    });

    let mut buf = Buffer::new_from_canonical_file_path(0, path.clone(), config.clone()).unwrap();
    buf.update_ts_state(0, 70);

    group.bench_function("append newline and update", |b| {
        b.iter(|| {
            buf.handle_action(Action::DotSet(TextObject::BufferEnd, 1), Source::Fsys);
            buf.handle_action(Action::InsertChar { c: '\n' }, Source::Fsys);
            buf.update_ts_state(0, 70);
        })
    });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
