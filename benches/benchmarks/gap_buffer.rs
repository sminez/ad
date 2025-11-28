//! GapBuffer benchmarks for measuring line-ending tracking performance.
use ad_editor::buffer::GapBuffer;
use criterion::{BatchSize, BenchmarkId, Criterion, criterion_group};

fn single_char_insert(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_char_insert");

    for n_lines in [100, 1_000, 10_000, 50_000] {
        let content: String = (0..n_lines).map(|_| "x".repeat(79) + "\n").collect();
        let n_chars = n_lines * 80;

        let positions: [(&str, usize); 5] = [
            ("start", 0),
            ("quarter", n_chars / 4),
            ("middle", n_chars / 2),
            ("three_quarter", 3 * n_chars / 4),
            ("end", n_chars.saturating_sub(1)),
        ];

        for (name, char_pos) in positions {
            group.bench_with_input(
                BenchmarkId::new(name, n_lines),
                &(content.clone(), char_pos),
                |b, (content, pos)| {
                    b.iter_batched(
                        || GapBuffer::from(content.as_str()),
                        |mut gb| gb.insert_char(*pos, 'a'),
                        BatchSize::SmallInput,
                    );
                },
            );
        }
    }
    group.finish();
}

criterion_group!(benches, single_char_insert);
