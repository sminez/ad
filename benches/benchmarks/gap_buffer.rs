//! GapBuffer benchmarks for measuring line-ending tracking performance.
use ad_editor::buffer::GapBuffer;
use criterion::{BatchSize, BenchmarkId, Criterion, criterion_group};
use libflate::gzip::Decoder;
use serde::Deserialize;
use std::{
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TestData {
    start_content: String,
    #[serde(rename = "txns")]
    transactions: Vec<TestTransaction>,
}

impl TestData {
    fn load(fname: &str) -> Self {
        let file = File::open(fname).expect("failed to open trace file");
        let input = BufReader::new(file);
        let mut decoder = Decoder::new(input).expect("failed to create decoder");
        let mut raw_json = vec![];
        decoder
            .read_to_end(&mut raw_json)
            .expect("failed to decompress");

        serde_json::from_reader(raw_json.as_slice()).expect("failed to parse JSON")
    }
}

#[derive(Debug, Clone, Deserialize)]
struct TestTransaction {
    patches: Vec<TestPatch>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(from = "(usize, usize, String)")]
struct TestPatch {
    position: usize,
    delete_len: usize,
    insert_content: String,
}

impl From<(usize, usize, String)> for TestPatch {
    fn from((position, delete_len, insert_content): (usize, usize, String)) -> Self {
        Self {
            position,
            delete_len,
            insert_content,
        }
    }
}

fn apply_patch(patch: &TestPatch, gb: &mut GapBuffer) {
    if patch.delete_len > 0 {
        gb.remove_range(patch.position, patch.position + patch.delete_len);
    }
    gb.insert_str(patch.position, &patch.insert_content);
}

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

fn realistic_editing(c: &mut Criterion) {
    let mut group = c.benchmark_group("realistic_editing");

    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/reference-tests/data/rustcode.json.gz"
    );
    let test_data = TestData::load(path);
    let transactions: &[TestTransaction] = &test_data.transactions[..1000];

    group.bench_function("rustcode_1000_txns", |b| {
        b.iter_batched(
            || GapBuffer::from(test_data.start_content.as_str()),
            |mut gb| {
                for tx in transactions {
                    for patch in &tx.patches {
                        apply_patch(patch, &mut gb);
                    }
                }
            },
            BatchSize::LargeInput,
        );
    });

    group.finish();
}

criterion_group!(benches, single_char_insert, realistic_editing);
