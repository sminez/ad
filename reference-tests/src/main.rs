use ad_editor::{Config, buffer::Buffer};
use reference_tests::TestData;
use std::{
    env, fs,
    sync::{Arc, RwLock},
};

fn main() {
    let mut args = env::args().skip(1);
    let fname = args
        .next()
        .expect("usage: reference_tests <test-file> <steps>");
    let steps = args
        .next()
        .map(|s| {
            s.parse::<usize>()
                .expect("usage: reference_tests <test-file> <steps>")
        })
        .unwrap_or(usize::MAX);

    let mut test_data = TestData::try_load_from_file(&fname).unwrap();

    let mut cfg = Config::default();
    cfg.editor.expand_tab = false;
    cfg.editor.match_indent = false;

    let mut b = Buffer::new_unnamed(0, &test_data.start_content, Arc::new(RwLock::new(cfg)));

    test_data.apply_n(&mut b, steps);
    fs::write("output/rust_output.txt", b.str_contents()).unwrap();
}
