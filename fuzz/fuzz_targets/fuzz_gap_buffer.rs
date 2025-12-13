#![no_main]

use ad_editor::buffer::GapBuffer;
use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use std::cmp::{max, min};

fuzz_target!(|data: FuzzData| {
    let mut gb = GapBuffer::from(data.initial_content);
    for action in data.actions.into_iter() {
        match action {
            TestAction::InsertChar(idx, ch) if idx < gb.len_chars() => gb.insert_char(idx, ch),
            TestAction::InsertString(idx, s) if idx < gb.len_chars() => gb.insert_str(idx, &s),
            TestAction::RemoveChar(idx) if idx < gb.len_chars() => gb.remove_char(idx),
            TestAction::RemoveRange(a, b) => {
                let (from, to) = (min(a, b), max(a, b));
                if to < gb.len_chars() {
                    gb.remove_range(from, to);
                }
            }

            _ => (), // no-op due to being out of range
        }
    }

    gb.to_string(); // we should be able to stringify the buffer
});

#[derive(Debug, Arbitrary)]
struct FuzzData {
    initial_content: String,
    actions: Vec<TestAction>,
}

#[derive(Debug, Arbitrary)]
enum TestAction {
    InsertChar(usize, char),
    InsertString(usize, String),
    RemoveChar(usize),
    RemoveRange(usize, usize),
}
