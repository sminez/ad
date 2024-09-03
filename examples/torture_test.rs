//! A one-shot run of Burntsushi's full torture test for the regex engine
//!
//! See the full benchmark tests in '/benches' for a trimmed down version of this test that
//! runs under criterion. (Cut down because the ad engine is not fast enough currently to
//! handle the full test in a reasonable time.)
use ad_editor::regex::Regex;
use std::{fs, time::Instant};

fn main() {
    let s = fs::read_to_string("test-data/burnt-sushi-torture-test/torture-test.haystack").unwrap();
    let re = fs::read_to_string("test-data/burnt-sushi-torture-test/torture-test.pattern").unwrap();

    println!("Haystack length: {}", s.len());
    println!("Pattern length: {}", re.len());

    let t1 = Instant::now();
    let mut r = Regex::compile(re.trim_end()).unwrap();
    let d_compile = Instant::now().duration_since(t1).as_micros();
    println!("Compile time: {d_compile} microseconds");

    let t1 = Instant::now();
    assert!(r.matches_str(&s));
    let d_match = Instant::now().duration_since(t1).as_secs();
    println!("Match time: {d_match} seconds");
}
