//! Api demonstration of the regex implementation
//! -> Running on my x270:
//!
//! $ ./target/release/examples/regex_demo
//! regex: "([0-9]+)-([0-9]+)"
//! input "this should work 123-456 other stuff"
//! compile time (micro seconds): 15
//!
//! :: matching against a str
//! match time (micro seconds): 4
//! Submatch $1: Some("123")
//! Submatch $2: Some("456")
//! Match ($0):  "123-456"

use ad_editor::{buffer::GapBuffer, regex::Regex};
use std::time::Instant;

fn main() {
    let mut haystack = "🦊".repeat(10000);
    haystack.push('\n');
    haystack = haystack.repeat(100);
    haystack.push_str("this should work 123-456 other stuff");

    let re = "([0-9]+)-([0-9]+)";
    let t1 = Instant::now();
    let mut r = Regex::compile(re).unwrap();
    let d_compile = Instant::now().duration_since(t1).as_micros();
    println!("compile time (micro seconds): {d_compile}");

    let mut gb = GapBuffer::from(haystack);
    gb.make_contiguous();

    println!("\n:: matching against a gap buffer");
    let t1 = Instant::now();
    let m = r.find(&gb).unwrap();
    let d_match = Instant::now().duration_since(t1).as_millis();
    println!("match time (ms): {d_match}");

    println!("Submatch $1: {:?}", m.str_submatch_text(1, gb.as_str()));
    println!("Submatch $2: {:?}", m.str_submatch_text(2, gb.as_str()));
    println!("Match ($0):  {:?}", m.str_match_text(gb.as_str()));
}
