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
//!
//! :: matching against a Rope
//! match time (micro seconds): 2
//! Submatch $1: Some(["123"])
//! Submatch $2: Some(["456"])
//! Match ($0):  ["123-456"]

use ad::regex::Regex;
use ropey::Rope;
use std::time::Instant;

fn main() {
    let re = "([0-9]+)-([0-9]+)";
    let s = "this should work 123-456 other stuff";
    let rope = Rope::from_str(s);

    println!("regex: {re:?}\ninput {s:?}");

    let t1 = Instant::now();
    let mut r = Regex::compile(re).unwrap();
    let d_compile = Instant::now().duration_since(t1).as_micros();
    println!("compile time (micro seconds): {d_compile}");

    println!("\n:: matching against a str");
    let t1 = Instant::now();
    let m = r.match_str(s).unwrap();
    let d_match = Instant::now().duration_since(t1).as_micros();
    println!("match time (micro seconds): {d_match}");

    println!("Submatch $1: {:?}", m.str_submatch_text(1, s));
    println!("Submatch $2: {:?}", m.str_submatch_text(2, s));
    println!("Match ($0):  {:?}", m.str_match_text(s));

    println!("\n:: matching against a Rope");
    let t1 = Instant::now();
    let m = r.match_rope(&rope).unwrap();
    let d_match = Instant::now().duration_since(t1).as_micros();
    println!("match time (micro seconds): {d_match}");

    println!("Submatch $1: {:?}", m.rope_submatch_text(1, &rope));
    println!("Submatch $2: {:?}", m.rope_submatch_text(2, &rope));
    println!("Match ($0):  {:?}", m.rope_match_text(&rope));
}
