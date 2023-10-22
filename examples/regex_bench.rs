//! Testing the performance of the regex implementation
use ad::regex::Regex;
use std::time::Instant;

// This is the pathological case that Russ Cox covers in his article which leads
// to exponential behaviour in backtracking based implementations.
//
// The graph from the article can be found here:
//   https://swtch.com/~rsc/regexp/grep1p.png
//
// His implementation of Thompson's NFA algorithm runs for the case being benchmarked
// here in just under 200 microseconds, with Perl 5.8.7 requiring an estimated 10^15
// years. In comparison, on a release build my current approach seems to run in around
// the same time of an average of 150 microseconds when matching without the DFA cache
// and around 1 microsecond once the cache is in place. Compilation of the regex itself
// takes around 25 microseconds.
fn main() {
    println!("Matching without DFA cache");
    time_match(false);
    println!("\n\nMatching with DFA cache");
    time_match(true);
}

fn compile(report_compile_time: bool) -> (Regex, String) {
    let s = "a".repeat(100);
    let mut re = "a?".repeat(100);
    re.push_str(&s);

    let t1 = Instant::now();
    let r = Regex::compile(&re).unwrap();
    let d_compile = Instant::now().duration_since(t1).as_micros();

    if report_compile_time {
        println!("Compile time: {d_compile} microseconds");
    }

    (r, s)
}

fn time_match(use_cache: bool) {
    let (mut r, s) = compile(true);
    let mut durations = Vec::with_capacity(100);

    for _ in 0..100 {
        if !use_cache {
            (r, _) = compile(false);
        }
        let t1 = Instant::now();
        assert!(r.matches_str(&s));
        durations.push(Instant::now().duration_since(t1).as_micros());
        print!(".");
    }
    println!();

    let raw = format!("Raw durations:\n{durations:?}");
    durations.sort_unstable();

    let min = durations[0];
    let max = durations[99];
    let mean = (durations.iter().sum::<u128>() as f32 / 100.0).round() as usize;
    println!("Match time:\n  min: {min}\n  max: {max}\n  mean: {mean}\n\n{raw}");
}
