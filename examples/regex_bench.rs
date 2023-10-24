//! Testing the performance of the regex implementation
use ad::regex::Regex;
use std::time::Instant;

// This is the pathological case that Russ Cox covers in his article which leads
// to exponential behaviour in backtracking based implementations.
//
// The graph from the article can be found here:
//   https://swtch.com/~rsc/regexp/grep1p.png
fn main() {
    println!("Matching VM");

    let s = "a".repeat(100);
    let mut re = "a?".repeat(100);
    re.push_str(&s);

    let t1 = Instant::now();
    let mut r = Regex::compile(&re).unwrap();
    let d_compile = Instant::now().duration_since(t1).as_micros();

    println!("Compile time: {d_compile} microseconds");

    let mut durations = Vec::with_capacity(100);

    for n in 0..100 {
        let t1 = Instant::now();
        assert!(r.match_str(&s).is_some(), "n={n}");
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
