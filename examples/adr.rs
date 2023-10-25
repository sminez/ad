// A simple test of the regex engine using a user provided regex
use ad::regex::Regex;
use ropey::Rope;
use std::{env::args, fs};

fn main() {
    let args: Vec<String> = args().collect();
    let mut r = Regex::compile(&args[1]).expect("invalid regex");
    let s = fs::read_to_string(&args[2]).expect("unable to open file");
    let rope = Rope::from_str(&s);

    for m in r.match_rope_all(&rope) {
        for n in 0..10 {
            if let Some(txt) = m.rope_submatch_text(n, &rope) {
                let loc = m.sub_loc(n).unwrap();
                println!("${n} {loc:?} -> '{txt}'")
            }
        }
        println!();
    }
}
