#![no_main]

use ad_editor::regex::Regex;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: String| {
    _ = Regex::compile(&input);
});
