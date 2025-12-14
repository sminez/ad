#![no_main]

use ad_editor::PlumbingRules;
use libfuzzer_sys::fuzz_target;
use std::str::FromStr;

fuzz_target!(|input: String| {
    _ = PlumbingRules::from_str(&input);
});
