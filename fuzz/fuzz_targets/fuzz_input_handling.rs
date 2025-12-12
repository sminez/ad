#![no_main]

use ad_editor::{Editor, key::Input};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
});
