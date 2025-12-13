#![no_main]

use ad_editor::exec::Addr;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: String| {
    _ = Addr::parse(&input);
});
