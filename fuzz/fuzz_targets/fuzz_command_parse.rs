#![no_main]

use ad_editor::editor::parse_command_fuzz;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: String| {
    parse_command_fuzz(&input);
});
