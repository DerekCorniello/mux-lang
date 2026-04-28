#![no_main]

use libfuzzer_sys::fuzz_target;
use mux_runtime::json::Json;

fuzz_target!(|data: &[u8]| {
    let input = String::from_utf8_lossy(data);
    let _ = Json::parse(&input);
});
