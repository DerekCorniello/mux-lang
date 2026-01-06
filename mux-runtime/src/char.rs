use std::ffi::CString;
use std::os::raw::c_char;

/// Convert a char (i32/Unicode code point) to a string
#[unsafe(no_mangle)]
pub extern "C" fn mux_char_to_string(c: i32) -> *mut c_char {
    let ch = char::from_u32(c as u32).unwrap_or('\u{FFFD}');
    let s = ch.to_string();
    CString::new(s).unwrap().into_raw()
}

/// Check if two chars are equal
#[unsafe(no_mangle)]
pub extern "C" fn mux_char_eq(a: i32, b: i32) -> bool {
    a == b
}

/// Check if char a is less than char b
#[unsafe(no_mangle)]
pub extern "C" fn mux_char_lt(a: i32, b: i32) -> bool {
    a < b
}
