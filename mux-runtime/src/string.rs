use std::ffi::{CStr, CString};
use std::fmt;
use std::os::raw::c_char;

use ordered_float;

use crate::Value;
use crate::refcount::mux_rc_alloc;
use crate::result::MuxResult;

#[derive(Clone, Debug)]
pub struct MuxString(pub String);

impl MuxString {
    pub fn to_int(&self) -> Result<i64, String> {
        self.0.parse().map_err(|_| "Invalid integer".to_string())
    }

    pub fn to_float(&self) -> Result<f64, String> {
        self.0.parse().map_err(|_| "Invalid float".to_string())
    }

    pub fn concat(&self, other: &MuxString) -> MuxString {
        MuxString(self.0.clone() + &other.0)
    }

    pub fn length(&self) -> i64 {
        self.0.len() as i64
    }

    pub fn hash(&self) -> i64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        self.0.hash(&mut hasher);
        hasher.finish() as i64
    }
}

impl fmt::Display for MuxString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// # Safety
/// Borrows `v` and clones the string data. Does NOT take ownership of `v`.
/// Returns a new C string that caller must free with `mux_free_string`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_string_from_value(v: *mut Value) -> *mut c_char {
    if let Value::String(s) = unsafe { &*v } {
        // Safe: s is a valid UTF-8 String from the Mux runtime
        CString::new(s.clone())
            .expect("valid UTF-8 String should convert to CString")
            .into_raw()
    } else {
        // Safe: empty string is valid UTF-8
        CString::new("".to_string())
            .expect("empty string should convert to CString")
            .into_raw()
    }
}

/// # Safety
/// Borrows `v` and clones the string data. Does NOT take ownership of `v`.
/// Returns a new C string that caller must free with `mux_free_string`.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_value_get_string(v: *mut Value) -> *mut c_char {
    unsafe { mux_string_from_value(v) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_to_int(s: *const c_char) -> *mut MuxResult {
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy();
    match MuxString(rust_str.to_string()).to_int() {
        Ok(i) => Box::into_raw(Box::new(MuxResult::ok(Value::Int(i)))),
        Err(e) => Box::into_raw(Box::new(MuxResult::err(e))),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_to_float(s: *const c_char) -> *mut MuxResult {
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy();
    match MuxString(rust_str.to_string()).to_float() {
        Ok(f) => Box::into_raw(Box::new(MuxResult::ok(Value::Float(
            ordered_float::OrderedFloat(f),
        )))),
        Err(e) => Box::into_raw(Box::new(MuxResult::err(e))),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    let a_str = unsafe { CStr::from_ptr(a).to_string_lossy() };
    let b_str = unsafe { CStr::from_ptr(b).to_string_lossy() };
    let result = MuxString(a_str.to_string()).concat(&MuxString(b_str.to_string()));
    // Safe: result is a valid UTF-8 String
    CString::new(result.0)
        .expect("valid UTF-8 String should convert to CString")
        .into_raw()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_length(s: *const c_char) -> i64 {
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy();
    MuxString(rust_str.to_string()).length()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_hash(s: *const c_char) -> i64 {
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy();
    MuxString(rust_str.to_string()).hash()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_contains(haystack: *const Value, needle: *const Value) -> bool {
    unsafe {
        if let (Value::String(haystack_str), Value::String(needle_str)) = (&*haystack, &*needle) {
            haystack_str.contains(needle_str)
        } else {
            false
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_contains_char(haystack: *const Value, needle: i64) -> bool {
    let haystack_str = unsafe {
        match &*haystack {
            Value::String(s) => s,
            _ => return false,
        }
    };
    let Some(ch) = char::from_u32(needle as u32) else {
        return false;
    };
    haystack_str.contains(ch)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_to_string(s: *const c_char) -> *mut c_char {
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy();
    // Safe: rust_str is lossily converted but valid UTF-8
    std::ffi::CString::new(rust_str.as_ref())
        .expect("lossily converted string should contain no null bytes")
        .into_raw()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_new_string_from_cstr(s: *const c_char) -> *mut Value {
    if s.is_null() {
        return std::ptr::null_mut();
    }
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy().to_string();
    let value = Value::String(rust_str);
    mux_rc_alloc(value)
}

/// Compare two C strings for equality
/// Returns 1 if equal, 0 if not equal
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_equal(a: *const c_char, b: *const c_char) -> i32 {
    if a.is_null() || b.is_null() {
        return if a == b { 1 } else { 0 };
    }
    unsafe {
        let a_str = CStr::from_ptr(a);
        let b_str = CStr::from_ptr(b);
        if a_str == b_str { 1 } else { 0 }
    }
}

/// Compare two C strings for inequality
/// Returns 1 if not equal, 0 if equal
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_not_equal(a: *const c_char, b: *const c_char) -> i32 {
    if mux_string_equal(a, b) == 1 { 0 } else { 1 }
}

/// Convert a character to its integer value
/// Only works for digit characters '0'-'9'
/// Returns Result<int, str>
#[unsafe(no_mangle)]
pub extern "C" fn mux_char_to_int(c: i64) -> *mut MuxResult {
    if let Some(ch) = char::from_u32(c as u32) {
        if ch.is_ascii_digit() {
            let digit = (ch as u8 - b'0') as i64;
            Box::into_raw(Box::new(MuxResult::ok(Value::Int(digit))))
        } else {
            Box::into_raw(Box::new(MuxResult::err(
                "Character is not a digit (0-9)".to_string(),
            )))
        }
    } else {
        Box::into_raw(Box::new(MuxResult::err("Invalid character".to_string())))
    }
}

/// Convert a character (i64) to a string
#[unsafe(no_mangle)]
pub extern "C" fn mux_char_to_string(c: i64) -> *mut c_char {
    if let Some(ch) = char::from_u32(c as u32) {
        let s = ch.to_string();
        // Safe: char::to_string() returns valid UTF-8
        CString::new(s)
            .expect("char conversion should produce valid UTF-8")
            .into_raw()
    } else {
        // Safe: empty string is valid UTF-8 (invalid char converted to empty)
        CString::new("")
            .expect("empty string should convert to CString")
            .into_raw()
    }
}
