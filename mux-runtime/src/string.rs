use std::ffi::{CStr, CString};
use std::fmt;
use std::os::raw::c_char;

use ordered_float;

use crate::result::MuxResult;
use crate::Value;

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
}

impl fmt::Display for MuxString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// # Safety
/// v must be a valid pointer to a Value::String.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_string_from_value(v: *mut Value) -> *mut c_char {
    if let Value::String(s) = unsafe { &*v } {
        CString::new(s.clone()).unwrap().into_raw()
    } else {
        CString::new("".to_string()).unwrap().into_raw() // Return empty string instead of panicking
    }
}

/// # Safety
/// v must be a valid pointer to a Value.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_value_get_string(v: *mut Value) -> *mut c_char {
    if let Value::String(s) = unsafe { &*v } {
        CString::new(s.clone()).unwrap().into_raw()
    } else {
        CString::new("".to_string()).unwrap().into_raw() // Return empty string instead of panicking
    }
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
    CString::new(result.0).unwrap().into_raw()
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
pub extern "C" fn mux_string_to_string(s: *const c_char) -> *mut c_char {
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy();
    std::ffi::CString::new(rust_str.as_ref())
        .unwrap()
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
    Box::into_raw(Box::new(value))
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
        if a_str == b_str {
            1
        } else {
            0
        }
    }
}

/// Compare two C strings for inequality
/// Returns 1 if not equal, 0 if equal
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_not_equal(a: *const c_char, b: *const c_char) -> i32 {
    if mux_string_equal(a, b) == 1 {
        0
    } else {
        1
    }
}
