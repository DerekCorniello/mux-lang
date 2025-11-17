use std::ffi::CString;
use std::fmt;
use std::os::raw::c_char;

use crate::Value;

#[derive(Clone, Debug)]
pub struct Bool(pub bool);

impl Bool {

    pub fn to_int(&self) -> i64 {
        if self.0 { 1 } else { 0 }
    }
}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", if self.0 { "true" } else { "false" })
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_bool_to_string(b: i32) -> *mut c_char {
    eprintln!("mux_bool_to_string called with b: {}", b);
    let s = format!("{}", Bool(b != 0));
    CString::new(s).unwrap().into_raw()
}

/// # Safety
/// v must be a valid pointer to a Value::Bool.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_bool_from_value(v: *mut Value) -> i32 {
    if let Value::Bool(b) = unsafe { &*v } {
        if *b { 1 } else { 0 }
    } else {
        0
    }
}

/// # Safety
/// v must be a valid pointer to a Value::Bool.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_bool_to_int(v: *mut Value) -> *mut Value {
    if let Value::Bool(b) = unsafe { &*v } {
        Box::into_raw(Box::new(Value::Int(Bool(*b).to_int())))
    } else {
        panic!("Expected Bool value");
    }
}

/// # Safety
/// v must be a valid pointer to a Value::Bool.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_bool_to_float(v: *mut Value) -> *mut Value {
    if let Value::Bool(b) = unsafe { &*v } {
        Box::into_raw(Box::new(Value::Float(ordered_float::OrderedFloat(Bool(*b).to_int() as f64))))
    } else {
        panic!("Expected Bool value");
    }
}
