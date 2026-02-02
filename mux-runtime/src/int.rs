use std::ffi::CString;
use std::fmt;
use std::os::raw::c_char;

use crate::result::MuxResult;
use crate::Value;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Int(pub i64);

impl Int {
    pub fn to_float(&self) -> f64 {
        self.0 as f64
    }

    pub fn add(&self, other: &Int) -> Int {
        Int(self.0 + other.0)
    }

    pub fn sub(&self, other: &Int) -> Int {
        Int(self.0 - other.0)
    }

    pub fn mul(&self, other: &Int) -> Int {
        Int(self.0 * other.0)
    }

    pub fn div(&self, other: &Int) -> Result<Int, String> {
        if other.0 == 0 {
            Err("Division by zero".to_string())
        } else {
            Ok(Int(self.0 / other.0))
        }
    }

    pub fn rem(&self, other: &Int) -> Result<Int, String> {
        if other.0 == 0 {
            Err("Modulo by zero".to_string())
        } else {
            Ok(Int(self.0 % other.0))
        }
    }

    pub fn lt(&self, other: &Int) -> bool {
        self.0 < other.0
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_to_string(i: i64) -> *mut c_char {
    let s = format!("{}", Int(i));
    // Safe: format! produces valid UTF-8 without null bytes
    CString::new(s)
        .expect("format output should be valid UTF-8")
        .into_raw()
}

/// # Safety
/// v must be a valid pointer to a Value::Int.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_int_from_value(v: *mut crate::Value) -> i64 {
    if let crate::Value::Int(i) = unsafe { &*v } {
        *i
    } else {
        panic!("Expected Int value");
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_add(a: i64, b: i64) -> i64 {
    Int(a).add(&Int(b)).0
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_sub(a: i64, b: i64) -> i64 {
    Int(a).sub(&Int(b)).0
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_mul(a: i64, b: i64) -> i64 {
    Int(a).mul(&Int(b)).0
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_div(a: i64, b: i64) -> *mut MuxResult {
    match Int(a).div(&Int(b)) {
        Ok(i) => Box::into_raw(Box::new(MuxResult::ok(Value::Int(i.0)))),
        Err(e) => Box::into_raw(Box::new(MuxResult::err(e))),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_rem(a: i64, b: i64) -> i64 {
    match Int(a).rem(&Int(b)) {
        Ok(i) => i.0,
        Err(_) => 0,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_eq(a: i64, b: i64) -> bool {
    Int(a) == Int(b)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_lt(a: i64, b: i64) -> bool {
    Int(a) < Int(b)
}
