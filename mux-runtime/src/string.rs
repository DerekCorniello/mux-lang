use std::ffi::{CStr, CString};
use std::fmt;
use std::os::raw::c_char;

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

    pub fn substring(&self, start: i64, len: i64) -> Result<MuxString, String> {
        let start_usize = start as usize;
        let len_usize = len as usize;
        if start_usize + len_usize > self.0.len() {
            Err("Index out of bounds".to_string())
        } else {
            Ok(MuxString(self.0[start_usize..start_usize + len_usize].to_string()))
        }
    }

    pub fn split(&self, sep: &str) -> Vec<MuxString> {
        self.0.split(sep).map(|s| MuxString(s.to_string())).collect()
    }

    pub fn replace(&self, from: &str, to: &str) -> MuxString {
        MuxString(self.0.replace(from, to))
    }
}

impl fmt::Display for MuxString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_to_int(s: *const c_char) -> i64 {
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy();
    MuxString(rust_str.to_string()).to_int().unwrap_or_default()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_to_float(s: *const c_char) -> f64 {
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy();
    MuxString(rust_str.to_string()).to_float().unwrap_or(0.0)
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