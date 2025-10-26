use std::ffi::CString;
use std::fmt;
use std::os::raw::c_char;

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
pub extern "C" fn mux_bool_to_string(b: bool) -> *mut c_char {
    let s = format!("{}", Bool(b));
    CString::new(s).unwrap().into_raw()
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_bool_to_int(b: bool) -> i64 {
    Bool(b).to_int()
}