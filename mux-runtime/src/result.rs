use crate::Value;
use std::ffi::CStr;
use std::fmt;

#[derive(Clone, Debug)]
pub enum MuxResult {
    Ok(Box<Value>),
    Err(String),
}

impl MuxResult {
    pub fn ok(val: Value) -> MuxResult {
        MuxResult::Ok(Box::new(val))
    }

    pub fn err(msg: String) -> MuxResult {
        MuxResult::Err(msg)
    }


}

#[unsafe(no_mangle)]
pub extern "C" fn mux_result_ok_int(val: i64) -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::ok(Value::Int(val))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_result_err_str(msg: *const std::os::raw::c_char) -> *mut MuxResult {
    let c_str = unsafe { CStr::from_ptr(msg) };
    let msg_str = c_str.to_string_lossy().into_owned();
    Box::into_raw(Box::new(MuxResult::err(msg_str)))
}

impl fmt::Display for MuxResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MuxResult::Ok(v) => write!(f, "Ok({})", v),
            MuxResult::Err(e) => write!(f, "Err({})", e),
        }
    }
}