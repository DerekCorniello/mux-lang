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

/// # Safety
/// The `msg` pointer must point to a valid, null-terminated C string.
/// The caller must ensure the pointer remains valid for the duration of this function call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_result_err_str(msg: *const std::os::raw::c_char) -> *mut MuxResult {
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

#[unsafe(no_mangle)]
pub extern "C" fn mux_result_discriminant(res: *mut MuxResult) -> i32 {
    if res.is_null() {
        return -1;
    }
    unsafe {
        match &*res {
            MuxResult::Ok(_) => 0,
            MuxResult::Err(_) => 1,
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_result_data(res: *mut MuxResult) -> *mut Value {
    if res.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        match &*res {
            MuxResult::Ok(v) => v.as_ref() as *const Value as *mut Value,
            MuxResult::Err(e) => Box::into_raw(Box::new(Value::String(e.clone()))),
        }
    }
}