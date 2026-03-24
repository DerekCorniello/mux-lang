use crate::refcount::mux_rc_alloc;
use crate::Value;
use std::ffi::CStr;
use std::fmt;

#[derive(Clone, Debug)]
pub enum MuxResult {
    Ok(Box<Value>),
    Err(Box<Value>),
}

impl MuxResult {
    pub fn ok(val: Value) -> MuxResult {
        MuxResult::Ok(Box::new(val))
    }

    pub fn err<V: Into<Value>>(val: V) -> MuxResult {
        MuxResult::Err(Box::new(val.into()))
    }
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
pub extern "C" fn mux_result_ok_int(val: i64) -> *mut Value {
    mux_rc_alloc(Value::Result(Ok(Box::new(Value::Int(val)))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_result_ok_float(val: f64) -> *mut Value {
    use ordered_float::OrderedFloat;
    mux_rc_alloc(Value::Result(Ok(Box::new(Value::Float(OrderedFloat(val))))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_result_ok_bool(val: i32) -> *mut Value {
    mux_rc_alloc(Value::Result(Ok(Box::new(Value::Bool(val != 0)))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_result_ok_char(val: i64) -> *mut Value {
    mux_rc_alloc(Value::Result(Ok(Box::new(Value::Int(val)))))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_result_ok_string(val: *mut Value) -> *mut Value {
    mux_result_ok_value(val)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_result_ok_value(val: *mut Value) -> *mut Value {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        let value = (*val).clone();
        mux_rc_alloc(Value::Result(Ok(Box::new(value))))
    }
}

/// # Safety
/// The `msg` pointer must point to a valid, null-terminated C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_result_err_str(msg: *const std::os::raw::c_char) -> *mut Value {
    let c_str = unsafe { CStr::from_ptr(msg) };
    let msg_str = c_str.to_string_lossy().into_owned();
    mux_rc_alloc(Value::Result(Err(Box::new(Value::String(msg_str)))))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_result_err_value(val: *mut Value) -> *mut Value {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        let value = (*val).clone();
        mux_rc_alloc(Value::Result(Err(Box::new(value))))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_result_is_ok(val: *mut Value) -> bool {
    if val.is_null() {
        return false;
    }
    unsafe { matches!(&*val, Value::Result(Ok(_))) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_result_is_err(val: *mut Value) -> bool {
    if val.is_null() {
        return false;
    }
    unsafe { matches!(&*val, Value::Result(Err(_))) }
}

/// Returns the inner value from a `Value::Result`, for both Ok and Err variants.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_result_data(val: *mut Value) -> *mut Value {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        match &*val {
            Value::Result(Ok(v)) => mux_rc_alloc(*v.clone()),
            Value::Result(Err(e)) => mux_rc_alloc(*e.clone()),
            _ => std::ptr::null_mut(),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_result_discriminant(val: *mut Value) -> i32 {
    if val.is_null() {
        return -1;
    }
    unsafe {
        match &*val {
            Value::Result(Ok(_)) => 0,
            Value::Result(Err(_)) => 1,
            _ => -1,
        }
    }
}
