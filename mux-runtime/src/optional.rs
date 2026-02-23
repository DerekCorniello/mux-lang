use crate::Value;
use crate::refcount::mux_rc_alloc;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Optional {
    Some(Box<Value>),
    None,
}

impl Optional {
    pub fn some(val: Value) -> Optional {
        Optional::Some(Box::new(val))
    }

    pub fn none() -> Optional {
        Optional::None
    }
}

impl fmt::Display for Optional {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Optional::Some(v) => write!(f, "Some({})", v),
            Optional::None => write!(f, "None"),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_discriminant(opt: *mut Optional) -> i32 {
    if opt.is_null() {
        return -1;
    }
    unsafe {
        match &*opt {
            Optional::Some(_) => 0,
            Optional::None => 1,
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_is_some(opt: *mut Optional) -> bool {
    if opt.is_null() {
        return false;
    }
    unsafe { matches!(&*opt, Optional::Some(_)) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_is_none(opt: *mut Optional) -> bool {
    if opt.is_null() {
        return false;
    }
    unsafe { matches!(&*opt, Optional::None) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_get_value(opt: *mut Optional) -> *mut Value {
    if opt.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        match &*opt {
            Optional::Some(v) => mux_rc_alloc(*v.clone()),
            Optional::None => std::ptr::null_mut(),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_data(opt: *mut Optional) -> *mut Value {
    mux_optional_get_value(opt)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_int(val: i64) -> *mut Optional {
    Box::into_raw(Box::new(Optional::some(Value::Int(val))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_float(val: f64) -> *mut Optional {
    use ordered_float::OrderedFloat;
    Box::into_raw(Box::new(Optional::some(Value::Float(OrderedFloat(val)))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_bool(val: i32) -> *mut Optional {
    Box::into_raw(Box::new(Optional::some(Value::Bool(val != 0))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_char(val: i64) -> *mut Optional {
    // Char is passed as i64, store as int
    Box::into_raw(Box::new(Optional::some(Value::Int(val))))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_string(val: *mut Value) -> *mut Optional {
    mux_optional_some_value(val)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_value(val: *mut Value) -> *mut Optional {
    if val.is_null() {
        return Box::into_raw(Box::new(Optional::none()));
    }
    unsafe {
        let value = (*val).clone();
        Box::into_raw(Box::new(Optional::some(value)))
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_none() -> *mut Optional {
    Box::into_raw(Box::new(Optional::none()))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_to_string(opt: *const Optional) -> *mut std::ffi::c_char {
    use std::ffi::CString;
    if opt.is_null() {
        // Safe: "null" is valid UTF-8 without null bytes
        return CString::new("null".to_string())
            .expect("'null' string should be valid UTF-8")
            .into_raw();
    }
    unsafe {
        let s = (*opt).to_string();
        // Safe: to_string produces valid UTF-8 without null bytes
        CString::new(s)
            .expect("to_string should produce valid UTF-8")
            .into_raw()
    }
}

/// # Safety
/// Takes ownership of `opt` - caller must NOT call `mux_free_optional` after this.
/// Returns ownership of a new `Value*` - caller is responsible for its lifecycle.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_into_value(opt: *mut Optional) -> *mut crate::Value {
    if opt.is_null() {
        return Box::into_raw(Box::new(crate::Value::Optional(None)));
    }
    unsafe {
        let optional = Box::from_raw(opt);
        match *optional {
            Optional::Some(value) => Box::into_raw(Box::new(crate::Value::Optional(Some(value)))),
            Optional::None => Box::into_raw(Box::new(crate::Value::Optional(None))),
        }
    }
}

/// Deprecated: Use `mux_optional_into_value` instead.
/// This function takes ownership of `opt` - caller must NOT call `mux_free_optional` after this.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
#[deprecated(
    since = "0.1.2",
    note = "Use mux_optional_into_value instead - this function takes ownership"
)]
pub extern "C" fn mux_value_from_optional(opt: *mut Optional) -> *mut crate::Value {
    mux_optional_into_value(opt)
}

/// # Safety
/// Takes ownership of `val` - caller must NOT free after this.
/// Returns ownership of a new `Optional*` - caller is responsible for its lifecycle.
/// If val contains an Optional, clones the inner value (does NOT take ownership of the wrapped Optional).
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_from_value(val: *mut crate::Value) -> *mut Optional {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        match &*val {
            crate::Value::Optional(Some(v)) => {
                Box::into_raw(Box::new(Optional::Some(Box::new(*v.clone()))))
            }
            crate::Value::Optional(None) => Box::into_raw(Box::new(Optional::None)),
            _ => std::ptr::null_mut(),
        }
    }
}
