use crate::refcount::mux_rc_alloc;
use crate::Value;
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
pub extern "C" fn mux_optional_data(opt: *mut Optional) -> *mut Value {
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
    if val.is_null() {
        return Box::into_raw(Box::new(Optional::none()));
    }
    unsafe {
        // Clone the value instead of taking ownership
        let value = (*val).clone();
        Box::into_raw(Box::new(Optional::some(value)))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_value(val: *mut Value) -> *mut Optional {
    // Generic function for any *mut Value (lists, maps, sets, custom types)
    if val.is_null() {
        return Box::into_raw(Box::new(Optional::none()));
    }
    unsafe {
        // Clone the value instead of taking ownership
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
        return CString::new("null".to_string()).unwrap().into_raw();
    }
    unsafe {
        let s = (*opt).to_string();
        CString::new(s).unwrap().into_raw()
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_from_optional(opt: *mut Optional) -> *mut crate::Value {
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
