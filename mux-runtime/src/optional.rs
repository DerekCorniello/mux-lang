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
pub extern "C" fn mux_optional_is_some(val: *mut Value) -> bool {
    if val.is_null() {
        return false;
    }
    unsafe { matches!(&*val, Value::Optional(Some(_))) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_is_none(val: *mut Value) -> bool {
    if val.is_null() {
        return false;
    }
    unsafe { matches!(&*val, Value::Optional(None)) }
}

/// Returns a new `*mut Value` containing the inner value of a `Value::Optional(Some(...))`.
/// Returns null if the optional is None or the pointer is null.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_get_value(val: *mut Value) -> *mut Value {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        match &*val {
            Value::Optional(Some(v)) => mux_rc_alloc(*v.clone()),
            _ => std::ptr::null_mut(),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_data(val: *mut Value) -> *mut Value {
    mux_optional_get_value(val)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_int(val: i64) -> *mut Value {
    mux_rc_alloc(Value::Optional(Some(Box::new(Value::Int(val)))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_float(val: f64) -> *mut Value {
    use ordered_float::OrderedFloat;
    mux_rc_alloc(Value::Optional(Some(Box::new(Value::Float(OrderedFloat(
        val,
    ))))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_bool(val: i32) -> *mut Value {
    mux_rc_alloc(Value::Optional(Some(Box::new(Value::Bool(val != 0)))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_char(val: i64) -> *mut Value {
    mux_rc_alloc(Value::Optional(Some(Box::new(Value::Int(val)))))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_string(val: *mut Value) -> *mut Value {
    mux_optional_some_value(val)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_value(val: *mut Value) -> *mut Value {
    if val.is_null() {
        return mux_rc_alloc(Value::Optional(None));
    }
    unsafe {
        let value = (*val).clone();
        mux_rc_alloc(Value::Optional(Some(Box::new(value))))
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_none() -> *mut Value {
    mux_rc_alloc(Value::Optional(None))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_to_string(val: *const Value) -> *mut std::ffi::c_char {
    use std::ffi::CString;
    if val.is_null() {
        return CString::new("null".to_string())
            .expect("'null' string should be valid UTF-8")
            .into_raw();
    }
    unsafe {
        let s = match &*val {
            Value::Optional(Some(v)) => format!("Some({})", v),
            Value::Optional(None) => "None".to_string(),
            other => other.to_string(),
        };
        CString::new(s)
            .expect("to_string should produce valid UTF-8")
            .into_raw()
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_optional_discriminant(val: *mut Value) -> i32 {
    if val.is_null() {
        return -1;
    }
    unsafe {
        match &*val {
            Value::Optional(Some(_)) => 0,
            Value::Optional(None) => 1,
            _ => -1,
        }
    }
}

/// Identity function: `*mut Value` that is already a `Value::Optional` passes through unchanged.
/// Exists for ABI compatibility with older generated code.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_into_value(val: *mut Value) -> *mut Value {
    val
}
