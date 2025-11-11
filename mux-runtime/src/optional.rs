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

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_data(opt: *mut Optional) -> *mut Value {
    if opt.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        match &*opt {
            Optional::Some(v) => v.as_ref() as *const Value as *mut Value,
            Optional::None => std::ptr::null_mut(),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_some_int(val: i64) -> *mut Optional {
    Box::into_raw(Box::new(Optional::some(Value::Int(val))))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_optional_none() -> *mut Optional {
    Box::into_raw(Box::new(Optional::none()))
}