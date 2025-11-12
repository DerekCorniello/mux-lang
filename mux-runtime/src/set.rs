use crate::Value;
use std::collections::BTreeSet;
use std::fmt;
use std::ffi::{CString, CStr};

#[derive(Clone, Debug)]
pub struct Set(pub BTreeSet<Value>);

impl Set {
    pub fn add(&mut self, val: Value) {
        self.0.insert(val);
    }

    pub fn remove(&mut self, val: &Value) -> bool {
        self.0.remove(val)
    }

    pub fn contains(&self, val: &Value) -> bool {
        self.0.contains(val)
    }


}

impl fmt::Display for Set {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let strs: Vec<String> = self.0.iter().map(|v| v.to_string()).collect();
        write!(f, "{{{}}}", strs.join(", "))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_value(set: *mut Set) -> *mut Value {
    let set = unsafe { Box::from_raw(set) };
    let value = Value::Set(set.0);
    Box::into_raw(Box::new(value))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_add(set: *mut Set, val: *mut Value) {
    let set = unsafe { &mut *set };
    let val = unsafe { Box::from_raw(val) };
    set.add(*val);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_contains(set: *const Set, val: *const Value) -> bool {
    unsafe { (*set).contains(&*val) }
}

/// # Safety
/// `set` must be a valid, non-null pointer to a `Set` created by this runtime.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_set_size(set: *const Set) -> i64 {
    unsafe { (*set).0.len() as i64 }
}

/// # Safety
/// `set` must be a valid, non-null pointer to a `Set` created by this runtime.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_set_is_empty(set: *const Set) -> bool {
    unsafe { (*set).0.is_empty() }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_to_string(set: *const Set) -> *mut std::ffi::c_char {
    let set = unsafe { &*set };
    let s = set.to_string();
    let c_str = CString::new(s).unwrap();
    c_str.into_raw()
}