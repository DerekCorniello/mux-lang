use crate::Value;
use crate::refcount::mux_rc_alloc;
use std::collections::BTreeSet;
use std::ffi::CString;
use std::fmt;

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
    let set = unsafe { &*set };
    let value = Value::Set(set.0.clone());
    mux_rc_alloc(value)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_add(set: *mut Set, val: *mut Value) {
    let set = unsafe { &mut *set };
    let val = unsafe { (*val).clone() };
    set.add(val);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[allow(clippy::mutable_key_type)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_add_value(set_val: *mut Value, val: *mut Value) {
    let value = unsafe { (*val).clone() };
    unsafe {
        if let Value::Set(set_data) = &*set_val {
            let mut new_set = set_data.clone();
            new_set.insert(value);
            *set_val = Value::Set(new_set);
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_contains(set: *const Set, val: *const Value) -> bool {
    unsafe { (*set).contains(&*val) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_remove(set: *mut Set, val: *mut Value) -> *mut crate::optional::Optional {
    let set = unsafe { &mut *set };
    let val = unsafe { (*val).clone() };
    if set.remove(&val) {
        Box::into_raw(Box::new(crate::optional::Optional::some(val)))
    } else {
        Box::into_raw(Box::new(crate::optional::Optional::none()))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[allow(clippy::mutable_key_type)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_remove_value(
    set_val: *mut Value,
    val: *mut Value,
) -> *mut crate::optional::Optional {
    let value = unsafe { (*val).clone() };
    unsafe {
        if let Value::Set(set_data) = &*set_val {
            let mut new_set = set_data.clone();
            let removed = new_set.remove(&value);
            *set_val = Value::Set(new_set);
            if removed {
                return Box::into_raw(Box::new(crate::optional::Optional::some(value)));
            }
            return Box::into_raw(Box::new(crate::optional::Optional::none()));
        }
    }
    Box::into_raw(Box::new(crate::optional::Optional::none()))
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
    // Safe: to_string produces valid UTF-8 without null bytes
    let c_str = CString::new(s).expect("to_string should produce valid UTF-8");
    c_str.into_raw()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[allow(clippy::mutable_key_type)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_union(a: *const Set, b: *const Set) -> *mut Set {
    if a.is_null() || b.is_null() {
        return std::ptr::null_mut();
    }

    let mut result = unsafe { (*a).0.clone() };
    result.extend(unsafe { (*b).0.clone() });
    Box::into_raw(Box::new(Set(result)))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[allow(clippy::mutable_key_type)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_difference(a: *const Set, b: *const Set) -> *mut Set {
    if a.is_null() || b.is_null() {
        return std::ptr::null_mut();
    }

    let left = unsafe { &(*a).0 };
    let right = unsafe { &(*b).0 };
    let result: BTreeSet<Value> = left.difference(right).cloned().collect();
    Box::into_raw(Box::new(Set(result)))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[allow(clippy::mutable_key_type)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_set_intersection(a: *const Set, b: *const Set) -> *mut Set {
    if a.is_null() || b.is_null() {
        return std::ptr::null_mut();
    }

    let left = unsafe { &(*a).0 };
    let right = unsafe { &(*b).0 };
    let result: BTreeSet<Value> = left.intersection(right).cloned().collect();
    Box::into_raw(Box::new(Set(result)))
}
