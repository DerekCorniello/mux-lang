use crate::{
    Value, list::List, map::Map, optional::Optional, refcount::mux_rc_alloc, result::MuxResult,
    set::Set,
};
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_range(start: i64, end: i64) -> *mut List {
    let mut vec = Vec::new();
    for i in start..end {
        vec.push(Value::Int(i));
    }
    Box::into_raw(Box::new(List(vec)))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_some(val: *mut Value) -> *mut Optional {
    let value = unsafe { (*val).clone() };
    Box::into_raw(Box::new(Optional::some(value)))
}

// Value creation functions for codegen - using reference counting
#[unsafe(no_mangle)]
pub extern "C" fn mux_int_value(i: i64) -> *mut Value {
    mux_rc_alloc(Value::Int(i))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_bool_value(b: i32) -> *mut Value {
    mux_rc_alloc(Value::Bool(b != 0))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_string_value(s: *const c_char) -> *mut Value {
    let c_str = unsafe { CStr::from_ptr(s) };
    let string = c_str.to_string_lossy().into_owned();
    mux_rc_alloc(Value::String(string))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_none() -> *mut Optional {
    Box::into_raw(Box::new(Optional::none()))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_ok(val: *mut Value) -> *mut MuxResult {
    let value = unsafe { (*val).clone() };
    Box::into_raw(Box::new(MuxResult::ok(value)))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_err(msg: *const c_char) -> *mut MuxResult {
    let c_str = unsafe { CStr::from_ptr(msg) };
    let msg_str = c_str.to_string_lossy().to_string();
    Box::into_raw(Box::new(MuxResult::err(msg_str)))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_new_list() -> *mut List {
    Box::into_raw(Box::new(List(Vec::new())))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_new_map() -> *mut Map {
    Box::into_raw(Box::new(Map(std::collections::BTreeMap::new())))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_new_set() -> *mut Set {
    Box::into_raw(Box::new(Set(std::collections::BTreeSet::new())))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_add(a: *mut Value, b: *mut Value) -> *mut Value {
    let a = unsafe { &*a };
    let b = unsafe { &*b };
    let result = match (a, b) {
        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
        (Value::String(a), Value::String(b)) => Value::String(a.clone() + b),
        (Value::String(a), Value::Int(b)) => Value::String(a.clone() + &b.to_string()),
        (Value::Int(a), Value::String(b)) => Value::String(a.to_string() + b),
        (Value::String(a), Value::Float(b)) => Value::String(a.clone() + &b.to_string()),
        (Value::Float(a), Value::String(b)) => Value::String(a.to_string() + b),
        (Value::String(a), Value::Bool(b)) => Value::String(a.clone() + &b.to_string()),
        (Value::Bool(a), Value::String(b)) => Value::String(a.to_string() + b),
        _ => Value::Int(0), // error
    };
    mux_rc_alloc(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_value(list: *mut List) -> *mut Value {
    let list_ref = unsafe { &*list };
    mux_rc_alloc(Value::List(list_ref.0.clone()))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_get_list(val: *mut Value) -> *mut List {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        match &*val {
            Value::List(list_data) => Box::into_raw(Box::new(List(list_data.clone()))),
            _ => std::ptr::null_mut(),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_get_map(val: *mut Value) -> *mut Map {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        match &*val {
            Value::Map(map_data) => Box::into_raw(Box::new(Map(map_data.clone()))),
            _ => std::ptr::null_mut(),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_get_set(val: *mut Value) -> *mut Set {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        match &*val {
            Value::Set(set_data) => Box::into_raw(Box::new(Set(set_data.clone()))),
            _ => std::ptr::null_mut(),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_to_string(val: *mut Value) -> *mut c_char {
    let value = unsafe { &*val };
    let s = value.to_string();
    // Safe: to_string produces valid UTF-8 without null bytes
    let c_str = CString::new(s).expect("to_string should produce valid UTF-8");
    c_str.into_raw()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_list_length(val: *const Value) -> i64 {
    let val = unsafe { &*val };
    if let Value::List(vec) = val {
        vec.len() as i64
    } else {
        0
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_list_get_value(val: *const Value, index: i64) -> *mut Value {
    let val = unsafe { &*val };
    if let Value::List(vec) = val {
        if index >= 0 && (index as usize) < vec.len() {
            let cloned = vec[index as usize].clone();
            mux_rc_alloc(cloned)
        } else {
            std::ptr::null_mut()
        }
    } else {
        std::ptr::null_mut()
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_list_slice(val: *const Value, start: i64, end: i64) -> *mut Value {
    let val = unsafe { &*val };
    if let Value::List(vec) = val {
        let len = vec.len() as i64;
        let s = start.max(0) as usize;
        let e = end.min(len) as usize;
        let sliced = if s < e {
            vec[s..e].to_vec()
        } else {
            Vec::new()
        };
        mux_rc_alloc(Value::List(sliced))
    } else {
        mux_rc_alloc(Value::List(Vec::new()))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub extern "C" fn mux_value_to_list(val: *mut Value) -> *mut crate::list::List {
    // Clone the value instead of taking ownership
    let val = unsafe { (*val).clone() };
    if let Value::List(vec) = val {
        Box::into_raw(Box::new(crate::list::List(vec)))
    } else {
        panic!("Expected List value");
    }
}

/// # Safety
/// `s` must be a valid pointer returned by a mux-runtime string function.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_free_string(s: *mut c_char) {
    if !s.is_null() {
        unsafe { drop(CString::from_raw(s)) };
    }
}

/// # Safety
/// `list` must be a valid pointer returned by a mux-runtime list function.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_free_list(list: *mut List) {
    if !list.is_null() {
        unsafe { drop(Box::from_raw(list)) };
    }
}

/// # Safety
/// `map` must be a valid pointer returned by a mux-runtime map function.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_free_map(map: *mut Map) {
    if !map.is_null() {
        unsafe { drop(Box::from_raw(map)) };
    }
}

/// # Safety
/// `set` must be a valid pointer returned by a mux-runtime set function.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_free_set(set: *mut Set) {
    if !set.is_null() {
        unsafe { drop(Box::from_raw(set)) };
    }
}

/// # Safety
/// `opt` must be a valid pointer returned by a mux-runtime optional function.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_free_optional(opt: *mut Optional) {
    if !opt.is_null() {
        unsafe { drop(Box::from_raw(opt)) };
    }
}

/// # Safety
/// `res` must be a valid pointer returned by a mux-runtime result function.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_free_result(res: *mut MuxResult) {
    if !res.is_null() {
        unsafe { drop(Box::from_raw(res)) };
    }
}

// Safe value extraction functions - don't take ownership
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_get_int(val: *const Value) -> i64 {
    unsafe {
        match &*val {
            Value::Int(i) => *i,
            _ => 0, // Return default value instead of panicking
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_get_float(val: *const Value) -> f64 {
    unsafe {
        match &*val {
            Value::Float(f) => f.into_inner(),
            _ => 0.0, // Return default value instead of panicking
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_get_bool(val: *const Value) -> i32 {
    unsafe {
        match &*val {
            Value::Bool(b) => i32::from(*b),
            _ => 0,
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_get_type_tag(val: *const Value) -> i32 {
    unsafe {
        match &*val {
            Value::Bool(_) => 0,
            Value::Int(_) => 1,
            Value::Float(_) => 2,
            Value::String(_) => 3,
            Value::List(_) => 4,
            Value::Map(_) => 5,
            Value::Set(_) => 6,
            Value::Tuple(_) => 10,
            Value::Optional(_) => 7,
            Value::Result(_) => 8,
            Value::Object(_) => 9,
        }
    }
}

/// Compare two Value pointers for equality
/// Returns 1 if equal, 0 if not equal
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_equal(a: *const Value, b: *const Value) -> i32 {
    if a.is_null() || b.is_null() {
        return if a == b { 1 } else { 0 };
    }
    unsafe { if *a == *b { 1 } else { 0 } }
}

/// Compare two Value pointers for inequality
/// Returns 1 if not equal, 0 if equal
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_not_equal(a: *const Value, b: *const Value) -> i32 {
    if mux_value_equal(a, b) == 1 { 0 } else { 1 }
}

// Proper Value cleanup function
