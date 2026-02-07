use crate::refcount::mux_rc_alloc;
use crate::Tuple;
use crate::Value;
use std::ffi::CString;
use std::os::raw::c_char;

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_new_tuple(left: *mut Value, right: *mut Value) -> *mut Tuple {
    let left_val = unsafe { (*left).clone() };
    let right_val = unsafe { (*right).clone() };
    Box::into_raw(Box::new(Tuple(left_val, right_val)))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_tuple_value(tuple: *mut Tuple) -> *mut Value {
    let tuple = unsafe { &*tuple };
    let value = Value::Tuple(Box::new(tuple.clone()));
    mux_rc_alloc(value)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_tuple_eq(a: *mut Tuple, b: *mut Tuple) -> bool {
    if a.is_null() || b.is_null() {
        return false;
    }
    let a = unsafe { &*a };
    let b = unsafe { &*b };
    a == b
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_tuple_to_string(tuple: *const Tuple) -> *mut c_char {
    let tuple = unsafe { &*tuple };
    let s = tuple.to_string();
    let c_str = CString::new(s).expect("to_string should produce valid UTF-8");
    c_str.into_raw()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_tuple_left(tuple: *mut Tuple) -> *mut Value {
    let tuple = unsafe { &*tuple };
    mux_rc_alloc(tuple.0.clone())
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_tuple_right(tuple: *mut Tuple) -> *mut Value {
    let tuple = unsafe { &*tuple };
    mux_rc_alloc(tuple.1.clone())
}
