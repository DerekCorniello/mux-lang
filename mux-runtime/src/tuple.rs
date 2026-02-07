use crate::Tuple;
use crate::Value;
use crate::refcount::mux_rc_alloc;
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

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_get_tuple(value: *mut Value) -> *mut Tuple {
    if value.is_null() {
        return std::ptr::null_mut();
    }
    // The Value::Tuple variant contains a Box<Tuple>
    // We need to cast the Value pointer to access the inner Box<Tuple>
    // This is safe because we're just reinterpreting the pointer
    unsafe {
        let value_ref = &mut *value;
        if let Value::Tuple(tuple_box) = value_ref {
            let tuple_ptr: *mut Tuple = &mut **tuple_box;
            tuple_ptr
        } else {
            std::ptr::null_mut()
        }
    }
}
