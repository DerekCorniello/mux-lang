use crate::Value;
use std::ffi::CStr;
use std::os::raw::c_char;

fn panic_assert(msg: &str) -> ! {
    eprintln!("Assertion failed: {}", msg);
    std::process::abort();
}

#[inline]
fn deref_ptr<T>(ptr: *mut T, context: &str) -> &'static T {
    if ptr.is_null() {
        panic_assert(&format!("{} received null pointer", context));
    }
    unsafe { &*(ptr as *const T) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_assert(condition: i32, message: *const c_char) {
    if condition == 0 {
        let msg = if message.is_null() {
            "assert condition was false".to_string()
        } else {
            unsafe { CStr::from_ptr(message) }
                .to_string_lossy()
                .into_owned()
        };
        panic_assert(&msg);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_eq(actual: *mut Value, expected: *mut Value) {
    let actual_val = deref_ptr(actual, "assert_eq");
    let expected_val = deref_ptr(expected, "assert_eq");
    if actual_val != expected_val {
        panic_assert(&format!("expected {}, got {}", expected_val, actual_val));
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_ne(actual: *mut Value, expected: *mut Value) {
    let actual_val = deref_ptr(actual, "assert_ne");
    let expected_val = deref_ptr(expected, "assert_ne");
    if actual_val == expected_val {
        panic_assert(&format!(
            "expected values to differ, but both were {}",
            actual_val
        ));
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_true(condition: i32) {
    if condition == 0 {
        panic_assert("expected true, got false");
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_false(condition: i32) {
    if condition != 0 {
        panic_assert("expected false, got true");
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_some(val: *mut Value) {
    let v = deref_ptr(val, "assert_some");
    match v {
        Value::Optional(None) => panic_assert("expected Some, got None"),
        Value::Optional(Some(_)) => {}
        _ => panic_assert("expected Optional value"),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_none(val: *mut Value) {
    let v = deref_ptr(val, "assert_none");
    match v {
        Value::Optional(None) => {}
        Value::Optional(Some(inner)) => {
            panic_assert(&format!("expected None, got Some({})", inner))
        }
        _ => panic_assert("expected Optional value"),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_ok(val: *mut Value) {
    let v = deref_ptr(val, "assert_ok");
    match v {
        Value::Result(Err(e)) => panic_assert(&format!("expected Ok, got Err({})", e)),
        Value::Result(Ok(_)) => {}
        _ => panic_assert("expected Result value"),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_err(val: *mut Value) {
    let v = deref_ptr(val, "assert_err");
    match v {
        Value::Result(Err(_)) => {}
        Value::Result(Ok(v)) => panic_assert(&format!("expected Err, got Ok({})", v)),
        _ => panic_assert("expected Result value"),
    }
}
