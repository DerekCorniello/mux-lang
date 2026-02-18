use crate::Value;
use crate::optional::Optional;
use crate::result::MuxResult;
use std::ffi::CStr;
use std::os::raw::c_char;

fn panic_assert(msg: &str) -> ! {
    eprintln!("Assertion failed: {}", msg);
    std::process::abort();
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
    if actual.is_null() || expected.is_null() {
        panic_assert("assert_eq received null pointer");
    }
    let actual_val = unsafe { &*actual };
    let expected_val = unsafe { &*expected };
    if actual_val != expected_val {
        panic_assert(&format!("expected {}, got {}", expected_val, actual_val));
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_ne(actual: *mut Value, expected: *mut Value) {
    if actual.is_null() || expected.is_null() {
        panic_assert("assert_ne received null pointer");
    }
    let actual_val = unsafe { &*actual };
    let expected_val = unsafe { &*expected };
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
pub extern "C" fn mux_assert_some(opt: *mut Optional) {
    if opt.is_null() {
        panic_assert("assert_some received null pointer");
    }
    match unsafe { &*opt } {
        Optional::None => panic_assert("expected Some, got None"),
        Optional::Some(_) => {}
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_none(opt: *mut Optional) {
    if opt.is_null() {
        panic_assert("assert_none received null pointer");
    }
    match unsafe { &*opt } {
        Optional::None => {}
        Optional::Some(v) => panic_assert(&format!("expected None, got Some({})", v)),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_ok(res: *mut MuxResult) {
    if res.is_null() {
        panic_assert("assert_ok received null pointer");
    }
    match unsafe { &*res } {
        MuxResult::Err(e) => panic_assert(&format!("expected Ok, got Err({})", e)),
        MuxResult::Ok(_) => {}
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_err(res: *mut MuxResult) {
    if res.is_null() {
        panic_assert("assert_err received null pointer");
    }
    match unsafe { &*res } {
        MuxResult::Err(_) => {}
        MuxResult::Ok(v) => panic_assert(&format!("expected Err, got Ok({})", v)),
    }
}
