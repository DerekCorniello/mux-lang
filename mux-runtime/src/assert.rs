use crate::Value;
use crate::optional::Optional;
use crate::result::MuxResult;
use std::ffi::CStr;
use std::os::raw::c_char;

fn panic_assert(msg: &str) -> ! {
    eprintln!("Assertion failed: {}", msg);
    std::process::abort();
}

#[inline]
fn deref_ptr<T>(ptr: *mut T, context: &str) -> &T {
    if ptr.is_null() {
        panic_assert(&format!("{} received null pointer", context));
    }
    unsafe { &*ptr }
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
pub extern "C" fn mux_assert_some(opt: *mut Optional) {
    let opt_val = deref_ptr(opt, "assert_some");
    match opt_val {
        Optional::None => panic_assert("expected Some, got None"),
        Optional::Some(_) => {}
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_none(opt: *mut Optional) {
    let opt_val = deref_ptr(opt, "assert_none");
    match opt_val {
        Optional::None => {}
        Optional::Some(v) => panic_assert(&format!("expected None, got Some({})", v)),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_ok(res: *mut MuxResult) {
    let res_val = deref_ptr(res, "assert_ok");
    match res_val {
        MuxResult::Err(e) => panic_assert(&format!("expected Ok, got Err({})", e)),
        MuxResult::Ok(_) => {}
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_assert_err(res: *mut MuxResult) {
    let res_val = deref_ptr(res, "assert_err");
    match res_val {
        MuxResult::Err(_) => {}
        MuxResult::Ok(v) => panic_assert(&format!("expected Err, got Ok({})", v)),
    }
}
