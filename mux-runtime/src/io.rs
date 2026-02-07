use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{self, Read, Write};
use std::os::raw::c_char;

use crate::refcount::mux_rc_alloc;
use crate::Value;

#[derive(Debug)]
pub struct MuxFile(pub std::fs::File);

pub fn print(s: &str) {
    print!("{}", s);
    std::io::Write::flush(&mut std::io::stdout()).expect("stdout flush should not fail");
}

pub fn read_line() -> Result<String, String> {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .map_err(|e| e.to_string())?;
    Ok(input.trim().to_string())
}

pub fn open_file(path: &str) -> Result<File, String> {
    File::open(path).map_err(|e| e.to_string())
}

pub fn read_file(mut file: File) -> Result<String, String> {
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .map_err(|e| e.to_string())?;
    Ok(contents)
}

pub fn write_file(mut file: File, content: &str) -> Result<(), String> {
    file.write_all(content.as_bytes())
        .map_err(|e| e.to_string())
}

pub fn close_file(_file: File) {
    // File closes on drop
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_value_from_string(s: *const c_char) -> *mut crate::Value {
    let c_str = unsafe { CStr::from_ptr(s) };
    let rust_str = c_str.to_string_lossy().to_string();
    let value = crate::Value::String(rust_str);
    mux_rc_alloc(value)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_print_cstr(s: *const c_char) {
    let s = unsafe { CStr::from_ptr(s).to_string_lossy() };
    print!("{}", s);
    std::io::Write::flush(&mut std::io::stdout()).expect("stdout flush should not fail");
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_print(val: *mut Value) {
    let val = unsafe { &*val };
    println!("{}", val);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_read_line() -> *mut c_char {
    match read_line() {
        // Safe: read_line returns valid UTF-8, no null bytes in user input
        Ok(s) => CString::new(s)
            .expect("read_line should return valid UTF-8")
            .into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// # Safety
/// This function is safe as it only flushes stdout.
#[unsafe(no_mangle)]
pub extern "C" fn mux_flush_stdout() {
    let _ = std::io::Write::flush(&mut std::io::stdout());
}

/// # Safety
/// Returns a valid i64 from stdin, or 0 on error.
#[unsafe(no_mangle)]
pub extern "C" fn mux_read_int() -> i64 {
    match read_line() {
        Ok(s) => s.trim().parse().unwrap_or(0),
        Err(_) => 0,
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_open_file(path: *const c_char) -> *mut MuxFile {
    let c_str = unsafe { CStr::from_ptr(path) };
    let path_str = c_str.to_string_lossy();
    match std::fs::File::open(&*path_str) {
        Ok(f) => Box::into_raw(Box::new(MuxFile(f))),
        Err(_) => std::ptr::null_mut(),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_read_file(file: *mut MuxFile) -> *mut c_char {
    let mut contents = String::new();
    match unsafe { (*file).0.read_to_string(&mut contents) } {
        Ok(_) => match CString::new(contents) {
            Ok(c) => c.into_raw(),
            Err(_) => std::ptr::null_mut(),
        },
        Err(_) => std::ptr::null_mut(),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_write_file(file: *mut MuxFile, content: *const c_char) -> bool {
    let c_str = unsafe { CStr::from_ptr(content) };
    let content_str = c_str.to_string_lossy();
    unsafe { (*file).0.write_all(content_str.as_bytes()).is_ok() }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_close_file(file: *mut MuxFile) {
    if !file.is_null() {
        unsafe {
            drop(Box::from_raw(file));
        }
    }
}
