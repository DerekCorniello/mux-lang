use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{self, Read, Write};
use std::os::raw::c_char;

pub fn print(s: &str) {
    print!("{}", s);
}

pub fn println(s: &str) {
    println!("{}", s);
}

pub fn read_line() -> Result<String, String> {
    let mut input = String::new();
    io::stdin().read_line(&mut input).map_err(|e| e.to_string())?;
    Ok(input.trim().to_string())
}

pub fn open_file(path: &str) -> Result<File, String> {
    File::open(path).map_err(|e| e.to_string())
}

pub fn read_file(mut file: File) -> Result<String, String> {
    let mut contents = String::new();
    file.read_to_string(&mut contents).map_err(|e| e.to_string())?;
    Ok(contents)
}

pub fn write_file(mut file: File, content: &str) -> Result<(), String> {
    file.write_all(content.as_bytes()).map_err(|e| e.to_string())
}

pub fn close_file(_file: File) {
    // File closes on drop
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_print(s: *const c_char) {
    let s = unsafe { CStr::from_ptr(s).to_string_lossy() };
    print!("{}", s);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_println(s: *const c_char) {
    let s = unsafe { CStr::from_ptr(s).to_string_lossy() };
    println!("{}", s);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_read_line() -> *mut c_char {
    match read_line() {
        Ok(s) => CString::new(s).unwrap().into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}