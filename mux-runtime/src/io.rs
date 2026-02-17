use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{self, Read, Write};
use std::os::raw::c_char;
use std::path::Path;

use crate::Value;
use crate::result::MuxResult;

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
    crate::refcount::mux_rc_alloc(value)
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

// ============================================================================
// File Operations - All return Result<T, string> as *mut MuxResult
// ============================================================================

/// Read file contents at path. Returns Result<string, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_read_file(path: *const c_char) -> *mut MuxResult {
    let c_str = unsafe { CStr::from_ptr(path) };
    let path_str = c_str.to_string_lossy();

    match std::fs::read_to_string(&*path_str) {
        Ok(contents) => Box::into_raw(Box::new(MuxResult::ok(Value::String(contents)))),
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "Failed to read file '{}': {}",
            path_str, e
        )))),
    }
}

/// Write content to file at path. Returns Result<void, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_write_file(path: *const c_char, content: *const c_char) -> *mut MuxResult {
    let path_str = unsafe { CStr::from_ptr(path).to_string_lossy() };
    let content_str = unsafe { CStr::from_ptr(content).to_string_lossy() };

    match std::fs::write(&*path_str, content_str.as_bytes()) {
        Ok(_) => Box::into_raw(Box::new(MuxResult::ok(Value::Unit))),
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "Failed to write file '{}': {}",
            path_str, e
        )))),
    }
}

/// Check if path exists. Returns Result<bool, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_exists(path: *const c_char) -> *mut MuxResult {
    let path_str = unsafe { CStr::from_ptr(path).to_string_lossy() };
    Box::into_raw(Box::new(MuxResult::ok(Value::Bool(
        Path::new(&*path_str).exists(),
    ))))
}

/// Remove file at path. Returns Result<void, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_remove(path: *const c_char) -> *mut MuxResult {
    let path_str = unsafe { CStr::from_ptr(path).to_string_lossy() };

    match std::fs::remove_file(&*path_str) {
        Ok(_) => Box::into_raw(Box::new(MuxResult::ok(Value::Unit))),
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "Failed to remove '{}': {}",
            path_str, e
        )))),
    }
}

/// Check if path is a file. Returns Result<bool, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_is_file(path: *const c_char) -> *mut MuxResult {
    let path_str = unsafe { CStr::from_ptr(path).to_string_lossy() };
    Box::into_raw(Box::new(MuxResult::ok(Value::Bool(
        Path::new(&*path_str).is_file(),
    ))))
}

/// Check if path is a directory. Returns Result<bool, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_is_dir(path: *const c_char) -> *mut MuxResult {
    let path_str = unsafe { CStr::from_ptr(path).to_string_lossy() };
    Box::into_raw(Box::new(MuxResult::ok(Value::Bool(
        Path::new(&*path_str).is_dir(),
    ))))
}

// ============================================================================
// Directory Operations - All return Result<T, string> as *mut MuxResult
// ============================================================================

/// Create directory at path. Returns Result<void, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_mkdir(path: *const c_char) -> *mut MuxResult {
    let path_str = unsafe { CStr::from_ptr(path).to_string_lossy() };

    match std::fs::create_dir_all(&*path_str) {
        Ok(_) => Box::into_raw(Box::new(MuxResult::ok(Value::Unit))),
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "Failed to create directory '{}': {}",
            path_str, e
        )))),
    }
}

/// List directory contents. Returns Result<list<string>, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_listdir(path: *const c_char) -> *mut MuxResult {
    let path_str = unsafe { CStr::from_ptr(path).to_string_lossy() };

    match std::fs::read_dir(&*path_str) {
        Ok(entries) => {
            let mut list = Vec::new();
            for entry in entries.flatten() {
                if let Some(name) = entry.file_name().to_str() {
                    list.push(Value::String(name.to_string()));
                }
            }
            Box::into_raw(Box::new(MuxResult::ok(Value::List(list))))
        }
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "Failed to list directory '{}': {}",
            path_str, e
        )))),
    }
}

// ============================================================================
// Path Operations - All return Result<string, string> as *mut MuxResult
// ============================================================================

/// Join two path components. Returns Result<string, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_join(path1: *const c_char, path2: *const c_char) -> *mut MuxResult {
    let path1_str = unsafe { CStr::from_ptr(path1).to_string_lossy() };
    let path2_str = unsafe { CStr::from_ptr(path2).to_string_lossy() };

    let joined = Path::new(&*path1_str).join(&*path2_str);
    match joined.to_str() {
        Some(s) => Box::into_raw(Box::new(MuxResult::ok(Value::String(s.to_string())))),
        None => Box::into_raw(Box::new(MuxResult::err(
            "Failed to join paths: invalid UTF-8".to_string(),
        ))),
    }
}

/// Get base name of path. Returns Result<string, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_basename(path: *const c_char) -> *mut MuxResult {
    let path_str = unsafe { CStr::from_ptr(path).to_string_lossy() };

    let basename = Path::new(&*path_str)
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("");

    Box::into_raw(Box::new(MuxResult::ok(Value::String(basename.to_string()))))
}

/// Get directory name of path. Returns Result<string, string>
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_io_dirname(path: *const c_char) -> *mut MuxResult {
    let path_str = unsafe { CStr::from_ptr(path).to_string_lossy() };

    let dirname = Path::new(&*path_str)
        .parent()
        .and_then(|p| p.to_str())
        .unwrap_or(".");

    Box::into_raw(Box::new(MuxResult::ok(Value::String(dirname.to_string()))))
}
