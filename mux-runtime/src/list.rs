use crate::Value;
use std::fmt;
use std::ffi::CString;

#[derive(Clone, Debug)]
pub struct List(pub Vec<Value>);

impl List {
    pub fn push_back(&mut self, val: Value) {
        self.0.push(val);
    }

    pub fn pop_back(&mut self) -> Option<Value> {
        self.0.pop()
    }

    pub fn length(&self) -> i64 {
        self.0.len() as i64
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let strs: Vec<String> = self.0.iter().map(|v| v.to_string()).collect();
        write!(f, "[{}]", strs.join(", "))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_push_back(list: *mut List, val: *mut Value) {
    let value = unsafe { *Box::from_raw(val) };
    unsafe { (*list).push_back(value) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_pop_back(list: *mut List) -> *mut crate::optional::Optional {
    let opt = unsafe { (*list).pop_back() };
    Box::into_raw(Box::new(opt.map(crate::optional::Optional::some).unwrap_or(crate::optional::Optional::none())))
}

/// # Safety
/// `list` must be a valid, non-null pointer to a `List` created by this runtime.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_list_is_empty(list: *const List) -> bool {
    unsafe { (*list).length() == 0 }
}

/// # Safety
/// `list` must be a valid, non-null pointer to a `List` created by this runtime.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_list_length(list: *const List) -> i64 {
    unsafe { (*list).length() }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_get(list: *const List, index: i64) -> *mut crate::optional::Optional {
    let len = unsafe { (*list).length() };
    if index < 0 || index >= len {
        Box::into_raw(Box::new(crate::optional::Optional::none()))
    } else {
        let val = unsafe { (*list).0[index as usize].clone() };
        Box::into_raw(Box::new(crate::optional::Optional::some(val)))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_get_value(list: *const List, index: i64) -> *mut Value {
    let len = unsafe { (*list).length() };
    if index < 0 || index >= len {
        std::ptr::null_mut()
    } else {
        let val = unsafe { (*list).0[index as usize].clone() };
        Box::into_raw(Box::new(val))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_set(list: *mut List, index: i64, val: *mut Value) {
    let value = unsafe { *Box::from_raw(val) };
    let len = unsafe { (*list).length() };
    if index >= 0 && index < len {
        unsafe { (*list).0[index as usize] = value; }
    }
    // else do nothing
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_insert(list: *mut List, index: i64, val: *mut Value) {
    let value = unsafe { *Box::from_raw(val) };
    let len = unsafe { (*list).length() as usize };
    let idx = if index < 0 { 0 } else if index as usize > len { len } else { index as usize };
    unsafe { (*list).0.insert(idx, value); }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_to_string(list: *const List) -> *mut std::ffi::c_char {
    let list = unsafe { &*list };
    let s = list.to_string();
    let c_str = CString::new(s).unwrap();
    c_str.into_raw()
}