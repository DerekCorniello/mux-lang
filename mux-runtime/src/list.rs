use crate::Value;
use std::ffi::CString;
use std::fmt;

#[derive(Clone, Debug)]
pub struct List(pub Vec<Value>);

impl List {
    pub fn push_back(&mut self, val: Value) {
        self.0.push(val);
    }

    pub fn push_front(&mut self, val: Value) {
        self.0.insert(0, val);
    }

    pub fn pop_back(&mut self) -> Option<Value> {
        self.0.pop()
    }

    pub fn pop_front(&mut self) -> Option<Value> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.remove(0))
        }
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
    Box::into_raw(Box::new(
        opt.map(crate::optional::Optional::some)
            .unwrap_or(crate::optional::Optional::none()),
    ))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_push(list: *mut List, val: *mut Value) {
    let value = unsafe { *Box::from_raw(val) };
    unsafe { (*list).push_back(value) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_pop(list: *mut List) -> *mut crate::optional::Optional {
    let val = if unsafe { (*list).0.is_empty() } {
        None
    } else {
        Some(unsafe { (*list).0.remove(0) })
    };
    Box::into_raw(Box::new(
        val.map(crate::optional::Optional::some)
            .unwrap_or(crate::optional::Optional::none()),
    ))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_push_front(list: *mut List, val: *mut Value) {
    let value = unsafe { *Box::from_raw(val) };
    unsafe {
        (*list).0.insert(0, value);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_pop_front(list: *mut List) -> *mut crate::optional::Optional {
    let val = if unsafe { (*list).0.is_empty() } {
        None
    } else {
        Some(unsafe { (*list).0.remove(0) })
    };
    Box::into_raw(Box::new(
        val.map(crate::optional::Optional::some)
            .unwrap_or(crate::optional::Optional::none()),
    ))
}

/// # Safety
/// `list` must be a valid, non-null pointer to a `List` created by this runtime.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_list_is_empty(list: *const List) -> bool {
    if list.is_null() {
        return false;
    }
    unsafe { (*list).length() == 0 }
}

/// # Safety
/// `list` must be a valid, non-null pointer to a `List` created by this runtime.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_list_length(list: *const List) -> i64 {
    unsafe { (*list).length() }
}

// Functions that operate directly on Values containing lists
// These functions extract the list, modify it, and update the original Value
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_push_back_value(list_val: *mut Value, val: *mut Value) {
    let value = unsafe { *Box::from_raw(val) };
    unsafe {
        // Extract list, modify it, and update the original Value
        if let Value::List(list_data) = &*list_val {
            let mut new_list = list_data.clone();
            new_list.push(value);
            *list_val = Value::List(new_list);
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_push_value(list_val: *mut Value, val: *mut Value) {
    let value = unsafe { *Box::from_raw(val) };
    unsafe {
        // Extract list, modify it, and update the original Value
        if let Value::List(list_data) = &*list_val {
            let mut new_list = list_data.clone();
            new_list.insert(0, value); // Add to front
            *list_val = Value::List(new_list);
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_push_front_value(list_val: *mut Value, val: *mut Value) {
    let value = unsafe { *Box::from_raw(val) };
    unsafe {
        // Extract list, modify it, and update the original Value
        if let Value::List(list_data) = &*list_val {
            let mut new_list = list_data.clone();
            new_list.insert(0, value);
            *list_val = Value::List(new_list);
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_pop_back_value(list_val: *mut Value) -> *mut crate::optional::Optional {
    let opt = unsafe {
        // Extract list data first
        let current_list = if let Value::List(ref list_data) = *list_val {
            Some(list_data.clone())
        } else {
            None
        };

        if let Some(mut list_data) = current_list {
            let popped = list_data.pop();
            // Update the original Value
            *list_val = Value::List(list_data);
            popped
        } else {
            None
        }
    };
    Box::into_raw(Box::new(
        opt.map(crate::optional::Optional::some)
            .unwrap_or(crate::optional::Optional::none()),
    ))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_pop_value(list_val: *mut Value) -> *mut crate::optional::Optional {
    let opt = unsafe {
        // Extract list data first
        let current_list = if let Value::List(ref list_data) = *list_val {
            Some(list_data.clone())
        } else {
            None
        };

        if let Some(mut list_data) = current_list {
            let popped = if list_data.is_empty() {
                None
            } else {
                Some(list_data.remove(0)) // Remove from front
            };
            // Update the original Value
            *list_val = Value::List(list_data);
            popped
        } else {
            None
        }
    };
    Box::into_raw(Box::new(
        opt.map(crate::optional::Optional::some)
            .unwrap_or(crate::optional::Optional::none()),
    ))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_pop_front_value(list_val: *mut Value) -> *mut crate::optional::Optional {
    let opt = unsafe {
        // Extract list data first
        let current_list = if let Value::List(ref list_data) = *list_val {
            Some(list_data.clone())
        } else {
            None
        };

        if let Some(mut list_data) = current_list {
            let popped = if list_data.is_empty() {
                None
            } else {
                Some(list_data.remove(0))
            };
            // Update the original Value
            *list_val = Value::List(list_data);
            popped
        } else {
            None
        }
    };
    Box::into_raw(Box::new(
        opt.map(crate::optional::Optional::some)
            .unwrap_or(crate::optional::Optional::none()),
    ))
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
    if list.is_null() {
        return std::ptr::null_mut();
    }
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
        unsafe {
            (*list).0[index as usize] = value;
        }
    }
    // else do nothing
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_insert(list: *mut List, index: i64, val: *mut Value) {
    let value = unsafe { *Box::from_raw(val) };
    let len = unsafe { (*list).length() as usize };
    let idx = if index < 0 {
        0
    } else if index as usize > len {
        len
    } else {
        index as usize
    };
    unsafe {
        (*list).0.insert(idx, value);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_to_string(list: *const List) -> *mut std::ffi::c_char {
    let list = unsafe { &*list };
    let s = list.to_string();
    let c_str = CString::new(s).unwrap();
    c_str.into_raw()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_list_concat(a: *const List, b: *const List) -> *mut List {
    if a.is_null() || b.is_null() {
        return std::ptr::null_mut();
    }

    let mut result = unsafe { (*a).0.clone() };
    result.extend(unsafe { (*b).0.clone() });
    Box::into_raw(Box::new(List(result)))
}
