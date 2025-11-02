use crate::Value;
use std::fmt;

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