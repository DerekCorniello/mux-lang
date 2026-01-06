use crate::Value;
use std::collections::BTreeMap;
use std::fmt;
use std::ffi::CString;
use std::os::raw::c_char;

#[derive(Clone, Debug)]
pub struct Map(pub BTreeMap<Value, Value>);

impl Map {
    pub fn insert(&mut self, key: Value, val: Value) {
        self.0.insert(key, val);
    }

    pub fn get(&self, key: &Value) -> Option<&Value> {
        self.0.get(key)
    }

    pub fn remove(&mut self, key: &Value) -> Option<Value> {
        self.0.remove(key)
    }

    pub fn contains(&self, key: &Value) -> bool {
        self.0.contains_key(key)
    }


}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pairs: Vec<String> = self.0.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_value(map: *mut Map) -> *mut Value {
    let map = unsafe { Box::from_raw(map) };
    let value = Value::Map(map.0);
    Box::into_raw(Box::new(value))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_get(map: *const Map, key: *const Value) -> *mut crate::optional::Optional {
    let opt = unsafe { (*map).get(&*key).cloned() };
    Box::into_raw(Box::new(opt.map(crate::optional::Optional::some).unwrap_or(crate::optional::Optional::none())))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_put(map: *mut Map, key: *mut Value, val: *mut Value) {
    let map = unsafe { &mut *map };
    let key = unsafe { Box::from_raw(key) };
    let val = unsafe { Box::from_raw(val) };
    map.insert(*key, *val);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_remove(map: *mut Map, key: *const Value) -> *mut crate::optional::Optional {
    let opt = unsafe { (*map).remove(&*key) };
    Box::into_raw(Box::new(opt.map(crate::optional::Optional::some).unwrap_or(crate::optional::Optional::none())))
}

/// # Safety
/// `map` must be a valid, non-null pointer to a `Map` created by this runtime.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_map_size(map: *const Map) -> i64 {
    unsafe { (*map).0.len() as i64 }
}

/// # Safety
/// `map` must be a valid, non-null pointer to a `Map` created by this runtime.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_map_is_empty(map: *const Map) -> bool {
    unsafe { (*map).0.is_empty() }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_to_string(map: *const Map) -> *mut c_char {
    let map = unsafe { &*map };
    let s = map.to_string();
    let c_str = CString::new(s).unwrap();
    c_str.into_raw()
}

/// Check if a map contains a key (used by codegen)
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_contains_key(map: *const Map, key: *const Value) -> bool {
    unsafe { (*map).contains(&*key) }
}