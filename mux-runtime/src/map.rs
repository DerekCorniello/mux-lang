use crate::Tuple;
use crate::Value;
use crate::refcount::mux_rc_alloc;
use std::collections::BTreeMap;
use std::ffi::CString;
use std::fmt;
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
        let pairs: Vec<String> = self
            .0
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_value(map: *mut Map) -> *mut Value {
    let map = unsafe { &*map };
    let value = Value::Map(map.0.clone());
    mux_rc_alloc(value)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_get(
    map: *const Map,
    key: *const Value,
) -> *mut crate::optional::Optional {
    let opt = unsafe { (*map).get(&*key).cloned() };
    Box::into_raw(Box::new(
        opt.map(crate::optional::Optional::some)
            .unwrap_or(crate::optional::Optional::none()),
    ))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_put(map: *mut Map, key: *mut Value, val: *mut Value) {
    let map = unsafe { &mut *map };
    let key = unsafe { (*key).clone() };
    let val = unsafe { (*val).clone() };
    map.insert(key, val);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[allow(clippy::mutable_key_type)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_put_value(map_val: *mut Value, key: *mut Value, val: *mut Value) {
    if map_val.is_null() || key.is_null() || val.is_null() {
        return;
    }
    unsafe {
        if let Value::Map(map_data) = &*map_val {
            let mut new_map = map_data.clone();
            let key_clone = (*key).clone();
            let val_clone = (*val).clone();
            new_map.insert(key_clone, val_clone);
            // Write back to the original Value
            *map_val = Value::Map(new_map);
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_remove(
    map: *mut Map,
    key: *const Value,
) -> *mut crate::optional::Optional {
    let opt = unsafe { (*map).remove(&*key) };
    Box::into_raw(Box::new(
        opt.map(crate::optional::Optional::some)
            .unwrap_or(crate::optional::Optional::none()),
    ))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[allow(clippy::mutable_key_type)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_remove_value(
    map_val: *mut Value,
    key: *mut Value,
) -> *mut crate::optional::Optional {
    let key = unsafe { (*key).clone() };
    unsafe {
        if let Value::Map(map_data) = &*map_val {
            let mut new_map = map_data.clone();
            let opt = new_map.remove(&key);
            *map_val = Value::Map(new_map);
            return Box::into_raw(Box::new(
                opt.map(crate::optional::Optional::some)
                    .unwrap_or(crate::optional::Optional::none()),
            ));
        }
    }
    Box::into_raw(Box::new(crate::optional::Optional::none()))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_contains(map: *const Map, key: *const Value) -> bool {
    if map.is_null() || key.is_null() {
        return false;
    }
    unsafe { (*map).contains(&*key) }
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
    // Safe: to_string produces valid UTF-8 without null bytes
    let c_str = CString::new(s).expect("to_string should produce valid UTF-8");
    c_str.into_raw()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_keys(map: *const Map) -> *mut Value {
    if map.is_null() {
        return mux_rc_alloc(Value::List(Vec::new()));
    }
    let keys: Vec<Value> = unsafe { (*map).0.keys().cloned().collect() };
    mux_rc_alloc(Value::List(keys))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_values(map: *const Map) -> *mut Value {
    if map.is_null() {
        return mux_rc_alloc(Value::List(Vec::new()));
    }
    let values: Vec<Value> = unsafe { (*map).0.values().cloned().collect() };
    mux_rc_alloc(Value::List(values))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_pairs(map: *const Map) -> *mut Value {
    if map.is_null() {
        return mux_rc_alloc(Value::List(Vec::new()));
    }
    let pairs: Vec<Value> = unsafe {
        (*map)
            .0
            .iter()
            .map(|(k, v)| Value::Tuple(Box::new(Tuple(k.clone(), v.clone()))))
            .collect()
    };
    mux_rc_alloc(Value::List(pairs))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[allow(clippy::mutable_key_type)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_map_merge(a: *const Map, b: *const Map) -> *mut Map {
    if a.is_null() || b.is_null() {
        return std::ptr::null_mut();
    }

    let mut result = unsafe { (*a).0.clone() };
    result.extend(unsafe { (*b).0.clone() });
    Box::into_raw(Box::new(Map(result)))
}
