use crate::Value;
use std::collections::BTreeMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

#[derive(Clone, Debug, PartialEq)]
pub enum Json {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Json>),
    Object(BTreeMap<String, Json>),
}

impl Json {
    pub fn parse(input: &str) -> Result<Json, String> {
        match serde_json::from_str::<serde_json::Value>(input) {
            Ok(v) => Ok(convert_serde_value(&v)),
            Err(e) => Err(format!("{}", e)),
        }
    }

    pub fn stringify(&self, indent: Option<usize>) -> String {
        let v = convert_to_serde_value(self);
        if let Some(n) = indent {
            serde_json::to_string_pretty(&v)
                .unwrap_or_else(|_| String::new())
                .replace("  ", &" ".repeat(n))
        } else {
            serde_json::to_string(&v).unwrap_or_else(|_| String::new())
        }
    }
}

fn convert_serde_value(v: &serde_json::Value) -> Json {
    match v {
        serde_json::Value::Null => Json::Null,
        serde_json::Value::Bool(b) => Json::Bool(*b),
        serde_json::Value::Number(n) => Json::Number(n.as_f64().unwrap_or(0.0)),
        serde_json::Value::String(s) => Json::String(s.clone()),
        serde_json::Value::Array(arr) => Json::Array(arr.iter().map(convert_serde_value).collect()),
        serde_json::Value::Object(map) => {
            let mut m = BTreeMap::new();
            for (k, v) in map.iter() {
                m.insert(k.clone(), convert_serde_value(v));
            }
            Json::Object(m)
        }
    }
}

fn convert_to_serde_value(j: &Json) -> serde_json::Value {
    match j {
        Json::Null => serde_json::Value::Null,
        Json::Bool(b) => serde_json::Value::Bool(*b),
        Json::Number(n) => serde_json::Number::from_f64(*n)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        Json::String(s) => serde_json::Value::String(s.clone()),
        Json::Array(a) => serde_json::Value::Array(a.iter().map(convert_to_serde_value).collect()),
        Json::Object(m) => {
            let map = m
                .iter()
                .map(|(k, v)| (k.clone(), convert_to_serde_value(v)))
                .collect();
            serde_json::Value::Object(map)
        }
    }
}

// Expose simple runtime helpers to convert between Json and Value
#[allow(clippy::mutable_key_type)]
pub fn json_to_value(j: &Json) -> Value {
    match j {
        Json::Null => Value::Unit,
        Json::Bool(b) => Value::Bool(*b),
        Json::Number(n) => Value::Float(ordered_float::OrderedFloat(*n)),
        Json::String(s) => Value::String(s.clone()),
        Json::Array(a) => Value::List(a.iter().map(json_to_value).collect()),
        Json::Object(m) => {
            let mut map = std::collections::BTreeMap::new();
            for (k, v) in m.iter() {
                map.insert(Value::String(k.clone()), json_to_value(v));
            }
            Value::Map(map)
        }
    }
}

pub fn value_to_json(v: &Value) -> Result<Json, String> {
    match v {
        Value::Unit => Ok(Json::Null),
        Value::Bool(b) => Ok(Json::Bool(*b)),
        Value::Int(i) => Ok(Json::Number(*i as f64)),
        Value::Float(f) => Ok(Json::Number(f.into_inner())),
        Value::String(s) => Ok(Json::String(s.clone())),
        Value::List(list) => Ok(Json::Array(
            list.iter()
                .map(|it| value_to_json(it).unwrap_or(Json::Null))
                .collect(),
        )),
        Value::Map(map) => {
            let mut m = BTreeMap::new();
            for (k, v) in map.iter() {
                // only string keys allowed in JSON
                if let Value::String(key_str) = k {
                    m.insert(key_str.clone(), value_to_json(v)?);
                } else {
                    return Err("map contains non-string key, cannot convert to JSON".to_string());
                }
            }
            Ok(Json::Object(m))
        }
        _ => Err("unsupported value type for JSON conversion".to_string()),
    }
}

#[allow(dead_code)]
pub fn json_to_cstring(s: &str) -> *mut c_char {
    CString::new(s).unwrap().into_raw()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_json_parse(input: *const c_char) -> *mut crate::result::MuxResult {
    if input.is_null() {
        let msg = CString::new("null input").unwrap();
        unsafe {
            return crate::result::mux_result_err_str(msg.as_ptr());
        }
    }
    let s = unsafe { CStr::from_ptr(input) }
        .to_string_lossy()
        .into_owned();
    match Json::parse(&s) {
        Ok(j) => {
            let v = json_to_value(&j);
            // Allocate a ref-counted Value and wrap it using existing result helper
            let v_ptr = crate::refcount::mux_rc_alloc(v);
            crate::result::mux_result_ok_value(v_ptr)
        }
        Err(e) => {
            let cmsg = CString::new(e).unwrap();
            unsafe { crate::result::mux_result_err_str(cmsg.as_ptr()) }
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_json_stringify(
    val: *const Value,
    indent_opt: *const crate::optional::Optional,
) -> *mut Value {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    let v = unsafe { &*val };
    let indent = if indent_opt.is_null() {
        None
    } else {
        unsafe {
            match &*indent_opt {
                crate::optional::Optional::Some(boxed) => match &**boxed {
                    Value::Int(i) => Some(*i as usize),
                    _ => None,
                },
                crate::optional::Optional::None => None,
            }
        }
    };

    match value_to_json(v) {
        Ok(j) => {
            let s = j.stringify(indent);
            let result_value = Value::String(s);
            crate::refcount::mux_rc_alloc(result_value)
        }
        Err(e) => {
            let result_value = Value::String(e);
            crate::refcount::mux_rc_alloc(result_value)
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_json_from_map(val: *const Value) -> *mut crate::result::MuxResult {
    if val.is_null() {
        let msg = CString::new("null input").unwrap();
        unsafe { return crate::result::mux_result_err_str(msg.as_ptr()) }
    }
    let v = unsafe { &*val };
    // Expect a Map at runtime
    match v {
        Value::Map(map) => {
            // Convert each value to Json to validate
            let mut jmap = BTreeMap::new();
            for (k, vv) in map.iter() {
                if let Value::String(key_str) = k {
                    match value_to_json(vv) {
                        Ok(jv) => {
                            jmap.insert(key_str.clone(), jv);
                        }
                        Err(e) => {
                            let cmsg = CString::new(e).unwrap();
                            unsafe { return crate::result::mux_result_err_str(cmsg.as_ptr()) }
                        }
                    }
                } else {
                    let cmsg = CString::new("map contains non-string key, cannot convert to JSON")
                        .unwrap();
                    unsafe { return crate::result::mux_result_err_str(cmsg.as_ptr()) }
                }
            }
            let j = Json::Object(jmap);
            let v = json_to_value(&j);
            let v_ptr = crate::refcount::mux_rc_alloc(v);
            crate::result::mux_result_ok_value(v_ptr)
        }
        _ => {
            let cmsg = CString::new("value is not a map").unwrap();
            unsafe { crate::result::mux_result_err_str(cmsg.as_ptr()) }
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_json_to_map(val: *const Value) -> *mut crate::result::MuxResult {
    if val.is_null() {
        let msg = CString::new("null input").unwrap();
        unsafe { return crate::result::mux_result_err_str(msg.as_ptr()) }
    }
    let v = unsafe { &*val };
    match value_to_json(v) {
        Ok(Json::Object(m)) => {
            let mv = json_to_value(&Json::Object(m));
            let ptr = crate::refcount::mux_rc_alloc(mv);
            crate::result::mux_result_ok_value(ptr)
        }
        Ok(_) => {
            let cmsg = CString::new("json value is not an object").unwrap();
            unsafe { crate::result::mux_result_err_str(cmsg.as_ptr()) }
        }
        Err(e) => {
            let cmsg = CString::new(e).unwrap();
            unsafe { crate::result::mux_result_err_str(cmsg.as_ptr()) }
        }
    }
}
