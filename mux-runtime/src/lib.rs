use ::std::collections::{BTreeMap, BTreeSet};
use ::std::ffi::{c_void, CStr, c_char};
use ::std::fmt;
use ::std::hash;
use ::std::mem;
use ::std::cmp;
use ::std::sync::Arc;
use ::std::sync::atomic::{AtomicUsize, Ordering};

pub type TypeId = u32;

#[derive(Clone, Debug)]
pub struct ObjectRef {
    pub ptr: *mut c_void,
    pub type_id: TypeId,
    pub ref_count: Arc<AtomicUsize>,
}

impl ObjectRef {
    pub fn new(ptr: *mut c_void, type_id: TypeId) -> Self {
        ObjectRef {
            ptr,
            type_id,
            ref_count: Arc::new(AtomicUsize::new(1)),
        }
    }

    pub fn inc_ref(&self) {
        self.ref_count.fetch_add(1, Ordering::Relaxed);
    }

    pub fn dec_ref(&self) -> usize {
        self.ref_count.fetch_sub(1, Ordering::Relaxed)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(ordered_float::OrderedFloat<f64>),
    String(String),
    List(Vec<Value>),
    Map(BTreeMap<Value, Value>),
    Set(BTreeSet<Value>),
    Optional(Option<Box<Value>>),
    Result(Result<Box<Value>, String>),
    Object(ObjectRef),
}

// Custom implementations for collections that need them
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Set(a), Value::Set(b)) => a == b,
            (Value::Optional(a), Value::Optional(b)) => a == b,
            (Value::Result(a), Value::Result(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => a.ptr == b.ptr && a.type_id == b.type_id,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl hash::Hash for Value {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            Value::Bool(b) => b.hash(state),
            Value::Int(i) => i.hash(state),
            Value::Float(f) => f.hash(state),
            Value::String(s) => s.hash(state),
            Value::List(l) => l.hash(state),
            Value::Map(m) => {
                for (k, v) in m {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Value::Set(s) => {
                for v in s {
                    v.hash(state);
                }
            }
            Value::Optional(o) => o.hash(state),
            Value::Result(r) => r.hash(state),
            Value::Object(obj) => {
                (obj.ptr as usize).hash(state);
                obj.type_id.hash(state);
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        // Simple ordering for now - objects are considered equal if pointers and types match
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b).unwrap_or(cmp::Ordering::Equal),
            (Value::String(a), Value::String(b)) => a.cmp(b),
            (Value::List(a), Value::List(b)) => a.cmp(b),
            (Value::Map(a), Value::Map(b)) => a.cmp(b),
            (Value::Set(a), Value::Set(b)) => a.cmp(b),
            (Value::Optional(a), Value::Optional(b)) => a.cmp(b),
            (Value::Result(a), Value::Result(b)) => a.cmp(b),
            (Value::Object(a), Value::Object(b)) => {
                (a.type_id, a.ptr as usize).cmp(&(b.type_id, b.ptr as usize))
            }
            // Different types - arbitrary ordering by variant index
            (a, b) => {
                let ord_a = match a {
                    Value::Bool(_) => 0,
                    Value::Int(_) => 1,
                    Value::Float(_) => 2,
                    Value::String(_) => 3,
                    Value::List(_) => 4,
                    Value::Map(_) => 5,
                    Value::Set(_) => 6,
                    Value::Optional(_) => 7,
                    Value::Result(_) => 8,
                    Value::Object(_) => 9,
                };
                let ord_b = match b {
                    Value::Bool(_) => 0,
                    Value::Int(_) => 1,
                    Value::Float(_) => 2,
                    Value::String(_) => 3,
                    Value::List(_) => 4,
                    Value::Map(_) => 5,
                    Value::Set(_) => 6,
                    Value::Optional(_) => 7,
                    Value::Result(_) => 8,
                    Value::Object(_) => 9,
                };
                ord_a.cmp(&ord_b)
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "{}", s),
            Value::List(l) => {
                let strs: Vec<String> = l.iter().map(|v| format!("{}", v)).collect();
                write!(f, "[{}]", strs.join(", "))
            },
            Value::Map(m) => {
                let pairs: Vec<String> = m.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "{{{}}}", pairs.join(", "))
            },
            Value::Set(s) => {
                let strs: Vec<String> = s.iter().map(|v| format!("{}", v)).collect();
                write!(f, "{{{}}}", strs.join(", "))
            },
            Value::Optional(o) => match o {
                Some(v) => write!(f, "Some({})", v),
                None => write!(f, "None"),
            },
            Value::Result(r) => match r {
                Ok(v) => write!(f, "Ok({})", v),
                Err(e) => write!(f, "Err({})", e),
            },
            Value::Object(obj) => write!(f, "Object(type_id={}, ptr={:?})", obj.type_id, obj.ptr),
        }
    }
}

pub mod bool;
pub mod float;
pub mod int;
pub mod io;
pub mod list;
pub mod map;
pub mod math;
pub mod object;
pub mod optional;
pub mod result;
pub mod set;
pub mod std;
pub mod string;
