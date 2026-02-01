use ::std::cmp;
use ::std::rc::Rc;
use ::std::collections::{BTreeMap, BTreeSet};
use ::std::ffi::c_void;
use ::std::fmt;
use ::std::hash;
use ::std::mem;
use ::std::sync::atomic::{AtomicUsize, Ordering};

pub type TypeId = u32;

/// Internal data that needs cleanup when all ObjectRefs are dropped.
/// This is stored in an Arc so it's shared across clones.
struct ObjectData {
    ptr: *mut c_void,
    type_id: TypeId,
    size: usize,
    ref_count: AtomicUsize,
}

impl Drop for ObjectData {
    fn drop(&mut self) {
        // When the Arc holding this ObjectData is dropped (all ObjectRefs gone),
        // free the underlying object memory
        if !self.ptr.is_null() && self.size > 0 {
            let layout =
                ::std::alloc::Layout::from_size_align(self.size, ::std::mem::align_of::<u8>())
                    .expect("Invalid layout for object");
            unsafe {
                ::std::alloc::dealloc(self.ptr as *mut u8, layout);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ObjectRef {
    data: Rc<ObjectData>,
}

// Manual Debug since ObjectData doesn't derive it
impl ::std::fmt::Debug for ObjectData {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        f.debug_struct("ObjectData")
            .field("ptr", &self.ptr)
            .field("type_id", &self.type_id)
            .field("size", &self.size)
            .field("ref_count", &self.ref_count.load(Ordering::Relaxed))
            .finish()
    }
}

impl ObjectRef {
    pub fn new(ptr: *mut c_void, type_id: TypeId, size: usize) -> Self {
        ObjectRef {
            data: Rc::new(ObjectData {
                ptr,
                type_id,
                size,
                ref_count: AtomicUsize::new(1),
            }),
        }
    }

    pub fn ptr(&self) -> *mut c_void {
        self.data.ptr
    }

    pub fn type_id(&self) -> TypeId {
        self.data.type_id
    }

    pub fn inc_ref(&self) {
        self.data.ref_count.fetch_add(1, Ordering::Relaxed);
    }

    pub fn dec_ref(&self) -> usize {
        self.data.ref_count.fetch_sub(1, Ordering::Relaxed)
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
            (Value::Object(a), Value::Object(b)) => {
                a.ptr() == b.ptr() && a.type_id() == b.type_id()
            }
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
                (obj.ptr() as usize).hash(state);
                obj.type_id().hash(state);
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
                (a.type_id(), a.ptr() as usize).cmp(&(b.type_id(), b.ptr() as usize))
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
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "{}", s),
            Value::List(list) => {
                write!(f, "[")?;
                for (i, item) in list.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Value::Map(map) => {
                write!(f, "{{")?;
                for (i, (key, val)) in map.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, val)?;
                }
                write!(f, "}}")
            }
            Value::Set(set) => {
                write!(f, "{{")?;
                for (i, item) in set.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "}}")
            }
            Value::Optional(opt) => match opt {
                Some(val) => write!(f, "Some({})", val),
                None => write!(f, "None"),
            },
            Value::Result(res) => match res {
                Ok(val) => write!(f, "Ok({})", val),
                Err(val) => write!(f, "Err({})", val),
            },
            Value::Object(obj) => {
                write!(f, "<Object at {:p} type_id={}>", obj.ptr(), obj.type_id())
            }
        }
    }
}

pub mod bool;
pub mod boxing;
pub mod float;
pub mod int;
pub mod io;
pub mod list;
pub mod map;
pub mod math;
pub mod object;
pub mod optional;
pub mod refcount;
pub mod result;
pub mod set;
pub mod std;
pub mod string;

// Re-export extern "C" functions for C linkage
pub use std::{mux_value_list_get_value, mux_value_list_length};

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_value(f: f64) -> *mut Value {
    refcount::mux_rc_alloc(Value::Float(ordered_float::OrderedFloat(f)))
}
