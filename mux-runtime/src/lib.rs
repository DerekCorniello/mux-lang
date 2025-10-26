use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
pub mod optional;
pub mod result;
pub mod set;
pub mod string;
