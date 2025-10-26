use crate::Value;
use std::fmt;

#[derive(Clone, Debug)]
pub struct List(pub Vec<Value>);

impl List {
    pub fn push(&mut self, val: Value) {
        self.0.insert(0, val);
    }

    pub fn pop(&mut self) -> Option<Value> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.remove(0))
        }
    }

    pub fn push_back(&mut self, val: Value) {
        self.0.push(val);
    }

    pub fn pop_back(&mut self) -> Option<Value> {
        self.0.pop()
    }

    pub fn length(&self) -> i64 {
        self.0.len() as i64
    }

    pub fn concat(&self, other: &List) -> List {
        let mut new_vec = self.0.clone();
        new_vec.extend(other.0.clone());
        List(new_vec)
    }



    pub fn join(&self, sep: &str) -> String {
        let strs: Vec<String> = self.0.iter().map(|v| v.to_string()).collect();
        strs.join(sep)
    }

    pub fn to_set(&self) -> crate::set::Set {
        use std::collections::BTreeSet;
        let mut set = BTreeSet::new();
        for v in &self.0 {
            set.insert(v.clone());
        }
        crate::set::Set(set)
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let strs: Vec<String> = self.0.iter().map(|v| v.to_string()).collect();
        write!(f, "[{}]", strs.join(", "))
    }
}