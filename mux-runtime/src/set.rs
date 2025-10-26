use crate::Value;
use std::collections::BTreeSet;
use std::fmt;

#[derive(Clone, Debug)]
pub struct Set(pub BTreeSet<Value>);

impl Set {
    pub fn add(&mut self, val: Value) {
        self.0.insert(val);
    }

    pub fn remove(&mut self, val: &Value) -> bool {
        self.0.remove(val)
    }

    pub fn contains(&self, val: &Value) -> bool {
        self.0.contains(val)
    }

    pub fn union(&self, other: &Set) -> Set {
        Set(self.0.union(&other.0).cloned().collect())
    }

    pub fn intersection(&self, other: &Set) -> Set {
        Set(self.0.intersection(&other.0).cloned().collect())
    }



    pub fn to_list(&self) -> crate::list::List {
        crate::list::List(self.0.iter().cloned().collect())
    }
}

impl fmt::Display for Set {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let strs: Vec<String> = self.0.iter().map(|v| v.to_string()).collect();
        write!(f, "{{{}}}", strs.join(", "))
    }
}