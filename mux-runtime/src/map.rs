use crate::Value;
use std::collections::BTreeMap;
use std::fmt;

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

    pub fn keys(&self) -> Vec<Value> {
        self.0.keys().cloned().collect()
    }

    pub fn values(&self) -> Vec<Value> {
        self.0.values().cloned().collect()
    }

    pub fn contains(&self, key: &Value) -> bool {
        self.0.contains_key(key)
    }

    pub fn items(&self) -> Vec<(Value, Value)> {
        self.0.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }


}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pairs: Vec<String> = self.0.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}