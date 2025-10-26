use crate::Value;
use std::fmt;

#[derive(Clone, Debug)]
pub enum MuxResult {
    Ok(Box<Value>),
    Err(String),
}

impl MuxResult {
    pub fn ok(val: Value) -> MuxResult {
        MuxResult::Ok(Box::new(val))
    }

    pub fn err(msg: String) -> MuxResult {
        MuxResult::Err(msg)
    }


}

impl fmt::Display for MuxResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MuxResult::Ok(v) => write!(f, "Ok({})", v),
            MuxResult::Err(e) => write!(f, "Err({})", e),
        }
    }
}