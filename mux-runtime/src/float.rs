use std::ffi::CString;
use std::fmt;
use std::os::raw::c_char;

use crate::result::MuxResult;
use crate::Value;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Float(pub ordered_float::OrderedFloat<f64>);

impl Float {

    pub fn to_int(&self) -> i64 {
        self.0.into_inner() as i64
    }

    pub fn add(&self, other: &Float) -> Float {
        Float(self.0 + other.0)
    }

    pub fn sub(&self, other: &Float) -> Float {
        Float(self.0 - other.0)
    }

    pub fn mul(&self, other: &Float) -> Float {
        Float(self.0 * other.0)
    }

    pub fn div(&self, other: &Float) -> Result<Float, String> {
        if other.0 == ordered_float::OrderedFloat(0.0) {
            Err("Division by zero".to_string())
        } else {
            Ok(Float(self.0 / other.0))
        }
    }

    pub fn abs(&self) -> Float {
        Float(ordered_float::OrderedFloat(self.0.abs()))
    }

    pub fn round(&self) -> Float {
        Float(ordered_float::OrderedFloat(self.0.round()))
    }

    pub fn floor(&self) -> Float {
        Float(ordered_float::OrderedFloat(self.0.floor()))
    }

    pub fn ceil(&self) -> Float {
        Float(ordered_float::OrderedFloat(self.0.ceil()))
    }

    pub fn lt(&self, other: &Float) -> bool {
        self.0 < other.0
    }


}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_to_string(f: f64) -> *mut c_char {
    let s = format!("{}", Float(ordered_float::OrderedFloat(f)));
    CString::new(s).unwrap().into_raw()
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_from_value(v: *mut Value) -> f64 {
    if let Value::Float(f) = unsafe { &*v } {
        f.into_inner()
    } else {
        panic!("Expected Float value");
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_to_int(f: f64) -> i64 {
    Float(ordered_float::OrderedFloat(f)).to_int()
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_add(a: f64, b: f64) -> f64 {
    Float(ordered_float::OrderedFloat(a)).add(&Float(ordered_float::OrderedFloat(b))).0.into_inner()
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_sub(a: f64, b: f64) -> f64 {
    Float(ordered_float::OrderedFloat(a)).sub(&Float(ordered_float::OrderedFloat(b))).0.into_inner()
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_mul(a: f64, b: f64) -> f64 {
    Float(ordered_float::OrderedFloat(a)).mul(&Float(ordered_float::OrderedFloat(b))).0.into_inner()
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_div(a: f64, b: f64) -> *mut MuxResult {
    match Float(ordered_float::OrderedFloat(a)).div(&Float(ordered_float::OrderedFloat(b))) {
        Ok(f) => Box::into_raw(Box::new(MuxResult::ok(Value::Float(f.0)))),
        Err(e) => Box::into_raw(Box::new(MuxResult::err(e))),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_eq(a: f64, b: f64) -> bool {
    Float(ordered_float::OrderedFloat(a)) == Float(ordered_float::OrderedFloat(b))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_float_lt(a: f64, b: f64) -> bool {
    Float(ordered_float::OrderedFloat(a)) < Float(ordered_float::OrderedFloat(b))
}
