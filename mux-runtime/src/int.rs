use std::ffi::CString;
use std::fmt;
use std::os::raw::c_char;

#[derive(Clone, Debug)]
pub struct Int(pub i64);

impl Int {

    pub fn to_float(&self) -> f64 {
        self.0 as f64
    }

    pub fn add(&self, other: &Int) -> Int {
        Int(self.0 + other.0)
    }

    pub fn sub(&self, other: &Int) -> Int {
        Int(self.0 - other.0)
    }

    pub fn mul(&self, other: &Int) -> Int {
        Int(self.0 * other.0)
    }

    pub fn div(&self, other: &Int) -> Result<Int, String> {
        if other.0 == 0 {
            Err("Division by zero".to_string())
        } else {
            Ok(Int(self.0 / other.0))
        }
    }

    pub fn rem(&self, other: &Int) -> Result<Int, String> {
        if other.0 == 0 {
            Err("Modulo by zero".to_string())
        } else {
            Ok(Int(self.0 % other.0))
        }
    }

    pub fn eq(&self, other: &Int) -> bool {
        self.0 == other.0
    }

    pub fn lt(&self, other: &Int) -> bool {
        self.0 < other.0
    }

    pub fn gt(&self, other: &Int) -> bool {
        self.0 > other.0
    }

    pub fn le(&self, other: &Int) -> bool {
        self.0 <= other.0
    }

    pub fn ge(&self, other: &Int) -> bool {
        self.0 >= other.0
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_to_string(i: i64) -> *mut c_char {
    let s = format!("{}", Int(i));
    CString::new(s).unwrap().into_raw()
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_to_float(i: i64) -> f64 {
    Int(i).to_float()
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_add(a: i64, b: i64) -> i64 {
    Int(a).add(&Int(b)).0
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_sub(a: i64, b: i64) -> i64 {
    Int(a).sub(&Int(b)).0
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_mul(a: i64, b: i64) -> i64 {
    Int(a).mul(&Int(b)).0
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_div(a: i64, b: i64) -> i64 {
    match Int(a).div(&Int(b)) {
        Ok(i) => i.0,
        Err(_) => 0, // or panic, but for FFI, return 0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_rem(a: i64, b: i64) -> i64 {
    match Int(a).rem(&Int(b)) {
        Ok(i) => i.0,
        Err(_) => 0,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_eq(a: i64, b: i64) -> bool {
    Int(a).eq(&Int(b))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_lt(a: i64, b: i64) -> bool {
    Int(a).lt(&Int(b))
}