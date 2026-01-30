pub fn pow(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}

pub fn sqrt(x: f64) -> f64 {
    x.sqrt()
}

pub fn sin(x: f64) -> f64 {
    x.sin()
}

pub fn cos(x: f64) -> f64 {
    x.cos()
}

pub const PI: f64 = std::f64::consts::PI;

pub const E: f64 = std::f64::consts::E;

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_pow(base: f64, exp: f64) -> f64 {
    pow(base, exp)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_int_pow(base: i64, exp: i64) -> i64 {
    if exp < 0 {
        return 0;
    }
    let mut result = 1i64;
    let mut b = base;
    let mut e = exp;
    while e > 0 {
        if e & 1 == 1 {
            result = result.wrapping_mul(b);
        }
        b = b.wrapping_mul(b);
        e >>= 1;
    }
    result
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_sqrt(x: f64) -> f64 {
    sqrt(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_sin(x: f64) -> f64 {
    sin(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_cos(x: f64) -> f64 {
    cos(x)
}
