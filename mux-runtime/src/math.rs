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

pub fn tan(x: f64) -> f64 {
    x.tan()
}

pub fn asin(x: f64) -> f64 {
    x.asin()
}

pub fn acos(x: f64) -> f64 {
    x.acos()
}

pub fn atan(x: f64) -> f64 {
    x.atan()
}

pub fn atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

pub fn ln(x: f64) -> f64 {
    x.ln()
}

pub fn log(x: f64, base: f64) -> f64 {
    x.log(base)
}

pub fn log2(x: f64) -> f64 {
    x.log2()
}

pub fn log10(x: f64) -> f64 {
    x.log10()
}

pub fn exp(x: f64) -> f64 {
    x.exp()
}

pub fn abs(x: f64) -> f64 {
    x.abs()
}

pub fn floor(x: f64) -> f64 {
    x.floor()
}

pub fn ceil(x: f64) -> f64 {
    x.ceil()
}

pub fn round(x: f64) -> f64 {
    x.round()
}

pub fn min(a: f64, b: f64) -> f64 {
    a.min(b)
}

pub fn max(a: f64, b: f64) -> f64 {
    a.max(b)
}

pub fn hypot(x: f64, y: f64) -> f64 {
    x.hypot(y)
}

pub const PI: f64 = std::f64::consts::PI;

pub const E: f64 = std::f64::consts::E;

// --- extern "C" wrappers ---

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

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_tan(x: f64) -> f64 {
    tan(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_asin(x: f64) -> f64 {
    asin(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_acos(x: f64) -> f64 {
    acos(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_atan(x: f64) -> f64 {
    atan(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_atan2(y: f64, x: f64) -> f64 {
    atan2(y, x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_ln(x: f64) -> f64 {
    ln(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_log(x: f64, base: f64) -> f64 {
    log(x, base)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_log2(x: f64) -> f64 {
    log2(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_log10(x: f64) -> f64 {
    log10(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_exp(x: f64) -> f64 {
    exp(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_abs(x: f64) -> f64 {
    abs(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_floor(x: f64) -> f64 {
    floor(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_ceil(x: f64) -> f64 {
    ceil(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_round(x: f64) -> f64 {
    round(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_min(a: f64, b: f64) -> f64 {
    min(a, b)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_max(a: f64, b: f64) -> f64 {
    max(a, b)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_hypot(x: f64, y: f64) -> f64 {
    hypot(x, y)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_pi() -> f64 {
    PI
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_math_e() -> f64 {
    E
}
