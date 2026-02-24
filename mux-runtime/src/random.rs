//! Random number generation for Mux standard library

use libc;
use std::sync::Once;

static INIT: Once = Once::new();

/// Initialize random seed with a specific value
/// # Safety
/// This function is thread-safe due to the Once implementation
#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_init(seed: i64) {
    INIT.call_once(|| unsafe { libc::srand(seed as u32) });
}

/// Get random integer (0 to RAND_MAX)
/// Auto-initializes with time on first call if not explicitly seeded
#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_int() -> i64 {
    INIT.call_once(|| {
        let time_seed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i32;
        unsafe { libc::srand(time_seed as u32) }
    });
    unsafe { libc::rand() as i64 }
}

/// Get random integer in range [min, max)
/// Returns min if min >= max
#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_range(min: i64, max: i64) -> i64 {
    if min >= max {
        return min;
    }
    let range_size = max - min;
    let scaled = ((mux_rand_int() as u128) * (range_size as u128)) >> 32;
    min + (scaled as i64)
}

/// Get random float [0.0, 1.0)
#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_float() -> f64 {
    let r = mux_rand_int() as f64;
    r / ((libc::RAND_MAX as f64) + 1.0)
}

/// Get random boolean (true or false with equal probability)
#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_bool() -> bool {
    mux_rand_int() % 2 == 0
}
