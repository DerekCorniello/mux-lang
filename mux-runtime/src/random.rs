//! Random number generation for Mux standard library

use libc;
use std::sync::atomic::{AtomicBool, Ordering};

static SEED_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Initialize random seed with a specific value
/// # Safety
/// This function is thread-safe due to atomic initialization check
#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_init(seed: i32) {
    unsafe { libc::srand(seed as u32) }
    SEED_INITIALIZED.store(true, Ordering::SeqCst);
}

/// Get random integer (0 to RAND_MAX)
/// Auto-initializes with time on first call if not explicitly seeded
#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_int() -> i32 {
    // Auto-initialize with time if not done
    if !SEED_INITIALIZED.load(Ordering::SeqCst) {
        let time_seed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i32;
        unsafe { libc::srand(time_seed as u32) }
        SEED_INITIALIZED.store(true, Ordering::SeqCst);
    }
    unsafe { libc::rand() }
}

/// Get random integer in range [min, max)
/// Returns min if min >= max
#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_range(min: i64, max: i64) -> i64 {
    if min >= max {
        return min;
    }
    let r = mux_rand_int() as i64;
    let range_size = max - min;
    min + (r % range_size)
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
