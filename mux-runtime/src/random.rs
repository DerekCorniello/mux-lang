use std::sync::{Mutex, Once};
use std::time::{SystemTime, UNIX_EPOCH};

const RAND_MAX: i64 = 2147483647;

static INIT: Once = Once::new();
static STATE: Mutex<u64> = Mutex::new(0);

fn lcg_next(state: u64) -> u64 {
    state
        .wrapping_mul(6364136223846793005)
        .wrapping_add(1442695040888963407)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_init(seed: i64) {
    INIT.call_once(|| {
        *STATE
            .lock()
            .expect("STATE mutex lock should not be poisoned") = seed as u64;
    });
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_int() -> i64 {
    INIT.call_once(|| {
        let seed = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        *STATE
            .lock()
            .expect("STATE mutex lock should not be poisoned") = seed;
    });
    let mut state = STATE
        .lock()
        .expect("STATE mutex lock should not be poisoned");
    *state = lcg_next(*state);
    ((*state >> 33) as i64) & RAND_MAX
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_range(min: i64, max: i64) -> i64 {
    if min >= max {
        return min;
    }
    let range_size = max - min;
    let scaled = ((mux_rand_int() as u128) * (range_size as u128)) >> 32;
    min + (scaled as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_float() -> f64 {
    let r = mux_rand_int() as f64;
    r / ((RAND_MAX as f64) + 1.0)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_rand_bool() -> bool {
    mux_rand_int() % 2 == 0
}
