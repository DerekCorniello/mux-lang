use crate::result::MuxResult;
use chrono::{DateTime, Datelike, Local, Timelike, Utc};
use std::ffi::CStr;
use std::os::raw::c_char;
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_now() -> *mut MuxResult {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(duration) => {
            let seconds = duration.as_secs() as i64;
            Box::into_raw(Box::new(MuxResult::ok(crate::Value::Int(seconds))))
        }
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "Failed to get current time: {}",
            e
        )))),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_now_millis() -> *mut MuxResult {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(duration) => {
            let millis = duration.as_millis() as i64;
            Box::into_raw(Box::new(MuxResult::ok(crate::Value::Int(millis))))
        }
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "Failed to get current time: {}",
            e
        )))),
    }
}

fn timestamp_to_datetime(timestamp: i64) -> Option<DateTime<Utc>> {
    DateTime::from_timestamp(timestamp, 0)
}

fn read_pattern(pattern: *const c_char) -> Result<String, String> {
    if pattern.is_null() {
        return Err("Format pattern cannot be null".to_string());
    }

    let pattern = unsafe { CStr::from_ptr(pattern) }
        .to_string_lossy()
        .into_owned();
    Ok(pattern)
}

fn datetime_field(timestamp: i64, get_field: impl FnOnce(&DateTime<Utc>) -> i64) -> *mut MuxResult {
    match timestamp_to_datetime(timestamp) {
        Some(dt) => Box::into_raw(Box::new(MuxResult::ok(crate::Value::Int(get_field(&dt))))),
        None => Box::into_raw(Box::new(MuxResult::err(format!(
            "Invalid timestamp: {}",
            timestamp
        )))),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_year(timestamp: i64) -> *mut MuxResult {
    datetime_field(timestamp, |dt| dt.year() as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_month(timestamp: i64) -> *mut MuxResult {
    datetime_field(timestamp, |dt| dt.month() as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_day(timestamp: i64) -> *mut MuxResult {
    datetime_field(timestamp, |dt| dt.day() as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_hour(timestamp: i64) -> *mut MuxResult {
    datetime_field(timestamp, |dt| dt.hour() as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_minute(timestamp: i64) -> *mut MuxResult {
    datetime_field(timestamp, |dt| dt.minute() as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_second(timestamp: i64) -> *mut MuxResult {
    datetime_field(timestamp, |dt| dt.second() as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_weekday(timestamp: i64) -> *mut MuxResult {
    datetime_field(timestamp, |dt| dt.weekday().num_days_from_sunday() as i64)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_format(timestamp: i64, pattern: *const c_char) -> *mut MuxResult {
    let pattern = match read_pattern(pattern) {
        Ok(p) => p,
        Err(msg) => return Box::into_raw(Box::new(MuxResult::err(msg))),
    };

    match timestamp_to_datetime(timestamp) {
        Some(dt) => {
            let formatted = dt.format(&pattern).to_string();
            Box::into_raw(Box::new(MuxResult::ok(crate::Value::String(formatted))))
        }
        None => Box::into_raw(Box::new(MuxResult::err(format!(
            "Invalid timestamp: {}",
            timestamp
        )))),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_format_local(
    timestamp: i64,
    pattern: *const c_char,
) -> *mut MuxResult {
    let pattern = match read_pattern(pattern) {
        Ok(p) => p,
        Err(msg) => return Box::into_raw(Box::new(MuxResult::err(msg))),
    };

    match timestamp_to_datetime(timestamp) {
        Some(dt) => {
            let local_dt = dt.with_timezone(&Local);
            let formatted = local_dt.format(&pattern).to_string();
            Box::into_raw(Box::new(MuxResult::ok(crate::Value::String(formatted))))
        }
        None => Box::into_raw(Box::new(MuxResult::err(format!(
            "Invalid timestamp: {}",
            timestamp
        )))),
    }
}

/// Sleep for the specified number of seconds.
/// Blocks the executing thread. For async/parallel use cases, consider using the `sync` module.
/// Returns error if seconds is negative.
#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_sleep(seconds: i64) -> *mut MuxResult {
    if seconds < 0 {
        return Box::into_raw(Box::new(MuxResult::err(
            "Sleep duration cannot be negative".to_string(),
        )));
    }
    thread::sleep(Duration::from_secs(seconds as u64));
    Box::into_raw(Box::new(MuxResult::ok(crate::Value::Unit)))
}

/// Sleep for the specified number of milliseconds.
/// Blocks the executing thread. For async/parallel use cases, consider using the `sync` module.
/// Returns error if milliseconds is negative.
#[unsafe(no_mangle)]
pub extern "C" fn mux_datetime_sleep_millis(milliseconds: i64) -> *mut MuxResult {
    if milliseconds < 0 {
        return Box::into_raw(Box::new(MuxResult::err(
            "Sleep duration cannot be negative".to_string(),
        )));
    }
    thread::sleep(Duration::from_millis(milliseconds as u64));
    Box::into_raw(Box::new(MuxResult::ok(crate::Value::Unit)))
}
