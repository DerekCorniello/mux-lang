//! Reference counting infrastructure for Mux runtime values.
//!
//! This module provides automatic memory management via reference counting.
//! Every heap-allocated Value is prefixed with a RefHeader containing an
//! atomic reference count. When the count reaches zero, the memory is freed.
//!
//! Memory layout:
//! ```text
//! ┌──────────────────┬─────────────┐
//! │ RefHeader        │   Value     │
//! │ (ref_count: u64) │  (payload)  │
//! └──────────────────┴─────────────┘
//!          ↑
//!     Allocation pointer
//!
//!          ↑ + sizeof(RefHeader)
//!     External *mut Value pointer
//! ```

use std::alloc::{alloc, dealloc, Layout};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::Value;

/// Header prepended to every reference-counted Value allocation.
/// Uses atomic operations for thread-safety.
#[repr(C)]
pub struct RefHeader {
    /// Atomic reference count.
    ref_count: AtomicUsize,
}

impl RefHeader {
    #[inline]
    const fn new() -> Self {
        RefHeader {
            ref_count: AtomicUsize::new(1),
        }
    }
}

/// Calculate the memory layout for RefHeader + Value
#[inline]
fn layout_for_value() -> Layout {
    let header_size = std::mem::size_of::<RefHeader>();
    let value_size = std::mem::size_of::<Value>();
    let value_align = std::mem::align_of::<Value>();

    // Ensure proper alignment for Value after header
    let header_align = std::mem::align_of::<RefHeader>();
    let total_align = header_align.max(value_align);

    // Calculate padding between header and value for alignment
    let header_padded = (header_size + value_align - 1) & !(value_align - 1);
    let total_size = header_padded + value_size;

    Layout::from_size_align(total_size, total_align)
        .expect("Failed to create layout for ref-counted Value")
}

#[inline]
fn value_offset() -> usize {
    let header_size = std::mem::size_of::<RefHeader>();
    let value_align = std::mem::align_of::<Value>();
    // Align header size up to value alignment
    (header_size + value_align - 1) & !(value_align - 1)
}

/// Allocate a new reference-counted Value.
///
/// Returns a pointer to the Value (not the header).
/// The Value starts with ref_count = 1.
///
/// # Safety
/// The returned pointer must eventually be passed to `mux_rc_dec` to free the memory.
#[allow(improper_ctypes_definitions)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_rc_alloc(value: Value) -> *mut Value {
    let layout = layout_for_value();

    unsafe {
        let ptr = alloc(layout);
        if ptr.is_null() {
            panic!("Failed to allocate ref-counted Value: out of memory");
        }

        let header = ptr as *mut RefHeader;
        header.write(RefHeader::new());

        let value_ptr = ptr.add(value_offset()) as *mut Value;
        value_ptr.write(value);

        value_ptr
    }
}

/// # Safety
/// The Value pointer must have been returned by `mux_rc_alloc`.
#[inline]
unsafe fn get_header(val: *mut Value) -> *mut RefHeader {
    unsafe { (val as *mut u8).sub(value_offset()) as *mut RefHeader }
}

#[inline]
unsafe fn get_alloc_base(val: *mut Value) -> *mut u8 {
    unsafe { (val as *mut u8).sub(value_offset()) }
}

/// Increment the reference count of a Value.
///
/// Call this when creating a new reference to an existing Value
/// (e.g., assigning to a new variable, passing as argument, etc.)
///
/// # Safety
/// - `val` must be a valid pointer returned by `mux_rc_alloc` or null.
/// - The Value must not have been freed.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_rc_inc(val: *mut Value) {
    if val.is_null() {
        return;
    }

    unsafe {
        let header = get_header(val);
        // Relaxed ordering is sufficient for increment
        (*header).ref_count.fetch_add(1, Ordering::Relaxed);
    }
}

/// Decrement the reference count of a Value.
///
/// If the count reaches zero, the Value is freed and this returns true.
/// Otherwise returns false.
///
/// # Safety
/// - `val` must be a valid pointer returned by `mux_rc_alloc` or null.
/// - After this returns true, `val` is invalid and must not be used.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_rc_dec(val: *mut Value) -> bool {
    if val.is_null() {
        return false;
    }

    unsafe {
        let header = get_header(val);

        let old_count = (*header).ref_count.fetch_sub(1, Ordering::AcqRel);

        if old_count == 1 {
            std::ptr::drop_in_place(val);

            let alloc_ptr = get_alloc_base(val);
            dealloc(alloc_ptr, layout_for_value());

            true
        } else {
            false
        }
    }
}

/// Get the current reference count of a Value (for debugging).
///
/// # Safety
/// - `val` must be a valid pointer returned by `mux_rc_alloc` or null.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_rc_count(val: *const Value) -> usize {
    if val.is_null() {
        return 0;
    }

    unsafe {
        let header = get_header(val as *mut Value);
        (*header).ref_count.load(Ordering::Relaxed)
    }
}

/// Clone a ref-counted Value by incrementing its reference count.
///
/// This is the preferred way to "copy" a Value - it just increments
/// the ref count and returns the same pointer.
///
/// # Safety
/// - `val` must be a valid pointer returned by `mux_rc_alloc` or null.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_rc_clone(val: *mut Value) -> *mut Value {
    if val.is_null() {
        return val;
    }

    mux_rc_inc(val);
    val
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Value;

    #[test]
    fn test_alloc_and_free() {
        let val = mux_rc_alloc(Value::Int(42));
        assert!(!val.is_null());
        assert_eq!(mux_rc_count(val), 1);

        // Verify the value
        unsafe {
            assert!(matches!(&*val, Value::Int(42)));
        }

        // Free it
        assert!(mux_rc_dec(val)); // Returns true when freed
    }

    #[test]
    fn test_inc_dec_cycle() {
        let val = mux_rc_alloc(Value::Int(100));
        assert_eq!(mux_rc_count(val), 1);

        mux_rc_inc(val);
        assert_eq!(mux_rc_count(val), 2);

        mux_rc_inc(val);
        assert_eq!(mux_rc_count(val), 3);

        assert!(!mux_rc_dec(val)); // Count is now 2
        assert_eq!(mux_rc_count(val), 2);

        assert!(!mux_rc_dec(val)); // Count is now 1
        assert_eq!(mux_rc_count(val), 1);

        assert!(mux_rc_dec(val)); // Count is now 0, freed
    }

    #[test]
    fn test_clone() {
        let val = mux_rc_alloc(Value::Bool(true));
        assert_eq!(mux_rc_count(val), 1);

        let cloned = mux_rc_clone(val);
        assert_eq!(cloned, val); // Same pointer
        assert_eq!(mux_rc_count(val), 2);

        assert!(!mux_rc_dec(cloned));
        assert!(mux_rc_dec(val));
    }

    #[test]
    fn test_null_safety() {
        // These should not crash
        mux_rc_inc(std::ptr::null_mut());
        assert!(!mux_rc_dec(std::ptr::null_mut()));
        assert_eq!(mux_rc_count(std::ptr::null()), 0);
        assert!(mux_rc_clone(std::ptr::null_mut()).is_null());
    }

    #[test]
    fn test_string_cleanup() {
        let s = String::from("Hello, World! This is a longer string to test heap allocation.");
        let val = mux_rc_alloc(Value::String(s));

        unsafe {
            if let Value::String(ref stored) = *val {
                assert_eq!(
                    stored,
                    "Hello, World! This is a longer string to test heap allocation."
                );
            } else {
                panic!("Expected String value");
            }
        }

        assert!(mux_rc_dec(val)); // Should clean up the String
    }

    #[test]
    fn test_list_cleanup() {
        let list = vec![
            Value::Int(1),
            Value::Int(2),
            Value::String("test".to_string()),
        ];
        let val = mux_rc_alloc(Value::List(list));

        unsafe {
            if let Value::List(ref stored) = *val {
                assert_eq!(stored.len(), 3);
            } else {
                panic!("Expected List value");
            }
        }

        assert!(mux_rc_dec(val)); // Should clean up the Vec and its contents
    }

    #[test]
    #[allow(clippy::mutable_key_type)]
    fn test_nested_collections() {
        use std::collections::BTreeMap;

        let mut map = BTreeMap::new();
        map.insert(Value::String("key1".to_string()), Value::Int(100));
        map.insert(
            Value::String("key2".to_string()),
            Value::List(vec![Value::Int(1), Value::Int(2)]),
        );

        let val = mux_rc_alloc(Value::Map(map));
        assert_eq!(mux_rc_count(val), 1);

        assert!(mux_rc_dec(val)); // Should clean up everything recursively
    }
}
