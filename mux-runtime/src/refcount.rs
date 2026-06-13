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

/// Deep-clone a Value: returns a new heap allocation whose payload is a
/// recursively cloned copy of the source. The returned pointer has
/// refcount = 1 and must eventually be released with `mux_rc_dec`.
///
/// For primitives (Int, Float, Bool, Unit) the new box wraps a copy
/// of the value. For owned aggregates (String, List, Map, Set, Tuple,
/// Optional, Result, Opaque) the inner data is recursively cloned. For
/// `Value::Object` the call dispatches to `mux_copy_object` so that class
/// fields participate in the copy via the registered copy callback, and
/// the refcounted box returned by that call is forwarded to the caller
/// unchanged (no re-wrap).
///
/// # Safety
/// `val` must be a valid pointer returned by `mux_rc_alloc` (or null).
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_value_deep_clone(val: *const Value) -> *mut Value {
    if val.is_null() {
        return std::ptr::null_mut();
    }
    if let Value::Object(_) = unsafe { &*val } {
        // copy_object already returns a refcounted *mut Value wrapping the
        // new object; forward it directly so we do not double-wrap.
        return unsafe { crate::object::copy_object(val as *mut Value) };
    }
    let cloned = unsafe { deep_clone_value(&*val) };
    mux_rc_alloc(cloned)
}

#[allow(clippy::mutable_key_type)]
fn deep_clone_value(val: &Value) -> Value {
    match val {
        Value::Unit | Value::Int(_) | Value::Bool(_) | Value::Float(_) => val.clone(),
        Value::String(s) => Value::String(s.clone()),
        Value::List(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(deep_clone_value(item));
            }
            Value::List(out)
        }
        Value::Map(entries) => {
            let mut out = std::collections::BTreeMap::new();
            for (k, v) in entries {
                out.insert(deep_clone_value(k), deep_clone_value(v));
            }
            Value::Map(out)
        }
        Value::Set(items) => {
            let mut out = std::collections::BTreeSet::new();
            for item in items {
                out.insert(deep_clone_value(item));
            }
            Value::Set(out)
        }
        Value::Tuple(t) => Value::Tuple(Box::new(crate::Tuple(
            deep_clone_value(&t.0),
            deep_clone_value(&t.1),
        ))),
        Value::Optional(opt) => match opt {
            Some(inner) => Value::Optional(Some(Box::new(deep_clone_value(inner)))),
            None => Value::Optional(None),
        },
        Value::Result(res) => match res {
            Ok(inner) => Value::Result(Ok(Box::new(deep_clone_value(inner)))),
            Err(inner) => Value::Result(Err(Box::new(deep_clone_value(inner)))),
        },
        Value::Object(obj) => unsafe {
            let temp = Value::Object(obj.clone());
            let copied_ptr = crate::object::copy_object(&temp as *const Value);
            if copied_ptr.is_null() {
                return Value::Object(obj.clone());
            }
            let result = (*copied_ptr).clone();
            crate::object::free_object(copied_ptr);
            result
        },
        Value::Opaque(bytes) => Value::Opaque(bytes.clone()),
    }
}

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
fn layout_for_value() -> Option<Layout> {
    let header_size = std::mem::size_of::<RefHeader>();
    let value_size = std::mem::size_of::<Value>();
    let value_align = std::mem::align_of::<Value>();

    // Ensure proper alignment for Value after header
    let header_align = std::mem::align_of::<RefHeader>();
    let total_align = header_align.max(value_align);

    // Calculate padding between header and value for alignment
    let header_padded = (header_size + value_align - 1) & !(value_align - 1);
    let total_size = header_padded + value_size;

    Layout::from_size_align(total_size, total_align).ok()
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
    let Some(layout) = layout_for_value() else {
        return std::ptr::null_mut();
    };

    unsafe {
        let ptr = alloc(layout);
        if ptr.is_null() {
            return std::ptr::null_mut();
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
            if let Some(layout) = layout_for_value() {
                dealloc(alloc_ptr, layout);
            }

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
    use std::ffi::c_void;

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

    #[test]
    fn test_deep_clone_primitives() {
        let v = mux_rc_alloc(Value::Int(42));
        let cloned = unsafe { mux_value_deep_clone(v) };
        assert!(!cloned.is_null());
        assert_eq!(mux_rc_count(cloned), 1);
        unsafe {
            assert!(matches!(&*cloned, Value::Int(42)));
        }
        assert!(mux_rc_dec(cloned));
        assert!(mux_rc_dec(v));
    }

    #[test]
    fn test_deep_clone_list_is_isolated() {
        let original = mux_rc_alloc(Value::List(vec![
            Value::Int(1),
            Value::String("hello".to_string()),
        ]));
        let cloned = unsafe { mux_value_deep_clone(original) };
        assert!(!cloned.is_null());
        assert_eq!(mux_rc_count(cloned), 1);
        assert_eq!(mux_rc_count(original), 1);

        // Both halves must free cleanly without double-frees.
        assert!(mux_rc_dec(cloned));
        assert!(mux_rc_dec(original));
    }

    #[test]
    fn test_deep_clone_nested_list() {
        let inner = Value::List(vec![Value::Int(1), Value::Int(2)]);
        let outer = mux_rc_alloc(Value::List(vec![inner.clone(), Value::Int(99)]));
        let cloned = unsafe { mux_value_deep_clone(outer) };
        assert!(!cloned.is_null());

        // Verify the cloned tree shape matches.
        unsafe {
            if let Value::List(items) = &*cloned {
                assert_eq!(items.len(), 2);
                if let Value::List(inner_items) = &items[0] {
                    assert_eq!(inner_items.len(), 2);
                } else {
                    panic!("Expected inner list");
                }
                if let Value::Int(99) = items[1] {
                    // ok
                } else {
                    panic!("Expected int at index 1");
                }
            } else {
                panic!("Expected outer list");
            }
        }
        assert!(mux_rc_dec(cloned));
        assert!(mux_rc_dec(outer));
    }

    #[test]
    fn test_deep_clone_object_uses_copy_callback() {
        // Register a class type and a copy callback that swaps a sentinel
        // field.  This verifies that the deep-clone of a `Value::Object`
        // dispatches to `mux_copy_object` rather than sharing the Rc.
        let type_id = crate::object::register_object_type_with_copy(
            "DeepCloneProbe",
            std::mem::size_of::<u64>(),
            None,
            Some(probe_copy as extern "C" fn(*mut c_void, *mut c_void)),
        );
        let original = crate::object::alloc_object(type_id);
        assert!(!original.is_null());

        // Write sentinel into the original.
        unsafe {
            let data = crate::object::get_object_ptr(original) as *mut u64;
            *data = 0xAABB;
        }

        // Deep-clone should call the copy callback and produce a fresh
        // object with the same data, not share the original.
        let cloned = unsafe { mux_value_deep_clone(original) };
        assert!(!cloned.is_null());
        assert_ne!(cloned, original);
        unsafe {
            let data = crate::object::get_object_ptr(cloned) as *const u64;
            assert_eq!(*data, 0xAABB);
        }

        // Both boxes must free cleanly.
        assert!(mux_rc_dec(cloned));
        assert!(mux_rc_dec(original));
    }

    extern "C" fn probe_copy(src: *mut c_void, dst: *mut c_void) {
        unsafe {
            let s = src as *const u64;
            let d = dst as *mut u64;
            *d = *s;
        }
    }
}
