use crate::refcount::{mux_rc_alloc, mux_rc_dec};
use crate::{ObjectRef, TypeId, Value};
use std::collections::HashMap;
use std::ffi::{CStr, c_char, c_void};
use std::sync::Mutex;

lazy_static::lazy_static! {
    static ref TYPE_REGISTRY: Mutex<HashMap<TypeId, ObjectType>> = Mutex::new(HashMap::new());
    static ref NEXT_TYPE_ID: Mutex<TypeId> = Mutex::new(1);
}

#[derive(Clone, Debug)]
pub struct ObjectType {
    pub id: TypeId,
    pub name: String,
    pub size: usize,
    pub destructor: Option<fn(*mut c_void)>,
}

impl ObjectType {
    pub fn new(name: String, size: usize) -> Self {
        let id = {
            let mut next_id = NEXT_TYPE_ID.lock().expect("mutex lock should not fail");
            let id = *next_id;
            *next_id += 1;
            id
        };

        ObjectType {
            id,
            name,
            size,
            destructor: None,
        }
    }
}

pub fn register_object_type(name: &str, size: usize) -> TypeId {
    let obj_type = ObjectType::new(name.to_string(), size);
    let id = obj_type.id;
    TYPE_REGISTRY
        .lock()
        .expect("mutex lock should not fail")
        .insert(id, obj_type);
    id
}

pub fn alloc_object(type_id: TypeId) -> *mut Value {
    let registry = TYPE_REGISTRY.lock().expect("mutex lock should not fail");
    let obj_type = registry.get(&type_id).expect("Invalid type ID");
    let size = obj_type.size;

    // Allocate memory for the object
    let layout = std::alloc::Layout::from_size_align(size, std::mem::align_of::<u8>())
        .expect("memory layout should be valid");
    let ptr = unsafe { std::alloc::alloc(layout) };

    if ptr.is_null() {
        panic!("Failed to allocate object");
    }

    // Create ObjectRef with size for proper cleanup
    let obj_ref = ObjectRef::new(ptr as *mut c_void, type_id, size);

    // Create Value::Object
    let value = Value::Object(obj_ref);

    // Return ref-counted value
    mux_rc_alloc(value)
}

/// # Safety
/// The `obj` pointer must be valid and obtained from `alloc_object` or similar.
/// After calling this function if the ref count reaches 0, the pointer becomes invalid.
///
/// This function decrements the reference count of the Value. When the count
/// reaches 0, the Value is dropped, which drops the ObjectRef, which (via Arc)
/// drops the ObjectData, which frees the underlying object memory.
pub unsafe fn free_object(obj: *mut Value) {
    // Simply decrement the RC - cleanup is automatic via Drop
    mux_rc_dec(obj);
}

/// # Safety
/// The `obj` pointer must be valid and point to a `Value::Object`.
pub unsafe fn get_object_ptr(obj: *const Value) -> *mut c_void {
    if obj.is_null() {
        return std::ptr::null_mut();
    }

    let value = unsafe { &*obj };
    if let Value::Object(obj_ref) = value {
        obj_ref.ptr()
    } else {
        std::ptr::null_mut()
    }
}

/// # Safety
/// The `obj` pointer must be valid and point to a `Value::Object`.
pub unsafe fn get_object_type_id(obj: *const Value) -> TypeId {
    if obj.is_null() {
        return 0;
    }

    let value = unsafe { &*obj };
    if let Value::Object(obj_ref) = value {
        obj_ref.type_id()
    } else {
        0
    }
}

// C API functions
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_register_object_type(name: *const c_char, size: usize) -> TypeId {
    let c_str = unsafe { CStr::from_ptr(name) };
    let name_str = c_str.to_string_lossy().into_owned();
    register_object_type(&name_str, size)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_alloc_object(type_id: TypeId) -> *mut Value {
    alloc_object(type_id)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_free_object(obj: *mut Value) {
    unsafe { free_object(obj) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_get_object_ptr(obj: *const Value) -> *mut c_void {
    unsafe { get_object_ptr(obj) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_get_object_type_id(obj: *const Value) -> TypeId {
    unsafe { get_object_type_id(obj) }
}
