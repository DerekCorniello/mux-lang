use crate::{TypeId, Value, ObjectRef};
use std::collections::HashMap;
use std::ffi::{CStr, c_char, c_void};
use std::sync::Mutex;
use std::sync::atomic::Ordering;
use std::alloc;

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
            let mut next_id = NEXT_TYPE_ID.lock().unwrap();
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
    TYPE_REGISTRY.lock().unwrap().insert(id, obj_type);
    id
}

pub fn alloc_object(type_id: TypeId) -> *mut Value {
    let registry = TYPE_REGISTRY.lock().unwrap();
    let obj_type = registry.get(&type_id).expect("Invalid type ID");

    // Allocate memory for the object
    let layout = std::alloc::Layout::from_size_align(obj_type.size, std::mem::align_of::<u8>()).unwrap();
    let ptr = unsafe { std::alloc::alloc(layout) };

    if ptr.is_null() {
        panic!("Failed to allocate object");
    }

    // Create ObjectRef
    let obj_ref = ObjectRef::new(ptr as *mut c_void, type_id);

    // Create Value::Object
    let value = Value::Object(obj_ref.clone());

    println!("Allocated object of type {} at {:p}, ref_count: {}", obj_type.name, ptr, obj_ref.ref_count.load(Ordering::Relaxed));

    // Return boxed value
    Box::into_raw(Box::new(value))
}

pub fn free_object(obj: *mut Value) {
    if obj.is_null() {
        return;
    }

    let value = unsafe { Box::from_raw(obj) };
    if let Value::Object(obj_ref) = *value {
        // Decrement ref count
        let ref_count = obj_ref.dec_ref();
        println!("Freeing object at {:p}, new ref_count: {}", obj_ref.ptr, ref_count);
        if ref_count == 0 {
            println!("Deallocating object at {:p}", obj_ref.ptr);
            // Get object type for cleanup
            if let Some(obj_type) = TYPE_REGISTRY.lock().unwrap().get(&obj_ref.type_id) {
                // Call destructor if present
                if let Some(destructor) = obj_type.destructor {
                    destructor(obj_ref.ptr);
                }

                // Free the object memory
                let layout = std::alloc::Layout::from_size_align(obj_type.size, std::mem::align_of::<u8>()).unwrap();
                unsafe { std::alloc::dealloc(obj_ref.ptr as *mut u8, layout) };
            }
        }
    }
    // value is dropped here
}

pub fn get_object_ptr(obj: *const Value) -> *mut c_void {
    if obj.is_null() {
        return std::ptr::null_mut();
    }

    let value = unsafe { &*obj };
    if let Value::Object(obj_ref) = value {
        obj_ref.ptr
    } else {
        std::ptr::null_mut()
    }
}

pub fn get_object_type_id(obj: *const Value) -> TypeId {
    if obj.is_null() {
        return 0;
    }

    let value = unsafe { &*obj };
    if let Value::Object(obj_ref) = value {
        obj_ref.type_id
    } else {
        0
    }
}

// C API functions
#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_register_object_type(name: *const c_char, size: usize) -> TypeId {
    let c_str = unsafe { CStr::from_ptr(name) };
    let name_str = c_str.to_str().unwrap();
    register_object_type(name_str, size)
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_alloc_object(type_id: TypeId) -> *mut Value {
    alloc_object(type_id)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_free_object(obj: *mut Value) {
    free_object(obj)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_get_object_ptr(obj: *const Value) -> *mut c_void {
    get_object_ptr(obj)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_get_object_type_id(obj: *const Value) -> TypeId {
    get_object_type_id(obj)
}