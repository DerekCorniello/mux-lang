use crate::TypeId;
use crate::Value;
use crate::object::{alloc_object, get_object_ptr, register_object_type};
use crate::refcount::mux_rc_dec;
use crate::result::MuxResult;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::ffi::c_void;
use std::mem::MaybeUninit;
use std::sync::Mutex;
use std::sync::atomic::{AtomicI64, Ordering};
use std::thread;
use std::time::Duration;

/// Closure representation as produced by the Mux compiler.
///
/// INVARIANTS (must match codegen exactly):
/// - Field order: function_ptr MUST be first, captures_ptr MUST be second
/// - captures_ptr == null if and only if the closure has no captures
/// - function_ptr always points to a valid function with signature:
///   - `extern "C" fn()` if captures_ptr is null
///   - `extern "C" fn(*mut c_void)` if captures_ptr is non-null
///
/// These invariants are critical for safe transmutation in `mux_sync_spawn`.
/// If the compiler's closure representation changes, this must be updated.
#[repr(C)]
struct ClosureRepr {
    function_ptr: *mut c_void,
    captures_ptr: *mut c_void,
}

// Compile-time assertion: ClosureRepr layout assumptions
const _: () = {
    const fn assert_closure_layout() {
        let _ = std::mem::transmute::<ClosureRepr, [*mut c_void; 2]>;
    }
    assert_closure_layout();
};

struct ThreadEntry {
    handle: Option<thread::JoinHandle<()>>,
}

lazy_static! {
    static ref NEXT_THREAD_ID: AtomicI64 = AtomicI64::new(1);
    static ref THREADS: Mutex<HashMap<i64, ThreadEntry>> = Mutex::new(HashMap::new());
    static ref NEXT_MUTEX_ID: AtomicI64 = AtomicI64::new(1);
    static ref MUTEXES: Mutex<HashMap<i64, usize>> = Mutex::new(HashMap::new());
    static ref NEXT_RWLOCK_ID: AtomicI64 = AtomicI64::new(1);
    static ref RWLOCKS: Mutex<HashMap<i64, usize>> = Mutex::new(HashMap::new());
    static ref NEXT_CONDVAR_ID: AtomicI64 = AtomicI64::new(1);
    static ref CONDVARS: Mutex<HashMap<i64, usize>> = Mutex::new(HashMap::new());
    static ref MUTEX_TYPE_ID: TypeId =
        register_object_type("Mutex", 8, Some(destroy_mutex_object as fn(*mut c_void)));
    static ref RWLOCK_TYPE_ID: TypeId =
        register_object_type("RwLock", 8, Some(destroy_rwlock_object as fn(*mut c_void)));
    static ref CONDVAR_TYPE_ID: TypeId = register_object_type(
        "CondVar",
        8,
        Some(destroy_condvar_object as fn(*mut c_void))
    );
    static ref THREAD_TYPE_ID: TypeId =
        register_object_type("Thread", 8, Some(destroy_thread_object as fn(*mut c_void)));
}

fn ok_unit() -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::ok(Value::Unit)))
}

fn err_string(message: impl Into<String>) -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::err(message.into())))
}

fn destroy_mutex_object(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let id = unsafe { *(ptr as *mut i64) };
    let mutex_ptr = {
        let mut mutexes = MUTEXES.lock().expect("MUTEXES lock should not be poisoned");
        mutexes.remove(&id).map(|p| p as *mut libc::pthread_mutex_t)
    };
    if let Some(mutex_ptr) = mutex_ptr {
        unsafe {
            let _ = libc::pthread_mutex_destroy(mutex_ptr);
            drop(Box::from_raw(mutex_ptr));
        }
    }
}

fn destroy_rwlock_object(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let id = unsafe { *(ptr as *mut i64) };
    let rwlock_ptr = {
        let mut rwlocks = RWLOCKS.lock().expect("RWLOCKS lock should not be poisoned");
        rwlocks
            .remove(&id)
            .map(|p| p as *mut libc::pthread_rwlock_t)
    };
    if let Some(rwlock_ptr) = rwlock_ptr {
        unsafe {
            let _ = libc::pthread_rwlock_destroy(rwlock_ptr);
            drop(Box::from_raw(rwlock_ptr));
        }
    }
}

fn destroy_condvar_object(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let id = unsafe { *(ptr as *mut i64) };
    let condvar_ptr = {
        let mut condvars = CONDVARS
            .lock()
            .expect("CONDVARS lock should not be poisoned");
        condvars.remove(&id).map(|p| p as *mut libc::pthread_cond_t)
    };
    if let Some(condvar_ptr) = condvar_ptr {
        unsafe {
            let _ = libc::pthread_cond_destroy(condvar_ptr);
            drop(Box::from_raw(condvar_ptr));
        }
    }
}

fn destroy_thread_object(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let id = unsafe { *(ptr as *mut i64) };
    let _entry = {
        let mut threads = THREADS.lock().expect("THREADS lock should not be poisoned");
        threads.remove(&id)
    };
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
fn extract_handle_id(handle: *mut Value, _type_name: &str) -> Result<i64, *mut MuxResult> {
    if handle.is_null() {
        return Err(err_string("handle is null"));
    }
    let ptr = unsafe { get_object_ptr(handle) };
    if ptr.is_null() {
        return Err(err_string("handle data is null"));
    }
    Ok(unsafe { *(ptr as *const i64) })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sync_spawn(closure: *mut c_void) -> *mut MuxResult {
    // SAFETY: `closure` must be a pointer to a ClosureRepr as produced by the Mux compiler.
    // The ClosureRepr struct documents the invariants that make this safe.
    if closure.is_null() {
        return err_string("sync.spawn received null function value");
    }

    let closure_addr = closure as usize;
    let handle = thread::Builder::new().spawn(move || {
        let closure_ptr = closure_addr as *mut ClosureRepr;
        if closure_ptr.is_null() {
            return;
        }
        let closure_ref = unsafe { &*closure_ptr };
        // Dispatch based on captures: null captures_ptr means no captures
        if closure_ref.captures_ptr.is_null() {
            let func: extern "C" fn() = unsafe { std::mem::transmute(closure_ref.function_ptr) };
            func();
        } else {
            let func: extern "C" fn(*mut c_void) =
                unsafe { std::mem::transmute(closure_ref.function_ptr) };
            func(closure_ref.captures_ptr);
        }
    });

    let join_handle = match handle {
        Ok(h) => h,
        Err(e) => return err_string(format!("Failed to spawn thread: {}", e)),
    };

    let id = NEXT_THREAD_ID.fetch_add(1, Ordering::Relaxed);
    let mut threads = THREADS
        .lock()
        .expect("THREADS mutex lock should not be poisoned");
    threads.insert(
        id,
        ThreadEntry {
            handle: Some(join_handle),
        },
    );
    drop(threads);

    let obj_ptr = alloc_object(*THREAD_TYPE_ID);
    let data_ptr = unsafe { get_object_ptr(obj_ptr) };
    if !data_ptr.is_null() {
        unsafe { *(data_ptr as *mut i64) = id };
    }
    let value = unsafe { (*obj_ptr).clone() };
    mux_rc_dec(obj_ptr);
    Box::into_raw(Box::new(MuxResult::ok(value)))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_thread_join(thread_handle: *mut Value) -> *mut MuxResult {
    let id = match extract_handle_id(thread_handle, "Thread") {
        Ok(id) => id,
        Err(e) => return e,
    };

    let join_handle = {
        let mut threads = THREADS
            .lock()
            .expect("THREADS mutex lock should not be poisoned");
        match threads.remove(&id) {
            Some(entry) => entry.handle,
            None => return err_string(format!("Thread handle {} not found", id)),
        }
    };

    let Some(handle) = join_handle else {
        return err_string("Thread already joined or detached");
    };

    match handle.join() {
        Ok(_) => ok_unit(),
        Err(_) => err_string("Thread panicked during execution"),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_thread_detach(thread_handle: *mut Value) -> *mut MuxResult {
    let id = match extract_handle_id(thread_handle, "Thread") {
        Ok(id) => id,
        Err(e) => return e,
    };

    let mut threads = THREADS
        .lock()
        .expect("THREADS mutex lock should not be poisoned");
    let Some(entry) = threads.remove(&id) else {
        return err_string(format!("Thread handle {} not found", id));
    };
    if entry.handle.is_none() {
        return err_string("Thread already joined or detached");
    }
    ok_unit()
}

fn init_pthread_mutex() -> Result<*mut libc::pthread_mutex_t, String> {
    let mut mutex = Box::new(MaybeUninit::<libc::pthread_mutex_t>::uninit());
    let rc = unsafe { libc::pthread_mutex_init(mutex.as_mut_ptr(), std::ptr::null()) };
    if rc != 0 {
        return Err(format!("pthread_mutex_init failed with error code {}", rc));
    }
    let initialized = unsafe { Box::<MaybeUninit<libc::pthread_mutex_t>>::assume_init(mutex) };
    Ok(Box::into_raw(initialized))
}

fn init_pthread_rwlock() -> Result<*mut libc::pthread_rwlock_t, String> {
    let mut rwlock = Box::new(MaybeUninit::<libc::pthread_rwlock_t>::uninit());
    let rc = unsafe { libc::pthread_rwlock_init(rwlock.as_mut_ptr(), std::ptr::null()) };
    if rc != 0 {
        return Err(format!("pthread_rwlock_init failed with error code {}", rc));
    }
    let initialized = unsafe { Box::<MaybeUninit<libc::pthread_rwlock_t>>::assume_init(rwlock) };
    Ok(Box::into_raw(initialized))
}

fn init_pthread_condvar() -> Result<*mut libc::pthread_cond_t, String> {
    let mut condvar = Box::new(MaybeUninit::<libc::pthread_cond_t>::uninit());
    let rc = unsafe { libc::pthread_cond_init(condvar.as_mut_ptr(), std::ptr::null()) };
    if rc != 0 {
        return Err(format!("pthread_cond_init failed with error code {}", rc));
    }
    let initialized = unsafe { Box::<MaybeUninit<libc::pthread_cond_t>>::assume_init(condvar) };
    Ok(Box::into_raw(initialized))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_mutex_new() -> *mut Value {
    match init_pthread_mutex() {
        Ok(ptr) => {
            let id = NEXT_MUTEX_ID.fetch_add(1, Ordering::Relaxed);
            let mut mutexes = MUTEXES.lock().expect("MUTEXES lock should not be poisoned");
            mutexes.insert(id, ptr as usize);

            let obj_ptr = alloc_object(*MUTEX_TYPE_ID);
            let data_ptr = unsafe { get_object_ptr(obj_ptr) };
            if !data_ptr.is_null() {
                unsafe { *(data_ptr as *mut i64) = id };
            }
            let value = unsafe { (*obj_ptr).clone() };
            mux_rc_dec(obj_ptr);
            Box::into_raw(Box::new(value))
        }
        Err(e) => panic!("Failed to initialize Mutex: {}", e),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_rwlock_new() -> *mut Value {
    match init_pthread_rwlock() {
        Ok(ptr) => {
            let id = NEXT_RWLOCK_ID.fetch_add(1, Ordering::Relaxed);
            let mut rwlocks = RWLOCKS.lock().expect("RWLOCKS lock should not be poisoned");
            rwlocks.insert(id, ptr as usize);

            let obj_ptr = alloc_object(*RWLOCK_TYPE_ID);
            let data_ptr = unsafe { get_object_ptr(obj_ptr) };
            if !data_ptr.is_null() {
                unsafe { *(data_ptr as *mut i64) = id };
            }
            let value = unsafe { (*obj_ptr).clone() };
            mux_rc_dec(obj_ptr);
            Box::into_raw(Box::new(value))
        }
        Err(e) => panic!("Failed to initialize RwLock: {}", e),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_condvar_new() -> *mut Value {
    match init_pthread_condvar() {
        Ok(ptr) => {
            let id = NEXT_CONDVAR_ID.fetch_add(1, Ordering::Relaxed);
            let mut condvars = CONDVARS
                .lock()
                .expect("CONDVARS lock should not be poisoned");
            condvars.insert(id, ptr as usize);

            let obj_ptr = alloc_object(*CONDVAR_TYPE_ID);
            let data_ptr = unsafe { get_object_ptr(obj_ptr) };
            if !data_ptr.is_null() {
                unsafe { *(data_ptr as *mut i64) = id };
            }
            let value = unsafe { (*obj_ptr).clone() };
            mux_rc_dec(obj_ptr);
            Box::into_raw(Box::new(value))
        }
        Err(e) => panic!("Failed to initialize CondVar: {}", e),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_mutex_lock(mutex_handle: *mut Value) -> *mut MuxResult {
    let id = match extract_handle_id(mutex_handle, "Mutex") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let mutex_ptr = {
        let mutexes = MUTEXES.lock().expect("MUTEXES lock should not be poisoned");
        match mutexes.get(&id) {
            Some(ptr) => *ptr as *mut libc::pthread_mutex_t,
            None => return err_string(format!("Mutex handle {} not found", id)),
        }
    };

    let rc = unsafe { libc::pthread_mutex_lock(mutex_ptr) };
    if rc != 0 {
        return err_string(format!("pthread_mutex_lock failed with error code {}", rc));
    }
    ok_unit()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_mutex_unlock(mutex_handle: *mut Value) -> *mut MuxResult {
    let id = match extract_handle_id(mutex_handle, "Mutex") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let mutex_ptr = {
        let mutexes = MUTEXES.lock().expect("MUTEXES lock should not be poisoned");
        match mutexes.get(&id) {
            Some(ptr) => *ptr as *mut libc::pthread_mutex_t,
            None => return err_string(format!("Mutex handle {} not found", id)),
        }
    };

    let rc = unsafe { libc::pthread_mutex_unlock(mutex_ptr) };
    if rc != 0 {
        return err_string(format!(
            "pthread_mutex_unlock failed with error code {}",
            rc
        ));
    }
    ok_unit()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_rwlock_read_lock(rwlock_handle: *mut Value) -> *mut MuxResult {
    let id = match extract_handle_id(rwlock_handle, "RwLock") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let rwlock_ptr = {
        let rwlocks = RWLOCKS.lock().expect("RWLOCKS lock should not be poisoned");
        match rwlocks.get(&id) {
            Some(ptr) => *ptr as *mut libc::pthread_rwlock_t,
            None => return err_string(format!("RwLock handle {} not found", id)),
        }
    };

    let rc = unsafe { libc::pthread_rwlock_rdlock(rwlock_ptr) };
    if rc != 0 {
        return err_string(format!(
            "pthread_rwlock_rdlock failed with error code {}",
            rc
        ));
    }
    ok_unit()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_rwlock_write_lock(rwlock_handle: *mut Value) -> *mut MuxResult {
    let id = match extract_handle_id(rwlock_handle, "RwLock") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let rwlock_ptr = {
        let rwlocks = RWLOCKS.lock().expect("RWLOCKS lock should not be poisoned");
        match rwlocks.get(&id) {
            Some(ptr) => *ptr as *mut libc::pthread_rwlock_t,
            None => return err_string(format!("RwLock handle {} not found", id)),
        }
    };

    let rc = unsafe { libc::pthread_rwlock_wrlock(rwlock_ptr) };
    if rc != 0 {
        return err_string(format!(
            "pthread_rwlock_wrlock failed with error code {}",
            rc
        ));
    }
    ok_unit()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_rwlock_unlock(rwlock_handle: *mut Value) -> *mut MuxResult {
    let id = match extract_handle_id(rwlock_handle, "RwLock") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let rwlock_ptr = {
        let rwlocks = RWLOCKS.lock().expect("RWLOCKS lock should not be poisoned");
        match rwlocks.get(&id) {
            Some(ptr) => *ptr as *mut libc::pthread_rwlock_t,
            None => return err_string(format!("RwLock handle {} not found", id)),
        }
    };

    let rc = unsafe { libc::pthread_rwlock_unlock(rwlock_ptr) };
    if rc != 0 {
        return err_string(format!(
            "pthread_rwlock_unlock failed with error code {}",
            rc
        ));
    }
    ok_unit()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_condvar_wait(
    condvar_handle: *mut Value,
    mutex_handle: *mut Value,
) -> *mut MuxResult {
    let cond_id = match extract_handle_id(condvar_handle, "CondVar") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let mutex_id = match extract_handle_id(mutex_handle, "Mutex") {
        Ok(id) => id,
        Err(e) => return e,
    };

    let cond_ptr = {
        let condvars = CONDVARS
            .lock()
            .expect("CONDVARS lock should not be poisoned");
        match condvars.get(&cond_id) {
            Some(ptr) => *ptr as *mut libc::pthread_cond_t,
            None => return err_string(format!("CondVar handle {} not found", cond_id)),
        }
    };
    let mutex_ptr = {
        let mutexes = MUTEXES.lock().expect("MUTEXES lock should not be poisoned");
        match mutexes.get(&mutex_id) {
            Some(ptr) => *ptr as *mut libc::pthread_mutex_t,
            None => return err_string(format!("Mutex handle {} not found", mutex_id)),
        }
    };

    let rc = unsafe { libc::pthread_cond_wait(cond_ptr, mutex_ptr) };
    if rc != 0 {
        return err_string(format!("pthread_cond_wait failed with error code {}", rc));
    }
    ok_unit()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_condvar_signal(condvar_handle: *mut Value) -> *mut MuxResult {
    let id = match extract_handle_id(condvar_handle, "CondVar") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let cond_ptr = {
        let condvars = CONDVARS
            .lock()
            .expect("CONDVARS lock should not be poisoned");
        match condvars.get(&id) {
            Some(ptr) => *ptr as *mut libc::pthread_cond_t,
            None => return err_string(format!("CondVar handle {} not found", id)),
        }
    };

    let rc = unsafe { libc::pthread_cond_signal(cond_ptr) };
    if rc != 0 {
        return err_string(format!("pthread_cond_signal failed with error code {}", rc));
    }
    ok_unit()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_condvar_broadcast(condvar_handle: *mut Value) -> *mut MuxResult {
    let id = match extract_handle_id(condvar_handle, "CondVar") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let cond_ptr = {
        let condvars = CONDVARS
            .lock()
            .expect("CONDVARS lock should not be poisoned");
        match condvars.get(&id) {
            Some(ptr) => *ptr as *mut libc::pthread_cond_t,
            None => return err_string(format!("CondVar handle {} not found", id)),
        }
    };

    let rc = unsafe { libc::pthread_cond_broadcast(cond_ptr) };
    if rc != 0 {
        return err_string(format!(
            "pthread_cond_broadcast failed with error code {}",
            rc
        ));
    }
    ok_unit()
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_sync_sleep(ms: i64) {
    if ms <= 0 {
        return;
    }
    thread::sleep(Duration::from_millis(ms as u64));
}
