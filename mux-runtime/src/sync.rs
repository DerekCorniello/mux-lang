use crate::Value;
use crate::result::MuxResult;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::ffi::c_void;
use std::mem::MaybeUninit;
use std::sync::Mutex;
use std::sync::atomic::{AtomicI64, Ordering};
use std::thread;
use std::time::Duration;

#[repr(C)]
struct ClosureRepr {
    function_ptr: *mut c_void,
    captures_ptr: *mut c_void,
}

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
}

fn ok_unit() -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::ok(Value::Unit)))
}

fn err_string(message: impl Into<String>) -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::err(message.into())))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
fn extract_handle_id(handle: *mut Value, type_name: &str) -> Result<i64, *mut MuxResult> {
    if handle.is_null() {
        return Err(err_string(format!("{} handle is null", type_name)));
    }
    let value = unsafe { &*handle };
    match value {
        Value::Int(id) => Ok(*id),
        _ => Err(err_string(format!(
            "Invalid {} handle representation",
            type_name
        ))),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sync_spawn(closure: *mut c_void) -> *mut MuxResult {
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
    Box::into_raw(Box::new(MuxResult::ok(Value::Int(id))))
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
        match threads.get_mut(&id) {
            Some(entry) => entry.handle.take(),
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
    let Some(entry) = threads.get_mut(&id) else {
        return err_string(format!("Thread handle {} not found", id));
    };
    if entry.handle.take().is_none() {
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
            Box::into_raw(Box::new(Value::Int(id)))
        }
        Err(_) => std::ptr::null_mut(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_rwlock_new() -> *mut Value {
    match init_pthread_rwlock() {
        Ok(ptr) => {
            let id = NEXT_RWLOCK_ID.fetch_add(1, Ordering::Relaxed);
            let mut rwlocks = RWLOCKS.lock().expect("RWLOCKS lock should not be poisoned");
            rwlocks.insert(id, ptr as usize);
            Box::into_raw(Box::new(Value::Int(id)))
        }
        Err(_) => std::ptr::null_mut(),
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
            Box::into_raw(Box::new(Value::Int(id)))
        }
        Err(_) => std::ptr::null_mut(),
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
