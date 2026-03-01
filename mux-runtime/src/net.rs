use crate::object::{alloc_object, get_object_ptr, register_object_type};
use crate::refcount::{mux_rc_alloc, mux_rc_dec};
use crate::result::MuxResult;
use crate::{Tuple, TypeId, Value};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::ffi::c_void;
use std::io::{Read, Write};
use std::net::{TcpStream as StdTcpStream, UdpSocket as StdUdpSocket};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex};

type TcpEntry = Arc<Mutex<StdTcpStream>>;
type UdpEntry = Arc<Mutex<StdUdpSocket>>;

static NEXT_HANDLE: AtomicI64 = AtomicI64::new(1);

lazy_static! {
    static ref TCP_STREAMS: Mutex<HashMap<i64, TcpEntry>> = Mutex::new(HashMap::new());
    static ref UDP_SOCKETS: Mutex<HashMap<i64, UdpEntry>> = Mutex::new(HashMap::new());
}

fn tcp_stream_destructor(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let handle = unsafe { *(ptr as *mut i64) };
    if handle == 0 {
        return;
    }
    let mut streams = TCP_STREAMS.lock().expect("mutex lock should not fail");
    streams.remove(&handle);
}

fn udp_socket_destructor(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let handle = unsafe { *(ptr as *mut i64) };
    if handle == 0 {
        return;
    }
    let mut sockets = UDP_SOCKETS.lock().expect("mutex lock should not fail");
    sockets.remove(&handle);
}

lazy_static! {
    static ref TCP_STREAM_TYPE_ID: TypeId = register_object_type(
        "TcpStream",
        std::mem::size_of::<i64>(),
        Some(tcp_stream_destructor),
    );
    static ref UDP_SOCKET_TYPE_ID: TypeId = register_object_type(
        "UdpSocket",
        std::mem::size_of::<i64>(),
        Some(udp_socket_destructor),
    );
}

fn next_handle() -> i64 {
    NEXT_HANDLE.fetch_add(1, Ordering::SeqCst)
}

fn store_tcp_stream(stream: StdTcpStream) -> i64 {
    let handle = next_handle();
    let entry = Arc::new(Mutex::new(stream));
    TCP_STREAMS
        .lock()
        .expect("mutex lock should not fail")
        .insert(handle, entry);
    handle
}

fn store_udp_socket(socket: StdUdpSocket) -> i64 {
    let handle = next_handle();
    let entry = Arc::new(Mutex::new(socket));
    UDP_SOCKETS
        .lock()
        .expect("mutex lock should not fail")
        .insert(handle, entry);
    handle
}

fn remove_tcp_stream(handle: i64) {
    TCP_STREAMS
        .lock()
        .expect("mutex lock should not fail")
        .remove(&handle);
}

fn remove_udp_socket(handle: i64) {
    UDP_SOCKETS
        .lock()
        .expect("mutex lock should not fail")
        .remove(&handle);
}

fn get_tcp_entry(handle: i64) -> Option<TcpEntry> {
    TCP_STREAMS
        .lock()
        .expect("mutex lock should not fail")
        .get(&handle)
        .cloned()
}

fn get_udp_entry(handle: i64) -> Option<UdpEntry> {
    UDP_SOCKETS
        .lock()
        .expect("mutex lock should not fail")
        .get(&handle)
        .cloned()
}

fn tcp_handle(value: *const Value) -> Option<i64> {
    if value.is_null() {
        return None;
    }
    let ptr = unsafe { get_object_ptr(value) };
    if ptr.is_null() {
        return None;
    }
    Some(unsafe { *(ptr as *const i64) })
}

fn udp_handle(value: *const Value) -> Option<i64> {
    if value.is_null() {
        return None;
    }
    let ptr = unsafe { get_object_ptr(value) };
    if ptr.is_null() {
        return None;
    }
    Some(unsafe { *(ptr as *const i64) })
}

fn write_handle(value: *mut Value, handle: i64) {
    if value.is_null() {
        return;
    }
    let ptr = unsafe { get_object_ptr(value) };
    if ptr.is_null() {
        return;
    }
    unsafe { *(ptr as *mut i64) = handle };
}

fn create_tcp_value(handle: i64) -> Value {
    let obj_ptr = alloc_object(*TCP_STREAM_TYPE_ID);
    let data_ptr = unsafe { get_object_ptr(obj_ptr) };
    if !data_ptr.is_null() {
        unsafe { *(data_ptr as *mut i64) = handle };
    }
    let value = unsafe { (*obj_ptr).clone() };
    unsafe { mux_rc_dec(obj_ptr) };
    value
}

fn create_udp_value(handle: i64) -> Value {
    let obj_ptr = alloc_object(*UDP_SOCKET_TYPE_ID);
    let data_ptr = unsafe { get_object_ptr(obj_ptr) };
    if !data_ptr.is_null() {
        unsafe { *(data_ptr as *mut i64) = handle };
    }
    let value = unsafe { (*obj_ptr).clone() };
    unsafe { mux_rc_dec(obj_ptr) };
    value
}

fn value_to_string(value: *mut Value) -> Result<String, String> {
    if value.is_null() {
        return Err("string pointer is null".to_string());
    }
    let val = unsafe { &*value };
    if let Value::String(s) = val {
        Ok(s.clone())
    } else {
        Err("expected string".to_string())
    }
}

fn value_to_bytes(list: *mut Value) -> Result<Vec<u8>, String> {
    if list.is_null() {
        return Ok(Vec::new());
    }
    let val = unsafe { &*list };
    if let Value::List(vec) = val {
        let mut bytes = Vec::with_capacity(vec.len());
        for item in vec {
            if let Value::Int(i) = item {
                if *i < 0 || *i > 255 {
                    return Err("byte value out of range".to_string());
                }
                bytes.push(*i as u8);
            } else {
                return Err("bytes list must contain ints".to_string());
            }
        }
        Ok(bytes)
    } else {
        Err("expected list of ints".to_string())
    }
}

fn tuple_from_bytes_and_addr(bytes: Vec<u8>, addr: String) -> Value {
    let byte_list = bytes.iter().map(|b| Value::Int(*b as i64)).collect();
    let tuple = Tuple(Value::List(byte_list), Value::String(addr));
    Value::Tuple(Box::new(tuple))
}

fn net_result_ok(value: Value) -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::ok(value)))
}

fn net_result_err(msg: String) -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::err(Value::String(msg))))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_connect(addr: *mut Value) -> *mut MuxResult {
    match value_to_string(addr) {
        Ok(address) => match StdTcpStream::connect(address) {
            Ok(socket) => {
                let handle = store_tcp_stream(socket);
                net_result_ok(create_tcp_value(handle))
            }
            Err(e) => net_result_err(format!("failed to connect: {}", e)),
        },
        Err(e) => net_result_err(e),
    }
}

fn stream_guard(handle: i64) -> Result<TcpEntry, String> {
    get_tcp_entry(handle).ok_or_else(|| "invalid tcp stream".to_string())
}

fn socket_guard(handle: i64) -> Result<UdpEntry, String> {
    get_udp_entry(handle).ok_or_else(|| "invalid udp socket".to_string())
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_read(stream: *mut Value, size: i64) -> *mut MuxResult {
    let handle = match tcp_handle(stream) {
        Some(h) if h != 0 => h,
        _ => return net_result_err("invalid tcp stream".to_string()),
    };
    let guard = match stream_guard(handle) {
        Ok(entry) => entry,
        Err(err) => return net_result_err(err),
    };
    let mut socket = guard.lock().expect("mutex lock should not fail");
    if size <= 0 {
        return net_result_ok(Value::List(Vec::new()));
    }
    let mut buf = vec![0u8; size as usize];
    match socket.read(&mut buf) {
        Ok(n) => {
            buf.truncate(n);
            net_result_ok(Value::List(buf.into_iter().map(|b| Value::Int(b as i64)).collect()))
        }
        Err(e) => net_result_err(format!("tcp read failed: {}", e)),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_write(stream: *mut Value, data: *mut Value) -> *mut MuxResult {
    let handle = match tcp_handle(stream) {
        Some(h) if h != 0 => h,
        _ => return net_result_err("invalid tcp stream".to_string()),
    };
    let guard = match stream_guard(handle) {
        Ok(entry) => entry,
        Err(err) => return net_result_err(err),
    };
    let bytes = match value_to_bytes(data) {
        Ok(b) => b,
        Err(err) => return net_result_err(err),
    };
    let mut socket = guard.lock().expect("mutex lock should not fail");
    match socket.write(&bytes) {
        Ok(n) => net_result_ok(Value::Int(n as i64)),
        Err(e) => net_result_err(format!("tcp write failed: {}", e)),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_close(stream: *mut Value) {
    if let Some(handle) = tcp_handle(stream) {
        remove_tcp_stream(handle);
        write_handle(stream, 0);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_set_nonblocking(stream: *mut Value, enabled: i32) {
    if let Some(handle) = tcp_handle(stream) {
        if let Ok(entry) = stream_guard(handle) {
            let mut socket = entry.lock().expect("mutex lock should not fail");
            if let Err(e) = socket.set_nonblocking(enabled != 0) {
                eprintln!("failed to set non-blocking: {}", e);
            }
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_peer_addr(stream: *mut Value) -> *mut Value {
    let handle = tcp_handle(stream).unwrap_or(0);
    if handle == 0 {
        return mux_rc_alloc(Value::String("".to_string()));
    }
    match stream_guard(handle) {
        Ok(entry) => {
            let socket = entry.lock().expect("mutex lock should not fail");
            match socket.peer_addr() {
                Ok(addr) => mux_rc_alloc(Value::String(addr.to_string())),
                Err(e) => mux_rc_alloc(Value::String(e.to_string())),
            }
        }
        Err(_) => mux_rc_alloc(Value::String("".to_string())),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_local_addr(stream: *mut Value) -> *mut Value {
    let handle = tcp_handle(stream).unwrap_or(0);
    if handle == 0 {
        return mux_rc_alloc(Value::String("".to_string()));
    }
    match stream_guard(handle) {
        Ok(entry) => {
            let socket = entry.lock().expect("mutex lock should not fail");
            match socket.local_addr() {
                Ok(addr) => mux_rc_alloc(Value::String(addr.to_string())),
                Err(e) => mux_rc_alloc(Value::String(e.to_string())),
            }
        }
        Err(_) => mux_rc_alloc(Value::String("".to_string())),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_bind(addr: *mut Value) -> *mut MuxResult {
    match value_to_string(addr) {
        Ok(address) => match StdUdpSocket::bind(address) {
            Ok(socket) => {
                let handle = store_udp_socket(socket);
                net_result_ok(create_udp_value(handle))
            }
            Err(e) => net_result_err(format!("udp bind failed: {}", e)),
        },
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_send_to(socket: *mut Value, data: *mut Value, addr: *mut Value) -> *mut MuxResult {
    let handle = match udp_handle(socket) {
        Some(h) if h != 0 => h,
        _ => return net_result_err("invalid udp socket".to_string()),
    };
    let guard = match socket_guard(handle) {
        Ok(entry) => entry,
        Err(err) => return net_result_err(err),
    };
    let payload = match value_to_bytes(data) {
        Ok(b) => b,
        Err(err) => return net_result_err(err),
    };
    let destination = match value_to_string(addr) {
        Ok(a) => a,
        Err(err) => return net_result_err(err),
    };
    let mut socket = guard.lock().expect("mutex lock should not fail");
    match socket.send_to(&payload, destination) {
        Ok(n) => net_result_ok(Value::Int(n as i64)),
        Err(e) => net_result_err(format!("udp send failed: {}", e)),
    }
}

fn tuple_result(data: Vec<u8>, addr: String) -> Value {
    tuple_from_bytes_and_addr(data, addr)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_recv_from(socket: *mut Value, size: i64) -> *mut MuxResult {
    let handle = match udp_handle(socket) {
        Some(h) if h != 0 => h,
        _ => return net_result_err("invalid udp socket".to_string()),
    };
    let guard = match socket_guard(handle) {
        Ok(entry) => entry,
        Err(err) => return net_result_err(err),
    };
    if size <= 0 {
        return net_result_err("invalid buffer size".to_string());
    }
    let mut buf = vec![0u8; size as usize];
    let mut socket = guard.lock().expect("mutex lock should not fail");
    match socket.recv_from(&mut buf) {
        Ok((n, addr)) => {
            buf.truncate(n);
            net_result_ok(tuple_result(buf, addr.to_string()))
        }
        Err(e) => net_result_err(format!("udp recv failed: {}", e)),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_close(socket: *mut Value) {
    if let Some(handle) = udp_handle(socket) {
        remove_udp_socket(handle);
        write_handle(socket, 0);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_set_nonblocking(socket: *mut Value, enabled: i32) {
    if let Some(handle) = udp_handle(socket) {
        if let Ok(entry) = socket_guard(handle) {
            let mut sock = entry.lock().expect("mutex lock should not fail");
            if let Err(e) = sock.set_nonblocking(enabled != 0) {
                eprintln!("failed to set udp non-blocking: {}", e);
            }
        }
    }
}
