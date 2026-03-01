use crate::object::{alloc_object, get_object_ptr, register_object_type};
use crate::refcount::mux_rc_dec;
use crate::result::MuxResult;
use crate::{Tuple, TypeId, Value};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::ffi::c_void;
use std::io::{Read, Write};
use std::net::{TcpStream as StdTcpStream, UdpSocket as StdUdpSocket};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex};

type SocketMap<T> = Mutex<HashMap<i64, Arc<Mutex<T>>>>;

static NEXT_HANDLE: AtomicI64 = AtomicI64::new(1);

lazy_static! {
    static ref TCP_STREAMS: SocketMap<StdTcpStream> = Mutex::new(HashMap::new());
    static ref UDP_SOCKETS: SocketMap<StdUdpSocket> = Mutex::new(HashMap::new());
}

lazy_static! {
    static ref TCP_STREAM_TYPE_ID: TypeId = register_object_type(
        "TcpStream",
        std::mem::size_of::<i64>(),
        Some(|ptr| drop_socket_handle(&TCP_STREAMS, ptr)),
    );
    static ref UDP_SOCKET_TYPE_ID: TypeId = register_object_type(
        "UdpSocket",
        std::mem::size_of::<i64>(),
        Some(|ptr| drop_socket_handle(&UDP_SOCKETS, ptr)),
    );
}

fn lock_map<'a, T>(map: &'a SocketMap<T>) -> std::sync::MutexGuard<'a, HashMap<i64, Arc<Mutex<T>>>> {
    map.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn next_handle() -> i64 {
    NEXT_HANDLE.fetch_add(1, Ordering::SeqCst)
}

fn insert_socket<T>(map: &SocketMap<T>, socket: T) -> i64 {
    let handle = next_handle();
    lock_map(map).insert(handle, Arc::new(Mutex::new(socket)));
    handle
}

fn remove_socket<T>(map: &SocketMap<T>, handle: i64) {
    lock_map(map).remove(&handle);
}

fn get_socket_entry<T>(map: &SocketMap<T>, handle: i64) -> Option<Arc<Mutex<T>>> {
    lock_map(map).get(&handle).cloned()
}

fn drop_socket_handle<T>(map: &SocketMap<T>, ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let handle = unsafe { *(ptr as *mut i64) };
    if handle == 0 {
        return;
    }
    remove_socket(map, handle);
}

fn socket_entry_or_err<T>(
    map: &SocketMap<T>,
    handle: i64,
    label: &str,
) -> Result<Arc<Mutex<T>>, String> {
    get_socket_entry(map, handle)
        .ok_or_else(|| format!("invalid {} handle", label))
}

fn with_socket<T, R, F>(
    map: &SocketMap<T>,
    handle: i64,
    label: &str,
    op: F,
) -> Result<R, String>
where
    F: FnOnce(&mut T) -> Result<R, String>,
{
    let entry = socket_entry_or_err(map, handle, label)?;
    let mut guard = entry.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    op(&mut guard)
}

fn with_tcp_stream<R, F>(handle: i64, op: F) -> Result<R, String>
where
    F: FnOnce(&mut StdTcpStream) -> Result<R, String>,
{
    with_socket(&TCP_STREAMS, handle, "tcp stream", op)
}

fn with_udp_socket<R, F>(handle: i64, op: F) -> Result<R, String>
where
    F: FnOnce(&mut StdUdpSocket) -> Result<R, String>,
{
    with_socket(&UDP_SOCKETS, handle, "udp socket", op)
}

fn store_tcp_stream(stream: StdTcpStream) -> i64 {
    insert_socket(&TCP_STREAMS, stream)
}

fn store_udp_socket(socket: StdUdpSocket) -> i64 {
    insert_socket(&UDP_SOCKETS, socket)
}

fn remove_tcp_stream(handle: i64) {
    remove_socket(&TCP_STREAMS, handle)
}

fn remove_udp_socket(handle: i64) {
    remove_socket(&UDP_SOCKETS, handle)
}

fn socket_handle(value: *const Value) -> Option<i64> {
    if value.is_null() {
        return None;
    }
    let ptr = unsafe { get_object_ptr(value) };
    if ptr.is_null() {
        return None;
    }
    Some(unsafe { *(ptr as *const i64) })
}

fn require_handle(value: *const Value, label: &str) -> Result<i64, String> {
    socket_handle(value)
        .filter(|&handle| handle != 0)
        .ok_or_else(|| format!("invalid {}", label))
}

fn tcp_handle(value: *const Value) -> Result<i64, String> {
    require_handle(value, "tcp stream")
}

fn udp_handle(value: *const Value) -> Result<i64, String> {
    require_handle(value, "udp socket")
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

fn create_socket_value(handle: i64, type_id: TypeId) -> Value {
    let obj_ptr = alloc_object(type_id);
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
    let byte_list = bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
    let tuple = Tuple(Value::List(byte_list), Value::String(addr));
    Value::Tuple(Box::new(tuple))
}

fn net_result_ok(value: Value) -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::ok(value)))
}

fn net_result_err(msg: String) -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::err(Value::String(msg))))
}

fn net_result_unit(result: Result<(), String>) -> *mut MuxResult {
    match result {
        Ok(()) => net_result_ok(Value::Unit),
        Err(err) => net_result_err(err),
    }
}

fn net_result_string(result: Result<String, String>) -> *mut MuxResult {
    match result {
        Ok(value) => net_result_ok(Value::String(value)),
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_connect(addr: *mut Value) -> *mut MuxResult {
    match value_to_string(addr).and_then(|address| {
        StdTcpStream::connect(address).map_err(|e| format!("failed to connect: {}", e))
    }) {
        Ok(stream) => {
            let handle = store_tcp_stream(stream);
            net_result_ok(create_socket_value(handle, *TCP_STREAM_TYPE_ID))
        }
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_read(stream: *mut Value, size: i64) -> *mut MuxResult {
    if size <= 0 {
        return net_result_ok(Value::List(Vec::new()));
    }
    let handle = match tcp_handle(stream) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    let result = with_tcp_stream(handle, |socket| {
        let mut buf = vec![0u8; size as usize];
        let count = socket
            .read(&mut buf)
            .map_err(|e| format!("tcp read failed: {}", e))?;
        buf.truncate(count);
        Ok(buf)
    });
    match result {
        Ok(bytes) => {
            let values = bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
            net_result_ok(Value::List(values))
        }
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_write(stream: *mut Value, data: *mut Value) -> *mut MuxResult {
    let handle = match tcp_handle(stream) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    let payload = match value_to_bytes(data) {
        Ok(bytes) => bytes,
        Err(err) => return net_result_err(err),
    };
    let result = with_tcp_stream(handle, |socket| {
        socket
            .write(&payload)
            .map_err(|e| format!("tcp write failed: {}", e))
    });
    match result {
        Ok(written) => net_result_ok(Value::Int(written as i64)),
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_close(stream: *mut Value) {
    if let Ok(handle) = tcp_handle(stream) {
        remove_tcp_stream(handle);
        write_handle(stream, 0);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_set_nonblocking(stream: *mut Value, enabled: i32) -> *mut MuxResult {
    let handle = match tcp_handle(stream) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    net_result_unit(with_tcp_stream(handle, |socket| {
        socket
            .set_nonblocking(enabled != 0)
            .map_err(|e| format!("tcp set_nonblocking failed: {}", e))
    }))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_peer_addr(stream: *mut Value) -> *mut MuxResult {
    let result = tcp_handle(stream).and_then(|handle|
        with_tcp_stream(handle, |socket| {
            socket
                .peer_addr()
                .map(|addr| addr.to_string())
                .map_err(|e| format!("tcp peer_addr failed: {}", e))
        })
    );
    net_result_string(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_local_addr(stream: *mut Value) -> *mut MuxResult {
    let result = tcp_handle(stream).and_then(|handle|
        with_tcp_stream(handle, |socket| {
            socket
                .local_addr()
                .map(|addr| addr.to_string())
                .map_err(|e| format!("tcp local_addr failed: {}", e))
        })
    );
    net_result_string(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_bind(addr: *mut Value) -> *mut MuxResult {
    match value_to_string(addr).and_then(|address| {
        StdUdpSocket::bind(address).map_err(|e| format!("udp bind failed: {}", e))
    }) {
        Ok(socket) => {
            let handle = store_udp_socket(socket);
            net_result_ok(create_socket_value(handle, *UDP_SOCKET_TYPE_ID))
        }
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_send_to(
    socket: *mut Value,
    data: *mut Value,
    addr: *mut Value,
) -> *mut MuxResult {
    let handle = match udp_handle(socket) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    let payload = match value_to_bytes(data) {
        Ok(bytes) => bytes,
        Err(err) => return net_result_err(err),
    };
    let destination = match value_to_string(addr) {
        Ok(addr) => addr,
        Err(err) => return net_result_err(err),
    };
    match with_udp_socket(handle, |sock| {
        sock.send_to(&payload, destination.clone())
            .map_err(|e| format!("udp send failed: {}", e))
    }) {
        Ok(written) => net_result_ok(Value::Int(written as i64)),
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_recv_from(socket: *mut Value, size: i64) -> *mut MuxResult {
    if size <= 0 {
        return net_result_err("invalid buffer size".to_string());
    }
    let handle = match udp_handle(socket) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    match with_udp_socket(handle, |sock| {
        let mut buf = vec![0u8; size as usize];
        let result = sock.recv_from(&mut buf).map_err(|e| format!("udp recv failed: {}", e))?;
        buf.truncate(result.0);
        Ok(tuple_from_bytes_and_addr(buf, result.1.to_string()))
    }) {
        Ok(value) => net_result_ok(value),
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_close(socket: *mut Value) {
    if let Ok(handle) = udp_handle(socket) {
        remove_udp_socket(handle);
        write_handle(socket, 0);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_set_nonblocking(socket: *mut Value, enabled: i32) -> *mut MuxResult {
    let handle = match udp_handle(socket) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    net_result_unit(with_udp_socket(handle, |sock| {
        sock.set_nonblocking(enabled != 0)
            .map_err(|e| format!("udp set_nonblocking failed: {}", e))
    }))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_peer_addr(socket: *mut Value) -> *mut MuxResult {
    let result = udp_handle(socket).and_then(|handle|
        with_udp_socket(handle, |sock| {
            sock
                .peer_addr()
                .map(|addr| addr.to_string())
                .map_err(|e| format!("udp peer_addr failed: {}", e))
        })
    );
    net_result_string(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_local_addr(socket: *mut Value) -> *mut MuxResult {
    let result = udp_handle(socket).and_then(|handle|
        with_udp_socket(handle, |sock| {
            sock
                .local_addr()
                .map(|addr| addr.to_string())
                .map_err(|e| format!("udp local_addr failed: {}", e))
        })
    );
    net_result_string(result)
}
