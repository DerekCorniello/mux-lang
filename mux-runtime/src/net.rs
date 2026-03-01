use std::collections::BTreeMap;
use std::ffi::CStr;
use std::io::{Read, Write};
use std::net::{TcpStream as StdTcpStream, UdpSocket as StdUdpSocket, Shutdown};
use std::os::raw::c_char;
use std::sync::Mutex;

use crate::object::{register_object_type, ObjectRef, TypeId};
use crate::result::MuxResult;
use crate::Value;

lazy_static::lazy_static! {
    static ref TCP_STREAM_TYPE_ID: Mutex<Option<TypeId>> = Mutex::new(None);
    static ref UDP_SOCKET_TYPE_ID: Mutex<Option<TypeId>> = Mutex::new(None);
}

fn get_tcp_stream_type_id() -> TypeId {
    let mut id = TCP_STREAM_TYPE_ID.lock().expect("mutex lock should not fail");
    *id.get_or_insert_with(|| register_object_type("TcpStream", std::mem::size_of::<*mut StdTcpStream>()))
}

fn get_udp_socket_type_id() -> TypeId {
    let mut id = UDP_SOCKET_TYPE_ID.lock().expect("mutex lock should not fail");
    *id.get_or_insert_with(|| register_object_type("UdpSocket", std::mem::size_of::<*mut StdUdpSocket>()))
}

/// Convert a list of integers (bytes) to a Vec<u8>
fn bytes_from_list(list: &[Value]) -> Vec<u8> {
    list.iter()
        .filter_map(|v| match v {
            Value::Int(i) if *i >= 0 && *i <= 255 => Some(*i as u8),
            _ => None,
        })
        .collect()
}

/// Convert a Vec<u8> to a list of integers (bytes)
fn list_from_bytes(bytes: &[u8]) -> Vec<Value> {
    bytes.iter().map(|&b| Value::Int(b as i64)).collect()
}

// ============================================================================
// TcpStream Implementation
// ============================================================================

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_connect(addr: *const c_char) -> *mut MuxResult {
    if addr.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("address is null".to_string())));
    }

    let addr_str = unsafe { CStr::from_ptr(addr).to_string_lossy() };

    match StdTcpStream::connect(&*addr_str) {
        Ok(stream) => {
            let type_id = get_tcp_stream_type_id();
            let stream_ptr = Box::into_raw(Box::new(stream));
            let obj_ref = ObjectRef::new(stream_ptr as *mut std::ffi::c_void, type_id, std::mem::size_of::<*mut StdTcpStream>());
            let value = Value::Object(obj_ref);
            Box::into_raw(Box::new(MuxResult::ok(value)))
        }
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "Failed to connect to '{}': {}",
            addr_str, e
        )))),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_read(stream: *mut Value, size: i64) -> *mut MuxResult {
    if stream.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("stream is null".to_string())));
    }

    let size = if size < 0 { 0 } else { size as usize };

    unsafe {
        let value = &*stream;
        if let Value::Object(obj_ref) = value {
            let stream_ptr = obj_ref.ptr() as *mut StdTcpStream;
            if stream_ptr.is_null() {
                return Box::into_raw(Box::new(MuxResult::err("invalid stream".to_string())));
            }

            let mut buffer = vec![0u8; size];
            match (*stream_ptr).read(&mut buffer) {
                Ok(n) => {
                    buffer.truncate(n);
                    let bytes_list = list_from_bytes(&buffer);
                    Box::into_raw(Box::new(MuxResult::ok(Value::List(bytes_list))))
                }
                Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
                    "Read failed: {}",
                    e
                )))),
            }
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a TcpStream".to_string())))
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_write(stream: *mut Value, data: *mut Value) -> *mut MuxResult {
    if stream.is_null() || data.is_null() {
        return Box::into_raw(Box::new(MuxResult::err(
            "stream or data is null".to_string(),
        )));
    }

    unsafe {
        let stream_value = &*stream;
        let data_value = &*data;

        if let Value::Object(obj_ref) = stream_value {
            let stream_ptr = obj_ref.ptr() as *mut StdTcpStream;
            if stream_ptr.is_null() {
                return Box::into_raw(Box::new(MuxResult::err("invalid stream".to_string())));
            }

            let bytes = match data_value {
                Value::List(list) => bytes_from_list(list),
                _ => return Box::into_raw(Box::new(MuxResult::err(
                    "data must be a list of bytes".to_string(),
                ))),
            };

            match (*stream_ptr).write_all(&bytes) {
                Ok(_) => Box::into_raw(Box::new(MuxResult::ok(Value::Int(bytes.len() as i64)))),
                Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
                    "Write failed: {}",
                    e
                )))),
            }
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a TcpStream".to_string())))
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_close(stream: *mut Value) -> *mut MuxResult {
    if stream.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("stream is null".to_string())));
    }

    unsafe {
        let value = &*stream;
        if let Value::Object(obj_ref) = value {
            let stream_ptr = obj_ref.ptr() as *mut StdTcpStream;
            if !stream_ptr.is_null() {
                let _ = (*stream_ptr).shutdown(Shutdown::Both);
                drop(Box::from_raw(stream_ptr));
            }
            Box::into_raw(Box::new(MuxResult::ok(Value::Unit)))
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a TcpStream".to_string())))
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_set_nonblocking(stream: *mut Value, enabled: i32) -> *mut MuxResult {
    if stream.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("stream is null".to_string())));
    }

    unsafe {
        let value = &*stream;
        if let Value::Object(obj_ref) = value {
            let stream_ptr = obj_ref.ptr() as *mut StdTcpStream;
            if stream_ptr.is_null() {
                return Box::into_raw(Box::new(MuxResult::err("invalid stream".to_string())));
            }

            match (*stream_ptr).set_nonblocking(enabled != 0) {
                Ok(_) => Box::into_raw(Box::new(MuxResult::ok(Value::Unit))),
                Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
                    "Failed to set non-blocking mode: {}",
                    e
                )))),
            }
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a TcpStream".to_string())))
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_peer_addr(stream: *mut Value) -> *mut MuxResult {
    if stream.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("stream is null".to_string())));
    }

    unsafe {
        let value = &*stream;
        if let Value::Object(obj_ref) = value {
            let stream_ptr = obj_ref.ptr() as *mut StdTcpStream;
            if stream_ptr.is_null() {
                return Box::into_raw(Box::new(MuxResult::err("invalid stream".to_string())));
            }

            match (*stream_ptr).peer_addr() {
                Ok(addr) => Box::into_raw(Box::new(MuxResult::ok(Value::String(addr.to_string())))),
                Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
                    "Failed to get peer address: {}",
                    e
                )))),
            }
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a TcpStream".to_string())))
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_local_addr(stream: *mut Value) -> *mut MuxResult {
    if stream.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("stream is null".to_string())));
    }

    unsafe {
        let value = &*stream;
        if let Value::Object(obj_ref) = value {
            let stream_ptr = obj_ref.ptr() as *mut StdTcpStream;
            if stream_ptr.is_null() {
                return Box::into_raw(Box::new(MuxResult::err("invalid stream".to_string())));
            }

            match (*stream_ptr).local_addr() {
                Ok(addr) => Box::into_raw(Box::new(MuxResult::ok(Value::String(addr.to_string())))),
                Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
                    "Failed to get local address: {}",
                    e
                )))),
            }
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a TcpStream".to_string())))
        }
    }
}

// ============================================================================
// UdpSocket Implementation
// ============================================================================

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_bind(addr: *const c_char) -> *mut MuxResult {
    if addr.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("address is null".to_string())));
    }

    let addr_str = unsafe { CStr::from_ptr(addr).to_string_lossy() };

    match StdUdpSocket::bind(&*addr_str) {
        Ok(socket) => {
            let type_id = get_udp_socket_type_id();
            let socket_ptr = Box::into_raw(Box::new(socket));
            let obj_ref = ObjectRef::new(socket_ptr as *mut std::ffi::c_void, type_id, std::mem::size_of::<*mut StdUdpSocket>());
            let value = Value::Object(obj_ref);
            Box::into_raw(Box::new(MuxResult::ok(value)))
        }
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "Failed to bind to '{}': {}",
            addr_str, e
        )))),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_send_to(socket: *mut Value, data: *mut Value, addr: *const c_char) -> *mut MuxResult {
    if socket.is_null() || data.is_null() || addr.is_null() {
        return Box::into_raw(Box::new(MuxResult::err(
            "socket, data, or address is null".to_string(),
        )));
    }

    let addr_str = unsafe { CStr::from_ptr(addr).to_string_lossy() };

    unsafe {
        let socket_value = &*socket;
        let data_value = &*data;

        if let Value::Object(obj_ref) = socket_value {
            let socket_ptr = obj_ref.ptr() as *mut StdUdpSocket;
            if socket_ptr.is_null() {
                return Box::into_raw(Box::new(MuxResult::err("invalid socket".to_string())));
            }

            let bytes = match data_value {
                Value::List(list) => bytes_from_list(list),
                _ => return Box::into_raw(Box::new(MuxResult::err(
                    "data must be a list of bytes".to_string(),
                ))),
            };

            match (*socket_ptr).send_to(&bytes, &*addr_str) {
                Ok(n) => Box::into_raw(Box::new(MuxResult::ok(Value::Int(n as i64)))),
                Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
                    "Send failed: {}",
                    e
                )))),
            }
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a UdpSocket".to_string())))
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_recv_from(socket: *mut Value, size: i64) -> *mut MuxResult {
    if socket.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("socket is null".to_string())));
    }

    let size = if size < 0 { 0 } else { size as usize };

    unsafe {
        let value = &*socket;
        if let Value::Object(obj_ref) = value {
            let socket_ptr = obj_ref.ptr() as *mut StdUdpSocket;
            if socket_ptr.is_null() {
                return Box::into_raw(Box::new(MuxResult::err("invalid socket".to_string())));
            }

            let mut buffer = vec![0u8; size];
            match (*socket_ptr).recv_from(&mut buffer) {
                Ok((n, addr)) => {
                    buffer.truncate(n);
                    let bytes_list = list_from_bytes(&buffer);
                    let addr_str = addr.to_string();

                    // Create a tuple: (bytes, addr)
                    let tuple = crate::Tuple(
                        Value::List(bytes_list),
                        Value::String(addr_str),
                    );
                    Box::into_raw(Box::new(MuxResult::ok(Value::Tuple(Box::new(tuple)))))
                }
                Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
                    "Receive failed: {}",
                    e
                )))),
            }
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a UdpSocket".to_string())))
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_close(socket: *mut Value) -> *mut MuxResult {
    if socket.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("socket is null".to_string())));
    }

    unsafe {
        let value = &*socket;
        if let Value::Object(obj_ref) = value {
            let socket_ptr = obj_ref.ptr() as *mut StdUdpSocket;
            if !socket_ptr.is_null() {
                drop(Box::from_raw(socket_ptr));
            }
            Box::into_raw(Box::new(MuxResult::ok(Value::Unit)))
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a UdpSocket".to_string())))
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_set_nonblocking(socket: *mut Value, enabled: i32) -> *mut MuxResult {
    if socket.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("socket is null".to_string())));
    }

    unsafe {
        let value = &*socket;
        if let Value::Object(obj_ref) = value {
            let socket_ptr = obj_ref.ptr() as *mut StdUdpSocket;
            if socket_ptr.is_null() {
                return Box::into_raw(Box::new(MuxResult::err("invalid socket".to_string())));
            }

            match (*socket_ptr).set_nonblocking(enabled != 0) {
                Ok(_) => Box::into_raw(Box::new(MuxResult::ok(Value::Unit))),
                Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
                    "Failed to set non-blocking mode: {}",
                    e
                )))),
            }
        } else {
            Box::into_raw(Box::new(MuxResult::err("not a UdpSocket".to_string())))
        }
    }
}

// ============================================================================
// HTTP Request/Response Types
// ============================================================================

pub struct Request {
    pub method: String,
    pub url: String,
    pub headers: BTreeMap<String, String>,
    pub body: Vec<u8>,
}

pub struct Response {
    pub status: i64,
    pub headers: BTreeMap<String, String>,
    pub body: Vec<u8>,
}

/// Convert a Mux Map<Value, Value> to a Rust BTreeMap<String, String>
fn map_from_mux_value(map: &BTreeMap<Value, Value>) -> BTreeMap<String, String> {
    let mut result = BTreeMap::new();
    for (k, v) in map {
        if let (Value::String(key), Value::String(val)) = (k, v) {
            result.insert(key.clone(), val.clone());
        }
    }
    result
}

/// Convert a Rust BTreeMap<String, String> to a Mux Map<Value, Value>
fn map_to_mux_value(map: &BTreeMap<String, String>) -> BTreeMap<Value, Value> {
    let mut result = BTreeMap::new();
    for (k, v) in map {
        result.insert(Value::String(k.clone()), Value::String(v.clone()));
    }
    result
}

// ============================================================================
// HTTP Functions (Basic GET/POST support)
// ============================================================================

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_http_request(
    method: *const c_char,
    url: *const c_char,
    headers: *mut Value,
    body: *mut Value,
) -> *mut MuxResult {
    if method.is_null() || url.is_null() {
        return Box::into_raw(Box::new(MuxResult::err(
            "method or url is null".to_string(),
        )));
    }

    let method_str = unsafe { CStr::from_ptr(method).to_string_lossy() };
    let url_str = unsafe { CStr::from_ptr(url).to_string_lossy() };

    let headers_map = if headers.is_null() {
        BTreeMap::new()
    } else {
        unsafe {
            match &*headers {
                Value::Map(m) => map_from_mux_value(m),
                _ => BTreeMap::new(),
            }
        }
    };

    let body_bytes = if body.is_null() {
        Vec::new()
    } else {
        unsafe {
            match &*body {
                Value::List(list) => bytes_from_list(list),
                _ => Vec::new(),
            }
        }
    };

    // Perform HTTP request using TCP
    match perform_http_request(&method_str, &url_str, headers_map, body_bytes) {
        Ok(response) => {
            let headers_value = Value::Map(map_to_mux_value(&response.headers));
            let body_value = Value::List(list_from_bytes(&response.body));

            let tuple = crate::Tuple(
                Value::Int(response.status),
                Value::Tuple(Box::new(crate::Tuple(headers_value, body_value))),
            );
            Box::into_raw(Box::new(MuxResult::ok(Value::Tuple(Box::new(tuple)))))
        }
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "HTTP request failed: {}",
            e
        )))),
    }
}

fn perform_http_request(
    method: &str,
    url: &str,
    headers: BTreeMap<String, String>,
    body: Vec<u8>,
) -> Result<Response, String> {
    // Parse URL to extract host, port, and path
    let (host, port, path, is_https) = parse_url(url)?;

    if is_https {
        return Err("HTTPS not yet supported. Use HTTP or implement TLS support.".to_string());
    }

    let addr = format!("{}:{}", host, port);

    // Connect to server
    let mut stream = StdTcpStream::connect(&addr)
        .map_err(|e| format!("Failed to connect to {}: {}", addr, e))?;

    // Build HTTP request
    let request = build_http_request(method, &host, &path, &headers, &body);

    // Send request
    stream
        .write_all(request.as_bytes())
        .map_err(|e| format!("Failed to send request: {}", e))?;

    // Read response
    let mut response_data = Vec::new();
    let mut buffer = [0u8; 4096];

    loop {
        match stream.read(&mut buffer) {
            Ok(0) => break,
            Ok(n) => response_data.extend_from_slice(&buffer[..n]),
            Err(e) => return Err(format!("Failed to read response: {}", e)),
        }
    }

    // Parse response
    parse_http_response(&response_data)
}

fn parse_url(url: &str) -> Result<(String, u16, String, bool), String> {
    if url.starts_with("http://") {
        let rest = &url[7..];
        let (host_port, path) = rest.split_once('/').map(|(hp, p)| (hp, format!("/{}", p))).unwrap_or((rest, "/".to_string()));

        let (host, port) = if let Some((h, p)) = host_port.split_once(':') {
            (h.to_string(), p.parse::<u16>().map_err(|e| format!("Invalid port: {}", e))?)
        } else {
            (host_port.to_string(), 80u16)
        };

        Ok((host, port, path, false))
    } else if url.starts_with("https://") {
        let rest = &url[8..];
        let (host_port, path) = rest.split_once('/').map(|(hp, p)| (hp, format!("/{}", p))).unwrap_or((rest, "/".to_string()));

        let (host, port) = if let Some((h, p)) = host_port.split_once(':') {
            (h.to_string(), p.parse::<u16>().map_err(|e| format!("Invalid port: {}", e))?)
        } else {
            (host_port.to_string(), 443u16)
        };

        Ok((host, port, path, true))
    } else {
        Err("URL must start with http:// or https://".to_string())
    }
}

fn build_http_request(
    method: &str,
    host: &str,
    path: &str,
    headers: &BTreeMap<String, String>,
    body: &[u8],
) -> String {
    let mut request = format!("{} {} HTTP/1.1\r\n", method.to_uppercase(), path);
    request.push_str(&format!("Host: {}\r\n", host));
    request.push_str(&format!("Content-Length: {}\r\n", body.len()));

    for (key, value) in headers {
        request.push_str(&format!("{}: {}\r\n", key, value));
    }

    request.push_str("Connection: close\r\n");
    request.push_str("\r\n");

    // Append body as bytes after headers
    let mut request_bytes = request.into_bytes();
    request_bytes.extend_from_slice(body);

    // Convert back to string (may not be valid UTF-8 if body is binary, but for simple HTTP it should work)
    String::from_utf8_lossy(&request_bytes).to_string()
}

fn parse_http_response(data: &[u8]) -> Result<Response, String> {
    let response_str = String::from_utf8_lossy(data);

    // Parse status line
    let mut lines = response_str.lines();
    let status_line = lines.next().ok_or("Empty response")?;

    let parts: Vec<&str> = status_line.split_whitespace().collect();
    if parts.len() < 2 {
        return Err("Invalid HTTP response".to_string());
    }

    let status: i64 = parts[1]
        .parse()
        .map_err(|_| "Invalid status code".to_string())?;

    // Parse headers
    let mut headers = BTreeMap::new();
    let mut header_end = false;
    let mut body_start = 0;

    for (i, line) in response_str.lines().enumerate() {
        if line.is_empty() {
            header_end = true;
            // Find position in original data where body starts
            let header_part = &response_str[..response_str.find(line).unwrap_or(0)];
            body_start = header_part.len() + if header_part.ends_with('\n') { 2 } else { 1 };
            break;
        }

        if i == 0 {
            continue; // Skip status line
        }

        if let Some((key, value)) = line.split_once(':') {
            headers.insert(key.trim().to_string(), value.trim().to_string());
        }
    }

    // Extract body
    let body = if header_end && body_start < data.len() {
        data[body_start..].to_vec()
    } else {
        Vec::new()
    };

    Ok(Response {
        status,
        headers,
        body,
    })
}

// ============================================================================
// Convenience Functions
// ============================================================================

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_http_get(url: *const c_char) -> *mut MuxResult {
    if url.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("url is null".to_string())));
    }

    let url_str = unsafe { CStr::from_ptr(url).to_string_lossy() };
    let empty_headers: BTreeMap<String, String> = BTreeMap::new();

    match perform_http_request("GET", &url_str, empty_headers, Vec::new()) {
        Ok(response) => {
            let headers_value = Value::Map(map_to_mux_value(&response.headers));
            let body_value = Value::List(list_from_bytes(&response.body));

            let tuple = crate::Tuple(
                Value::Int(response.status),
                Value::Tuple(Box::new(crate::Tuple(headers_value, body_value))),
            );
            Box::into_raw(Box::new(MuxResult::ok(Value::Tuple(Box::new(tuple)))))
        }
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "HTTP GET failed: {}",
            e
        )))),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_http_post(url: *const c_char, body: *mut Value) -> *mut MuxResult {
    if url.is_null() {
        return Box::into_raw(Box::new(MuxResult::err("url is null".to_string())));
    }

    let url_str = unsafe { CStr::from_ptr(url).to_string_lossy() };
    let empty_headers: BTreeMap<String, String> = BTreeMap::new();

    let body_bytes = if body.is_null() {
        Vec::new()
    } else {
        unsafe {
            match &*body {
                Value::List(list) => bytes_from_list(list),
                _ => Vec::new(),
            }
        }
    };

    match perform_http_request("POST", &url_str, empty_headers, body_bytes) {
        Ok(response) => {
            let headers_value = Value::Map(map_to_mux_value(&response.headers));
            let body_value = Value::List(list_from_bytes(&response.body));

            let tuple = crate::Tuple(
                Value::Int(response.status),
                Value::Tuple(Box::new(crate::Tuple(headers_value, body_value))),
            );
            Box::into_raw(Box::new(MuxResult::ok(Value::Tuple(Box::new(tuple)))))
        }
        Err(e) => Box::into_raw(Box::new(MuxResult::err(format!(
            "HTTP POST failed: {}",
            e
        )))),
    }
}
