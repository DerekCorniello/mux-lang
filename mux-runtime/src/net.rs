use crate::json::{json_to_value, value_to_json, Json};
use crate::object::{alloc_object, get_object_ptr, register_object_type};
use crate::refcount::mux_rc_dec;
use crate::result::MuxResult;
use crate::{Tuple, TypeId, Value};
use lazy_static::lazy_static;
use std::collections::{BTreeMap, HashMap};
use std::ffi::c_void;
use std::io::{Read, Write};
use std::net::{
    TcpListener as StdTcpListener, TcpStream as StdTcpStream, UdpSocket as StdUdpSocket,
};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex};

type SocketMap<T> = Mutex<HashMap<i64, Arc<Mutex<T>>>>;

static NEXT_HANDLE: AtomicI64 = AtomicI64::new(1);

lazy_static! {
    static ref TCP_STREAMS: SocketMap<StdTcpStream> = Mutex::new(HashMap::new());
    static ref TCP_LISTENERS: SocketMap<StdTcpListener> = Mutex::new(HashMap::new());
    static ref UDP_SOCKETS: SocketMap<StdUdpSocket> = Mutex::new(HashMap::new());
}

lazy_static! {
    static ref TCP_STREAM_TYPE_ID: TypeId = register_object_type(
        "TcpStream",
        std::mem::size_of::<i64>(),
        Some(|ptr| drop_socket_handle(&TCP_STREAMS, ptr)),
    );
    static ref TCP_LISTENER_TYPE_ID: TypeId = register_object_type(
        "TcpListener",
        std::mem::size_of::<i64>(),
        Some(|ptr| drop_socket_handle(&TCP_LISTENERS, ptr)),
    );
    static ref UDP_SOCKET_TYPE_ID: TypeId = register_object_type(
        "UdpSocket",
        std::mem::size_of::<i64>(),
        Some(|ptr| drop_socket_handle(&UDP_SOCKETS, ptr)),
    );
}

fn lock_map<'a, T>(
    map: &'a SocketMap<T>,
) -> std::sync::MutexGuard<'a, HashMap<i64, Arc<Mutex<T>>>> {
    map.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn next_handle() -> i64 {
    loop {
        let handle = NEXT_HANDLE.fetch_add(1, Ordering::SeqCst);
        if handle > 0 {
            return handle;
        }
        // Overflow occurred, atomically reset counter to 1
        let _ = NEXT_HANDLE.compare_exchange(handle, 1, Ordering::SeqCst, Ordering::SeqCst);
    }
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
    get_socket_entry(map, handle).ok_or_else(|| format!("invalid {} handle", label))
}

fn with_socket<T, R, F>(map: &SocketMap<T>, handle: i64, label: &str, op: F) -> Result<R, String>
where
    F: FnOnce(&mut T) -> Result<R, String>,
{
    let entry = socket_entry_or_err(map, handle, label)?;
    let mut guard = entry
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
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

fn with_tcp_listener<R, F>(handle: i64, op: F) -> Result<R, String>
where
    F: FnOnce(&mut StdTcpListener) -> Result<R, String>,
{
    with_socket(&TCP_LISTENERS, handle, "tcp listener", op)
}

fn store_tcp_stream(stream: StdTcpStream) -> i64 {
    insert_socket(&TCP_STREAMS, stream)
}

fn store_tcp_listener(listener: StdTcpListener) -> i64 {
    insert_socket(&TCP_LISTENERS, listener)
}

fn store_udp_socket(socket: StdUdpSocket) -> i64 {
    insert_socket(&UDP_SOCKETS, socket)
}

fn remove_tcp_stream(handle: i64) {
    remove_socket(&TCP_STREAMS, handle)
}

fn remove_tcp_listener(handle: i64) {
    remove_socket(&TCP_LISTENERS, handle)
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

fn tcp_listener_handle(value: *const Value) -> Result<i64, String> {
    require_handle(value, "tcp listener")
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
    mux_rc_dec(obj_ptr);
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

fn value_to_json_map(value: *const Value, label: &str) -> Result<BTreeMap<String, Json>, String> {
    if value.is_null() {
        return Err(format!("{} is null", label));
    }
    let json = value_to_json(unsafe { &*value })?;
    if let Json::Object(map) = json {
        Ok(map)
    } else {
        Err(format!("{} must be a JSON object", label))
    }
}

fn json_get_string(
    map: &BTreeMap<String, Json>,
    key: &str,
    required: bool,
) -> Result<Option<String>, String> {
    match map.get(key) {
        Some(Json::String(value)) => Ok(Some(value.clone())),
        Some(_) => Err(format!("'{}' must be a string", key)),
        None if required => Err(format!("missing required field '{}'", key)),
        None => Ok(None),
    }
}

fn json_headers(
    map: &BTreeMap<String, Json>,
    key: &str,
) -> Result<BTreeMap<String, String>, String> {
    let Some(headers_value) = map.get(key) else {
        return Ok(BTreeMap::new());
    };
    let Json::Object(headers) = headers_value else {
        return Err(format!("'{}' must be a JSON object", key));
    };

    let mut resolved = BTreeMap::new();
    for (name, value) in headers {
        if let Json::String(header_value) = value {
            resolved.insert(name.clone(), header_value.clone());
        } else {
            return Err(format!("header '{}' must be a string", name));
        }
    }
    Ok(resolved)
}

fn read_http_response(response: ureq::Response) -> Result<Value, String> {
    let status = i64::from(response.status());
    let mut response_headers = BTreeMap::new();
    for name in response.headers_names() {
        // `ureq::Response::header` returns only the first value for a header name.
        // HTTP allows multiple values for the same header (e.g. Set-Cookie). Join
        // multiple values with ", " per RFC 7230 §3.2.2 so callers receive all
        // header occurrences.
        let values = response.all(&name);
        if !values.is_empty() {
            let joined = values.join(", ");
            response_headers.insert(name, Json::String(joined));
        }
    }

    let body_text = response
        .into_string()
        .map_err(|e| format!("failed to read response body: {}", e))?;
    let body_json = if body_text.trim().is_empty() {
        Json::Null
    } else {
        Json::parse(&body_text).unwrap_or(Json::String(body_text))
    };

    let mut response_map = BTreeMap::new();
    response_map.insert("status".to_string(), Json::Number(status as f64));
    response_map.insert("headers".to_string(), Json::Object(response_headers));
    response_map.insert("body".to_string(), body_json);
    Ok(json_to_value(&Json::Object(response_map)))
}

const MAX_HTTP_HEADER_BYTES: usize = 64 * 1024;
const MAX_HTTP_BODY_BYTES: usize = 1024 * 1024;
const MAX_HTTP_HEADERS_COUNT: usize = 128;

fn find_double_crlf(buf: &[u8]) -> Option<usize> {
    buf.windows(4).position(|window| window == b"\r\n\r\n")
}

fn reason_phrase(status: u16) -> &'static str {
    match status {
        100 => "Continue",
        101 => "Switching Protocols",
        102 => "Processing",
        103 => "Early Hints",
        200 => "OK",
        201 => "Created",
        202 => "Accepted",
        203 => "Non-Authoritative Information",
        204 => "No Content",
        205 => "Reset Content",
        206 => "Partial Content",
        207 => "Multi-Status",
        208 => "Already Reported",
        226 => "IM Used",
        300 => "Multiple Choices",
        301 => "Moved Permanently",
        302 => "Found",
        303 => "See Other",
        304 => "Not Modified",
        305 => "Use Proxy",
        307 => "Temporary Redirect",
        308 => "Permanent Redirect",
        400 => "Bad Request",
        401 => "Unauthorized",
        402 => "Payment Required",
        403 => "Forbidden",
        404 => "Not Found",
        405 => "Method Not Allowed",
        406 => "Not Acceptable",
        407 => "Proxy Authentication Required",
        408 => "Request Timeout",
        409 => "Conflict",
        410 => "Gone",
        411 => "Length Required",
        412 => "Precondition Failed",
        413 => "Payload Too Large",
        414 => "URI Too Long",
        415 => "Unsupported Media Type",
        416 => "Range Not Satisfiable",
        417 => "Expectation Failed",
        418 => "I'm a teapot",
        421 => "Misdirected Request",
        422 => "Unprocessable Entity",
        423 => "Locked",
        424 => "Failed Dependency",
        425 => "Too Early",
        426 => "Upgrade Required",
        428 => "Precondition Required",
        429 => "Too Many Requests",
        431 => "Request Header Fields Too Large",
        451 => "Unavailable For Legal Reasons",
        500 => "Internal Server Error",
        501 => "Not Implemented",
        502 => "Bad Gateway",
        503 => "Service Unavailable",
        504 => "Gateway Timeout",
        505 => "HTTP Version Not Supported",
        506 => "Variant Also Negotiates",
        507 => "Insufficient Storage",
        508 => "Loop Detected",
        510 => "Not Extended",
        511 => "Network Authentication Required",
        _ => "Unknown",
    }
}

fn json_get_optional_object(
    map: &BTreeMap<String, Json>,
    key: &str,
) -> Result<Option<BTreeMap<String, Json>>, String> {
    match map.get(key) {
        Some(Json::Object(obj)) => Ok(Some(obj.clone())),
        Some(_) => Err(format!("'{}' must be an object", key)),
        None => Ok(None),
    }
}

fn json_get_required_int(map: &BTreeMap<String, Json>, key: &str) -> Result<i64, String> {
    match map.get(key) {
        Some(Json::Number(value)) => {
            if !value.is_finite() {
                return Err(format!("'{}' must be a finite number", key));
            }
            let rounded = value.round();
            if value.fract() != 0.0 {
                return Err(format!("'{}' must be an integer", key));
            }
            Ok(rounded as i64)
        }
        Some(_) => Err(format!("'{}' must be a number", key)),
        None => Err(format!("missing required field '{}'", key)),
    }
}

fn header_content_length(headers: &BTreeMap<String, String>) -> Result<Option<usize>, String> {
    let Some(raw_value) = headers
        .iter()
        .find_map(|(k, v)| k.eq_ignore_ascii_case("content-length").then_some(v))
    else {
        return Ok(None);
    };

    let len = raw_value
        .trim()
        .parse::<usize>()
        .map_err(|_| "invalid Content-Length header".to_string())?;
    if len > MAX_HTTP_BODY_BYTES {
        return Err("request body too large".to_string());
    }
    Ok(Some(len))
}

fn read_http_request_headers(stream: &mut StdTcpStream) -> Result<(Vec<u8>, usize), String> {
    let mut buffer = Vec::new();
    let mut chunk = [0u8; 1024];
    loop {
        let count = stream
            .read(&mut chunk)
            .map_err(|e| format!("http read failed: {}", e))?;
        if count == 0 {
            return Err("connection closed before request headers".to_string());
        }
        buffer.extend_from_slice(&chunk[..count]);
        if buffer.len() > MAX_HTTP_HEADER_BYTES {
            return Err("http headers too large".to_string());
        }
        if let Some(pos) = find_double_crlf(&buffer) {
            return Ok((buffer, pos + 4));
        }
    }
}

fn parse_http_request_headers(
    header_slice: &[u8],
) -> Result<(String, String, String, BTreeMap<String, String>), String> {
    let mut parsed_headers = vec![httparse::EMPTY_HEADER; MAX_HTTP_HEADERS_COUNT];
    let mut request = httparse::Request::new(&mut parsed_headers);
    let parse_status = request
        .parse(header_slice)
        .map_err(|e| format!("invalid http request: {}", e))?;
    if parse_status.is_partial() {
        return Err("incomplete http request headers".to_string());
    }

    let method = request
        .method
        .ok_or_else(|| "http request missing method".to_string())?
        .to_string();
    let raw_target = request
        .path
        .ok_or_else(|| "http request missing target".to_string())?
        .to_string();
    let version = match request.version {
        Some(0) => "HTTP/1.0".to_string(),
        Some(1) => "HTTP/1.1".to_string(),
        Some(v) => return Err(format!("unsupported http version {}", v)),
        None => return Err("http request missing version".to_string()),
    };

    let mut headers = BTreeMap::new();
    for header in request.headers.iter() {
        let value = std::str::from_utf8(header.value)
            .map_err(|_| format!("header '{}' contains invalid utf-8", header.name))?;
        let header_name = header.name.to_string();
        let header_value = value.trim();
        headers
            .entry(header_name)
            .and_modify(|existing: &mut String| {
                existing.push_str(", ");
                existing.push_str(header_value);
            })
            .or_insert_with(|| header_value.to_string());
    }
    Ok((method, raw_target, version, headers))
}

fn read_http_request_body(
    stream: &mut StdTcpStream,
    initial_body: &[u8],
    content_length: usize,
) -> Result<Vec<u8>, String> {
    let mut body = initial_body.to_vec();
    let mut chunk = [0u8; 1024];
    while body.len() < content_length {
        let count = stream
            .read(&mut chunk)
            .map_err(|e| format!("http read failed: {}", e))?;
        if count == 0 {
            return Err("connection closed before request body complete".to_string());
        }
        body.extend_from_slice(&chunk[..count]);
        if body.len() > content_length {
            body.truncate(content_length);
            break;
        }
    }
    body.truncate(content_length);
    Ok(body)
}

fn decode_http_request_body(body: &[u8]) -> Result<Json, String> {
    if body.is_empty() {
        Ok(Json::Null)
    } else {
        let body_text =
            std::str::from_utf8(body).map_err(|_| "request body is not utf-8".to_string())?;
        Ok(Json::parse(body_text).unwrap_or(Json::String(body_text.to_string())))
    }
}

fn build_http_request_json(
    method: String,
    raw_target: String,
    version: String,
    headers: BTreeMap<String, String>,
    body_json: Json,
) -> Json {
    let (path, query) = if let Some((p, q)) = raw_target.split_once('?') {
        (p.to_string(), q.to_string())
    } else {
        (raw_target, String::new())
    };
    let mut req_map = BTreeMap::new();
    req_map.insert("method".to_string(), Json::String(method));
    req_map.insert("path".to_string(), Json::String(path));
    req_map.insert("query".to_string(), Json::String(query));
    req_map.insert("version".to_string(), Json::String(version));
    req_map.insert(
        "headers".to_string(),
        Json::Object(
            headers
                .into_iter()
                .map(|(k, v)| (k, Json::String(v)))
                .collect(),
        ),
    );
    req_map.insert("body".to_string(), body_json);
    Json::Object(req_map)
}

fn read_http_request(stream: &mut StdTcpStream) -> Result<Json, String> {
    let (buffer, header_end) = read_http_request_headers(stream)?;
    let (method, raw_target, version, headers) = parse_http_request_headers(&buffer[..header_end])?;
    let initial_body = &buffer[header_end..];
    let content_length = match header_content_length(&headers)? {
        Some(len) => len,
        None => {
            if initial_body.is_empty() {
                0
            } else {
                return Err("request body present without Content-Length header".to_string());
            }
        }
    };
    let body = read_http_request_body(stream, initial_body, content_length)?;
    let body_json = decode_http_request_body(&body)?;
    Ok(build_http_request_json(
        method, raw_target, version, headers, body_json,
    ))
}

fn write_http_response(stream: &mut StdTcpStream, response: &Json) -> Result<(), String> {
    let Json::Object(response_map) = response else {
        return Err("response must be a JSON object".to_string());
    };
    let status_value = json_get_required_int(response_map, "status")?;
    let status_code =
        u16::try_from(status_value).map_err(|_| "status must fit in u16 range".to_string())?;
    if !(100..=999).contains(&status_code) {
        return Err("status must be between 100 and 999".to_string());
    }

    let header_json = json_get_optional_object(response_map, "headers")?;
    let mut headers = BTreeMap::new();
    if let Some(header_map) = header_json {
        for (k, v) in header_map {
            if let Json::String(value) = v {
                headers.insert(k, value);
            } else {
                return Err(format!("response header '{}' must be a string", k));
            }
        }
    }

    let body_json = response_map.get("body").cloned().unwrap_or(Json::Null);

    if !headers
        .keys()
        .any(|name| name.eq_ignore_ascii_case("content-type"))
    {
        headers.insert("Content-Type".to_string(), "application/json".to_string());
    }
    let is_json_content_type = headers
        .iter()
        .find_map(|(name, value)| {
            name.eq_ignore_ascii_case("content-type")
                .then_some(value.as_str())
        })
        .and_then(|value| value.split(';').next())
        .is_some_and(|value| value.trim().eq_ignore_ascii_case("application/json"));
    let body_bytes = if !is_json_content_type {
        match &body_json {
            Json::String(text) => text.as_bytes().to_vec(),
            _ => body_json.stringify(None).into_bytes(),
        }
    } else {
        body_json.stringify(None).into_bytes()
    };
    headers.insert("Content-Length".to_string(), body_bytes.len().to_string());
    if !headers
        .keys()
        .any(|name| name.eq_ignore_ascii_case("connection"))
    {
        headers.insert("Connection".to_string(), "close".to_string());
    }

    let mut message = format!(
        "HTTP/1.1 {} {}\r\n",
        status_code,
        reason_phrase(status_code)
    );
    for (name, value) in headers {
        message.push_str(&name);
        message.push_str(": ");
        message.push_str(&value);
        message.push_str("\r\n");
    }
    message.push_str("\r\n");

    stream
        .write_all(message.as_bytes())
        .map_err(|e| format!("http write failed: {}", e))?;
    stream
        .write_all(&body_bytes)
        .map_err(|e| format!("http write failed: {}", e))?;
    stream
        .flush()
        .map_err(|e| format!("http flush failed: {}", e))?;
    Ok(())
}

fn execute_http_request(request: *const Value) -> Result<Value, String> {
    let request_map = value_to_json_map(request, "request")?;
    let method = json_get_string(&request_map, "method", true)?
        .ok_or_else(|| "missing required field 'method'".to_string())?;
    let url = json_get_string(&request_map, "url", true)?
        .ok_or_else(|| "missing required field 'url'".to_string())?;
    let headers = json_headers(&request_map, "headers")?;
    let body = request_map.get("body").cloned();

    let mut has_content_type = false;
    let mut req = ureq::request(&method, &url);
    for (header_name, header_value) in &headers {
        if header_name.eq_ignore_ascii_case("content-type") {
            has_content_type = true;
        }
        req = req.set(header_name, header_value);
    }

    let response = if let Some(body_json) = body {
        if !has_content_type {
            req = req.set("Content-Type", "application/json");
        }
        let payload = body_json.stringify(None);
        match req.send_string(&payload) {
            Ok(response) => response,
            Err(ureq::Error::Status(_, response)) => response,
            Err(ureq::Error::Transport(error)) => {
                return Err(format!("http request failed: {}", error));
            }
        }
    } else {
        match req.call() {
            Ok(response) => response,
            Err(ureq::Error::Status(_, response)) => response,
            Err(ureq::Error::Transport(error)) => {
                return Err(format!("http request failed: {}", error));
            }
        }
    };

    read_http_response(response)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_http_request(request: *const Value) -> *mut MuxResult {
    match execute_http_request(request) {
        Ok(value) => net_result_ok(value),
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_listener_bind(addr: *mut Value) -> *mut MuxResult {
    match value_to_string(addr).and_then(|address| {
        StdTcpListener::bind(address).map_err(|e| format!("tcp listener bind failed: {}", e))
    }) {
        Ok(listener) => {
            let handle = store_tcp_listener(listener);
            net_result_ok(create_socket_value(handle, *TCP_LISTENER_TYPE_ID))
        }
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_listener_accept(listener: *mut Value) -> *mut MuxResult {
    let handle = match tcp_listener_handle(listener) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    match with_tcp_listener(handle, |socket| {
        socket
            .accept()
            .map(|(stream, _)| stream)
            .map_err(|e| format!("tcp listener accept failed: {}", e))
    }) {
        Ok(stream) => {
            let stream_handle = store_tcp_stream(stream);
            net_result_ok(create_socket_value(stream_handle, *TCP_STREAM_TYPE_ID))
        }
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_listener_set_nonblocking(
    listener: *mut Value,
    enabled: i32,
) -> *mut MuxResult {
    let handle = match tcp_listener_handle(listener) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    net_result_unit(with_tcp_listener(handle, |socket| {
        socket
            .set_nonblocking(enabled != 0)
            .map_err(|e| format!("tcp listener set_nonblocking failed: {}", e))
    }))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_listener_local_addr(listener: *mut Value) -> *mut MuxResult {
    let result = tcp_listener_handle(listener).and_then(|handle| {
        with_tcp_listener(handle, |socket| {
            socket
                .local_addr()
                .map(|addr| addr.to_string())
                .map_err(|e| format!("tcp listener local_addr failed: {}", e))
        })
    });
    net_result_string(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_listener_close(listener: *mut Value) {
    if let Ok(handle) = tcp_listener_handle(listener) {
        remove_tcp_listener(handle);
        write_handle(listener, 0);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_http_read_request(stream: *mut Value) -> *mut MuxResult {
    let handle = match tcp_handle(stream) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    match with_tcp_stream(handle, |socket| {
        read_http_request(socket).map(|v| json_to_value(&v))
    }) {
        Ok(value) => net_result_ok(value),
        Err(err) => net_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_http_write_response(
    stream: *mut Value,
    response: *mut Value,
) -> *mut MuxResult {
    let handle = match tcp_handle(stream) {
        Ok(handle) => handle,
        Err(err) => return net_result_err(err),
    };
    let response_json = if response.is_null() {
        return net_result_err("response is null".to_string());
    } else {
        match value_to_json(unsafe { &*response }) {
            Ok(v) => v,
            Err(err) => return net_result_err(err),
        }
    };
    let result = with_tcp_stream(handle, |socket| write_http_response(socket, &response_json));
    net_result_unit(result)
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
        return net_result_err("invalid buffer size".to_string());
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
    let result = tcp_handle(stream).and_then(|handle| {
        with_tcp_stream(handle, |socket| {
            socket
                .peer_addr()
                .map(|addr| addr.to_string())
                .map_err(|e| format!("tcp peer_addr failed: {}", e))
        })
    });
    net_result_string(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_tcp_local_addr(stream: *mut Value) -> *mut MuxResult {
    let result = tcp_handle(stream).and_then(|handle| {
        with_tcp_stream(handle, |socket| {
            socket
                .local_addr()
                .map(|addr| addr.to_string())
                .map_err(|e| format!("tcp local_addr failed: {}", e))
        })
    });
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
        let result = sock
            .recv_from(&mut buf)
            .map_err(|e| format!("udp recv failed: {}", e))?;
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
    let result = udp_handle(socket).and_then(|handle| {
        with_udp_socket(handle, |sock| {
            sock.peer_addr()
                .map(|addr| addr.to_string())
                .map_err(|e| format!("udp peer_addr failed: {}", e))
        })
    });
    net_result_string(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_net_udp_local_addr(socket: *mut Value) -> *mut MuxResult {
    let result = udp_handle(socket).and_then(|handle| {
        with_udp_socket(handle, |sock| {
            sock.local_addr()
                .map(|addr| addr.to_string())
                .map_err(|e| format!("udp local_addr failed: {}", e))
        })
    });
    net_result_string(result)
}
