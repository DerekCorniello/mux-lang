use crate::object::{alloc_object, get_object_ptr, register_object_type};
use crate::refcount::mux_rc_dec;
use crate::result::MuxResult;
use crate::{TypeId, Value};
use lazy_static::lazy_static;
use rusqlite::types::{Value as SqliteValue, ValueRef};
use rusqlite::{params_from_iter, Connection as SqliteConnection};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::ffi::{c_char, c_void, CStr};
use std::sync::atomic::{AtomicI64, Ordering};

static NEXT_HANDLE: AtomicI64 = AtomicI64::new(1);

thread_local! {
    static SQL_CONNECTIONS: RefCell<HashMap<i64, SqlConnection>> = RefCell::new(HashMap::new());
    static SQL_TRANSACTIONS: RefCell<HashMap<i64, SqlTransaction>> = RefCell::new(HashMap::new());
    static SQL_RESULTSETS: RefCell<HashMap<i64, SqlResultSet>> = RefCell::new(HashMap::new());
}

lazy_static! {
    static ref SQL_CONNECTION_TYPE_ID: TypeId = register_object_type(
        "Connection",
        std::mem::size_of::<i64>(),
        Some(drop_connection_handle),
    );
    static ref SQL_TRANSACTION_TYPE_ID: TypeId = register_object_type(
        "Transaction",
        std::mem::size_of::<i64>(),
        Some(drop_transaction_handle),
    );
    static ref SQL_RESULTSET_TYPE_ID: TypeId = register_object_type(
        "ResultSet",
        std::mem::size_of::<i64>(),
        Some(drop_resultset_handle),
    );
}

enum SqlConnection {
    Sqlite(SqliteConnection),
}

struct SqlTransaction {
    connection_handle: i64,
    active: bool,
}

struct SqlResultSet {
    rows: Vec<Value>,
    columns: Vec<String>,
    next_index: usize,
}

fn next_handle() -> i64 {
    loop {
        let handle = NEXT_HANDLE.fetch_add(1, Ordering::SeqCst);
        if handle > 0 {
            return handle;
        }
        let _ = NEXT_HANDLE.compare_exchange(handle, 1, Ordering::SeqCst, Ordering::SeqCst);
    }
}

fn sql_result_ok(value: Value) -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::ok(value)))
}

fn sql_result_err(msg: String) -> *mut MuxResult {
    Box::into_raw(Box::new(MuxResult::err(Value::String(msg))))
}

fn sql_result_unit(result: Result<(), String>) -> *mut MuxResult {
    match result {
        Ok(()) => sql_result_ok(Value::Unit),
        Err(err) => sql_result_err(err),
    }
}

fn sql_result_i64(result: Result<i64, String>) -> *mut MuxResult {
    match result {
        Ok(value) => sql_result_ok(Value::Int(value)),
        Err(err) => sql_result_err(err),
    }
}

fn create_handle_value(handle: i64, type_id: TypeId) -> Value {
    let obj_ptr = alloc_object(type_id);
    let data_ptr = unsafe { get_object_ptr(obj_ptr) };
    if !data_ptr.is_null() {
        unsafe { *(data_ptr as *mut i64) = handle };
    }
    let value = unsafe { (*obj_ptr).clone() };
    mux_rc_dec(obj_ptr);
    value
}

fn object_handle(value: *const Value) -> Option<i64> {
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
    object_handle(value)
        .filter(|&handle| handle != 0)
        .ok_or_else(|| format!("invalid {}", label))
}

fn connection_handle(value: *const Value) -> Result<i64, String> {
    require_handle(value, "sql connection")
}

fn transaction_handle(value: *const Value) -> Result<i64, String> {
    require_handle(value, "sql transaction")
}

fn resultset_handle(value: *const Value) -> Result<i64, String> {
    require_handle(value, "sql result set")
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

fn remove_connection(handle: i64) {
    SQL_CONNECTIONS.with(|connections| {
        connections.borrow_mut().remove(&handle);
    });
}

fn remove_transaction(handle: i64) {
    SQL_TRANSACTIONS.with(|transactions| {
        transactions.borrow_mut().remove(&handle);
    });
}

fn remove_resultset(handle: i64) {
    SQL_RESULTSETS.with(|resultsets| {
        resultsets.borrow_mut().remove(&handle);
    });
}

fn drop_connection_handle(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let handle = unsafe { *(ptr as *mut i64) };
    if handle != 0 {
        remove_connection(handle);
    }
}

fn drop_transaction_handle(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let handle = unsafe { *(ptr as *mut i64) };
    if handle != 0 {
        remove_transaction(handle);
    }
}

fn drop_resultset_handle(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    let handle = unsafe { *(ptr as *mut i64) };
    if handle != 0 {
        remove_resultset(handle);
    }
}

fn store_connection(connection: SqlConnection) -> i64 {
    let handle = next_handle();
    SQL_CONNECTIONS.with(|connections| {
        connections.borrow_mut().insert(handle, connection);
    });
    handle
}

fn store_transaction(transaction: SqlTransaction) -> i64 {
    let handle = next_handle();
    SQL_TRANSACTIONS.with(|transactions| {
        transactions.borrow_mut().insert(handle, transaction);
    });
    handle
}

fn store_resultset(resultset: SqlResultSet) -> i64 {
    let handle = next_handle();
    SQL_RESULTSETS.with(|resultsets| {
        resultsets.borrow_mut().insert(handle, resultset);
    });
    handle
}

fn with_connection<R, F>(handle: i64, op: F) -> Result<R, String>
where
    F: FnOnce(&mut SqlConnection) -> Result<R, String>,
{
    SQL_CONNECTIONS.with(|connections| {
        let mut map = connections.borrow_mut();
        let connection = map
            .get_mut(&handle)
            .ok_or_else(|| "invalid sql connection handle".to_string())?;
        op(connection)
    })
}

fn with_transaction<R, F>(handle: i64, op: F) -> Result<R, String>
where
    F: FnOnce(&mut SqlTransaction) -> Result<R, String>,
{
    SQL_TRANSACTIONS.with(|transactions| {
        let mut map = transactions.borrow_mut();
        let transaction = map
            .get_mut(&handle)
            .ok_or_else(|| "invalid sql transaction handle".to_string())?;
        op(transaction)
    })
}

fn with_resultset<R, F>(handle: i64, op: F) -> Result<R, String>
where
    F: FnOnce(&mut SqlResultSet) -> Result<R, String>,
{
    SQL_RESULTSETS.with(|resultsets| {
        let mut map = resultsets.borrow_mut();
        let resultset = map
            .get_mut(&handle)
            .ok_or_else(|| "invalid sql result set handle".to_string())?;
        op(resultset)
    })
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

fn value_list_to_sql_params(value: *mut Value) -> Result<Vec<SqliteValue>, String> {
    if value.is_null() {
        return Ok(Vec::new());
    }
    let val = unsafe { &*value };
    let Value::List(values) = val else {
        return Err("sql params must be a list".to_string());
    };
    values
        .iter()
        .map(mux_value_to_sql_param)
        .collect::<Result<Vec<_>, _>>()
}

fn mux_value_to_sql_param(value: &Value) -> Result<SqliteValue, String> {
    match value {
        Value::Unit => Ok(SqliteValue::Null),
        Value::Bool(b) => Ok(SqliteValue::Integer(if *b { 1 } else { 0 })),
        Value::Int(i) => Ok(SqliteValue::Integer(*i)),
        Value::Float(f) => Ok(SqliteValue::Real(f.into_inner())),
        Value::String(s) => Ok(SqliteValue::Text(s.clone())),
        Value::List(items) => {
            let mut bytes = Vec::with_capacity(items.len());
            for item in items {
                let Value::Int(byte) = item else {
                    return Err("blob parameter must be list<int>".to_string());
                };
                if *byte < 0 || *byte > 255 {
                    return Err("blob byte out of range".to_string());
                }
                bytes.push(*byte as u8);
            }
            Ok(SqliteValue::Blob(bytes))
        }
        Value::Optional(opt) => match opt {
            None => Ok(SqliteValue::Null),
            Some(inner) => mux_value_to_sql_param(inner),
        },
        _ => Err("unsupported sql parameter type".to_string()),
    }
}

fn sql_value_from_ref(value: ValueRef<'_>) -> Value {
    match value {
        ValueRef::Null => Value::Unit,
        ValueRef::Integer(i) => Value::Int(i),
        ValueRef::Real(f) => Value::Float(ordered_float::OrderedFloat(f)),
        ValueRef::Text(text) => Value::String(String::from_utf8_lossy(text).into_owned()),
        ValueRef::Blob(blob) => {
            let bytes = blob.iter().map(|b| Value::Int(i64::from(*b))).collect();
            Value::List(bytes)
        }
    }
}

fn sql_value_to_int(value: &Value) -> Result<i64, String> {
    match value {
        Value::Int(i) => Ok(*i),
        Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
        Value::Float(f) => {
            let raw = f.into_inner();
            if !raw.is_finite() {
                return Err("cannot convert non-finite float to int".to_string());
            }
            if raw.fract() != 0.0 {
                return Err("cannot convert non-integer float to int".to_string());
            }
            Ok(raw as i64)
        }
        Value::String(s) => s
            .parse::<i64>()
            .map_err(|_| "cannot parse sql value as int".to_string()),
        Value::Unit => Err("cannot convert null sql value to int".to_string()),
        _ => Err("cannot convert sql value to int".to_string()),
    }
}

fn sql_value_to_bool(value: &Value) -> Result<bool, String> {
    match value {
        Value::Bool(b) => Ok(*b),
        Value::Int(i) => Ok(*i != 0),
        Value::Float(f) => Ok(f.into_inner() != 0.0),
        Value::String(s) => {
            let normalized = s.trim().to_ascii_lowercase();
            match normalized.as_str() {
                "true" | "t" | "1" | "yes" | "y" => Ok(true),
                "false" | "f" | "0" | "no" | "n" => Ok(false),
                _ => Err("cannot parse sql value as bool".to_string()),
            }
        }
        Value::Unit => Err("cannot convert null sql value to bool".to_string()),
        _ => Err("cannot convert sql value to bool".to_string()),
    }
}

fn sql_value_to_float(value: &Value) -> Result<f64, String> {
    match value {
        Value::Float(f) => Ok(f.into_inner()),
        Value::Int(i) => Ok(*i as f64),
        Value::String(s) => s
            .parse::<f64>()
            .map_err(|_| "cannot parse sql value as float".to_string()),
        Value::Bool(b) => Ok(if *b { 1.0 } else { 0.0 }),
        Value::Unit => Err("cannot convert null sql value to float".to_string()),
        _ => Err("cannot convert sql value to float".to_string()),
    }
}

fn sql_value_to_bytes(value: &Value) -> Result<Vec<i64>, String> {
    match value {
        Value::List(items) => {
            let mut bytes = Vec::with_capacity(items.len());
            for item in items {
                let Value::Int(byte) = item else {
                    return Err("sql bytes must contain ints".to_string());
                };
                if *byte < 0 || *byte > 255 {
                    return Err("sql byte out of range".to_string());
                }
                bytes.push(*byte);
            }
            Ok(bytes)
        }
        Value::Unit => Err("cannot convert null sql value to bytes".to_string()),
        _ => Err("sql value is not bytes".to_string()),
    }
}

fn sql_value_to_strict_string(value: &Value) -> Result<String, String> {
    match value {
        Value::String(s) => Ok(s.clone()),
        Value::Unit => Err("cannot convert null sql value to string".to_string()),
        _ => Err("sql value is not a string".to_string()),
    }
}

fn route_connect(uri: &str) -> Result<SqlConnection, String> {
    if uri == "sqlite::memory:" || uri == "sqlite://:memory:" {
        return SqliteConnection::open_in_memory()
            .map(SqlConnection::Sqlite)
            .map_err(|e| format!("sqlite connect failed: {}", e));
    }

    if let Some(path) = uri.strip_prefix("sqlite://") {
        return SqliteConnection::open(path)
            .map(SqlConnection::Sqlite)
            .map_err(|e| format!("sqlite connect failed: {}", e));
    }

    if uri.starts_with("postgres://") || uri.starts_with("postgresql://") {
        return Err("unsupported sql provider: postgres".to_string());
    }
    if uri.starts_with("mysql://") || uri.starts_with("mariadb://") {
        return Err("unsupported sql provider: mysql/mariadb".to_string());
    }
    if uri.starts_with("sqlserver://") || uri.starts_with("mssql://") {
        return Err("unsupported sql provider: sqlserver".to_string());
    }

    Err(format!("unsupported or unrecognised sql uri scheme: {}", uri))
}

fn sqlite_execute(
    connection: &mut SqliteConnection,
    sql: &str,
    params: &[SqliteValue],
) -> Result<i64, String> {
    let affected = connection
        .execute(sql, params_from_iter(params.iter()))
        .map_err(|e| format!("sql execute failed: {}", e))?;
    i64::try_from(affected).map_err(|_| "affected row count overflowed int".to_string())
}

#[allow(clippy::mutable_key_type)]
fn sqlite_query(
    connection: &mut SqliteConnection,
    sql: &str,
    params: &[SqliteValue],
) -> Result<SqlResultSet, String> {
    let mut statement = connection
        .prepare(sql)
        .map_err(|e| format!("sql query prepare failed: {}", e))?;
    let columns: Vec<String> = statement
        .column_names()
        .iter()
        .map(|name| (*name).to_string())
        .collect();
    let column_count = columns.len();
    let mut rows = statement
        .query(params_from_iter(params.iter()))
        .map_err(|e| format!("sql query failed: {}", e))?;

    let mut out_rows = Vec::new();
    while let Some(row) = rows
        .next()
        .map_err(|e| format!("sql query failed: {}", e))?
    {
        let mut map = BTreeMap::new();
        for idx in 0..column_count {
            let col_name = columns
                .get(idx)
                .cloned()
                .unwrap_or_else(|| format!("column_{}", idx));
            let value_ref = row
                .get_ref(idx)
                .map_err(|e| format!("sql row read failed: {}", e))?;
            map.insert(Value::String(col_name), sql_value_from_ref(value_ref));
        }
        out_rows.push(Value::Map(map));
    }

    Ok(SqlResultSet {
        rows: out_rows,
        columns,
        next_index: 0,
    })
}

fn connection_execute_with_params(
    handle: i64,
    sql: &str,
    params: &[SqliteValue],
) -> Result<i64, String> {
    with_connection(handle, |connection| match connection {
        SqlConnection::Sqlite(conn) => sqlite_execute(conn, sql, params),
    })
}

fn connection_query_with_params(
    handle: i64,
    sql: &str,
    params: &[SqliteValue],
) -> Result<Value, String> {
    let resultset = with_connection(handle, |connection| match connection {
        SqlConnection::Sqlite(conn) => sqlite_query(conn, sql, params),
    })?;
    let rs_handle = store_resultset(resultset);
    Ok(create_handle_value(rs_handle, *SQL_RESULTSET_TYPE_ID))
}

fn transaction_execute_with_params(
    tx_handle: i64,
    sql: &str,
    params: &[SqliteValue],
) -> Result<i64, String> {
    with_transaction(tx_handle, |tx| {
        if !tx.active {
            return Err("transaction is no longer active".to_string());
        }
        connection_execute_with_params(tx.connection_handle, sql, params)
    })
}

fn transaction_query_with_params(
    tx_handle: i64,
    sql: &str,
    params: &[SqliteValue],
) -> Result<Value, String> {
    with_transaction(tx_handle, |tx| {
        if !tx.active {
            return Err("transaction is no longer active".to_string());
        }
        connection_query_with_params(tx.connection_handle, sql, params)
    })
}

#[unsafe(no_mangle)]
/// # Safety
/// The `uri` pointer must point to a valid, null-terminated C string for the
/// duration of this call.
pub unsafe extern "C" fn mux_sql_connect(uri: *const c_char) -> *mut MuxResult {
    if uri.is_null() {
        return sql_result_err("sql uri pointer is null".to_string());
    }
    let uri_text = unsafe { CStr::from_ptr(uri) }
        .to_string_lossy()
        .into_owned();
    match route_connect(&uri_text) {
        Ok(connection) => {
            let handle = store_connection(connection);
            sql_result_ok(create_handle_value(handle, *SQL_CONNECTION_TYPE_ID))
        }
        Err(err) => sql_result_err(err),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_int(value: i64) -> *mut Value {
    crate::refcount::mux_rc_alloc(Value::Int(value))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_float(value: f64) -> *mut Value {
    crate::refcount::mux_rc_alloc(Value::Float(ordered_float::OrderedFloat(value)))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_bool(value: bool) -> *mut Value {
    crate::refcount::mux_rc_alloc(Value::Bool(value))
}

/// # Safety
/// The `value` pointer must point to a valid, null-terminated C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_sql_value_string(value: *const c_char) -> *mut Value {
    if value.is_null() {
        return crate::refcount::mux_rc_alloc(Value::String(String::new()));
    }
    let raw = unsafe { CStr::from_ptr(value) }
        .to_string_lossy()
        .into_owned();
    crate::refcount::mux_rc_alloc(Value::String(raw))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_bytes(value: *const Value) -> *mut MuxResult {
    if value.is_null() {
        return sql_result_err("sql value bytes pointer is null".to_string());
    }
    let list = unsafe { &*value };
    match list {
        Value::List(items) => {
            let mut bytes = Vec::with_capacity(items.len());
            for item in items {
                let Value::Int(byte) = item else {
                    return sql_result_err("blob parameter must be list<int>".to_string());
                };
                if *byte < 0 || *byte > 255 {
                    return sql_result_err("blob byte out of range".to_string());
                }
                bytes.push(Value::Int(*byte));
            }
            sql_result_ok(Value::List(bytes))
        }
        _ => sql_result_err("sql.bytes expects a list<int>".to_string()),
    }
}
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_null() -> *mut Value {
    crate::refcount::mux_rc_alloc(Value::Unit)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_is_null(value: *const Value) -> bool {
    if value.is_null() {
        return true;
    }
    matches!(unsafe { &*value }, Value::Unit)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_as_int(value: *const Value) -> *mut MuxResult {
    if value.is_null() {
        return sql_result_err("sql value pointer is null".to_string());
    }
    match sql_value_to_int(unsafe { &*value }) {
        Ok(v) => sql_result_ok(Value::Int(v)),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_as_bool(value: *const Value) -> *mut MuxResult {
    if value.is_null() {
        return sql_result_err("sql value pointer is null".to_string());
    }
    match sql_value_to_bool(unsafe { &*value }) {
        Ok(v) => sql_result_ok(Value::Bool(v)),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_as_float(value: *const Value) -> *mut MuxResult {
    if value.is_null() {
        return sql_result_err("sql value pointer is null".to_string());
    }
    match sql_value_to_float(unsafe { &*value }) {
        Ok(v) => sql_result_ok(Value::Float(ordered_float::OrderedFloat(v))),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_as_string(value: *const Value) -> *mut MuxResult {
    if value.is_null() {
        return sql_result_err("sql value pointer is null".to_string());
    }
    match sql_value_to_strict_string(unsafe { &*value }) {
        Ok(v) => sql_result_ok(Value::String(v)),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_as_bytes(value: *const Value) -> *mut MuxResult {
    if value.is_null() {
        return sql_result_err("sql value pointer is null".to_string());
    }
    match sql_value_to_bytes(unsafe { &*value }) {
        Ok(v) => {
            let values = v.into_iter().map(Value::Int).collect::<Vec<_>>();
            sql_result_ok(Value::List(values))
        }
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_connection_close(connection: *mut Value) {
    if let Ok(handle) = connection_handle(connection) {
        remove_connection(handle);
        write_handle(connection, 0);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_connection_execute(
    connection: *mut Value,
    sql: *mut Value,
) -> *mut MuxResult {
    let result = connection_handle(connection).and_then(|handle| {
        value_to_string(sql)
            .and_then(|statement| connection_execute_with_params(handle, &statement, &[]))
    });
    sql_result_i64(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_connection_execute_params(
    connection: *mut Value,
    sql: *mut Value,
    params: *mut Value,
) -> *mut MuxResult {
    let result = connection_handle(connection).and_then(|handle| {
        let statement = value_to_string(sql)?;
        let sql_params = value_list_to_sql_params(params)?;
        connection_execute_with_params(handle, &statement, &sql_params)
    });
    sql_result_i64(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_connection_query(
    connection: *mut Value,
    sql: *mut Value,
) -> *mut MuxResult {
    let result = connection_handle(connection).and_then(|handle| {
        value_to_string(sql)
            .and_then(|statement| connection_query_with_params(handle, &statement, &[]))
    });
    match result {
        Ok(value) => sql_result_ok(value),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_connection_query_params(
    connection: *mut Value,
    sql: *mut Value,
    params: *mut Value,
) -> *mut MuxResult {
    let result = connection_handle(connection).and_then(|handle| {
        let statement = value_to_string(sql)?;
        let sql_params = value_list_to_sql_params(params)?;
        connection_query_with_params(handle, &statement, &sql_params)
    });
    match result {
        Ok(value) => sql_result_ok(value),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_connection_begin_transaction(connection: *mut Value) -> *mut MuxResult {
    let result = connection_handle(connection).and_then(|handle| {
        with_connection(handle, |conn| match conn {
            SqlConnection::Sqlite(sqlite) => sqlite
                .execute_batch("BEGIN TRANSACTION")
                .map_err(|e| format!("begin transaction failed: {}", e)),
        })?;
        let tx_handle = store_transaction(SqlTransaction {
            connection_handle: handle,
            active: true,
        });
        Ok(create_handle_value(tx_handle, *SQL_TRANSACTION_TYPE_ID))
    });

    match result {
        Ok(value) => sql_result_ok(value),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_transaction_commit(transaction: *mut Value) -> *mut MuxResult {
    let result = transaction_handle(transaction).and_then(|handle| {
        with_transaction(handle, |tx| {
            if !tx.active {
                return Err("transaction is no longer active".to_string());
            }
            with_connection(tx.connection_handle, |conn| match conn {
                SqlConnection::Sqlite(sqlite) => sqlite
                    .execute_batch("COMMIT")
                    .map_err(|e| format!("commit failed: {}", e)),
            })?;
            tx.active = false;
            Ok(())
        })
    });
    sql_result_unit(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_transaction_rollback(transaction: *mut Value) -> *mut MuxResult {
    let result = transaction_handle(transaction).and_then(|handle| {
        with_transaction(handle, |tx| {
            if !tx.active {
                return Err("transaction is no longer active".to_string());
            }
            with_connection(tx.connection_handle, |conn| match conn {
                SqlConnection::Sqlite(sqlite) => sqlite
                    .execute_batch("ROLLBACK")
                    .map_err(|e| format!("rollback failed: {}", e)),
            })?;
            tx.active = false;
            Ok(())
        })
    });
    sql_result_unit(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_transaction_execute(
    transaction: *mut Value,
    sql: *mut Value,
) -> *mut MuxResult {
    let result = transaction_handle(transaction).and_then(|tx_handle| {
        value_to_string(sql)
            .and_then(|statement| transaction_execute_with_params(tx_handle, &statement, &[]))
    });
    sql_result_i64(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_transaction_query(
    transaction: *mut Value,
    sql: *mut Value,
) -> *mut MuxResult {
    let result = transaction_handle(transaction).and_then(|tx_handle| {
        value_to_string(sql)
            .and_then(|statement| transaction_query_with_params(tx_handle, &statement, &[]))
    });
    match result {
        Ok(value) => sql_result_ok(value),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_resultset_rows(resultset: *mut Value) -> *mut MuxResult {
    let result = resultset_handle(resultset)
        .and_then(|handle| with_resultset(handle, |rs| Ok(Value::List(rs.rows.clone()))));
    match result {
        Ok(value) => sql_result_ok(value),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_resultset_next(resultset: *mut Value) -> *mut MuxResult {
    let result = resultset_handle(resultset).and_then(|handle| {
        with_resultset(handle, |rs| {
            let value = if rs.next_index < rs.rows.len() {
                let row = rs.rows[rs.next_index].clone();
                rs.next_index += 1;
                Value::Optional(Some(Box::new(row)))
            } else {
                Value::Optional(None)
            };
            Ok(value)
        })
    });
    match result {
        Ok(value) => sql_result_ok(value),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_resultset_columns(resultset: *mut Value) -> *mut MuxResult {
    let result = resultset_handle(resultset).and_then(|handle| {
        with_resultset(handle, |rs| {
            let columns = rs
                .columns
                .iter()
                .cloned()
                .map(Value::String)
                .collect::<Vec<_>>();
            Ok(Value::List(columns))
        })
    });
    match result {
        Ok(value) => sql_result_ok(value),
        Err(err) => sql_result_err(err),
    }
}
}
