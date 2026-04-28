use crate::object::{alloc_object, get_object_ptr, register_object_type};
use crate::refcount::{mux_rc_alloc, mux_rc_dec};
use crate::{TypeId, Value};
use lazy_static::lazy_static;
use mux_profiling::runtime_scope;
use mysql::prelude::Queryable;
use mysql::{
    Conn as MySqlConnection, Opts as MySqlOpts, Params as MySqlParams, Value as MySqlValue,
};
use postgres::types::{ToSql, Type as PgType};
use postgres::{Client as PostgresClient, NoTls};
use rusqlite::types::{Value as SqliteValue, ValueRef};
use rusqlite::{params_from_iter, Connection as SqliteConnection};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::ffi::{c_char, c_void, CStr};
use std::sync::atomic::{AtomicI64, Ordering};

// THREAD AFFINITY INVARIANT:
// Handle IDs (Connection, Transaction, ResultSet) are globally unique via NEXT_HANDLE,
// but the backing stores are thread_local. A handle is only valid on the thread that
// created it. If a handle is passed to another thread (via sync blocks, closures, or
// spawned tasks), operations will silently fail with "invalid sql ... handle".
// This is a fundamental constraint of the current design and not enforced by the type
// system. Users must ensure handles remain on their creating thread.

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
    Postgres(PostgresClient),
    MySql(MySqlConnection),
}

struct SqlTransaction {
    connection_handle: i64,
    connection: Option<SqlConnection>,
    active: bool,
}

struct SqlResultSet {
    rows: Vec<Value>,
    columns: Vec<String>,
    next_index: usize,
}

#[derive(Clone)]
enum SqlParam {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
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

fn sql_result_ok(value: Value) -> *mut Value {
    mux_rc_alloc(Value::Result(Ok(Box::new(value))))
}

fn sql_result_err(msg: String) -> *mut Value {
    mux_rc_alloc(Value::Result(Err(Box::new(Value::String(msg)))))
}

fn sql_result_unit(result: Result<(), String>) -> *mut Value {
    match result {
        Ok(()) => sql_result_ok(Value::Unit),
        Err(err) => sql_result_err(err),
    }
}

fn sql_result_i64(result: Result<i64, String>) -> *mut Value {
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

fn take_connection(handle: i64) -> Result<SqlConnection, String> {
    SQL_CONNECTIONS.with(|connections| {
        connections
            .borrow_mut()
            .remove(&handle)
            .ok_or_else(|| "connection handle not found".to_string())
    })
}

fn connection_has_active_transaction(handle: i64) -> bool {
    SQL_TRANSACTIONS.with(|transactions| {
        transactions
            .borrow()
            .values()
            .any(|tx| tx.active && tx.connection_handle == handle)
    })
}

fn return_connection(handle: i64, connection: SqlConnection) {
    SQL_CONNECTIONS.with(|connections| {
        connections.borrow_mut().insert(handle, connection);
    });
}

fn remove_transaction(handle: i64) {
    SQL_TRANSACTIONS.with(|transactions| {
        transactions.borrow_mut().remove(&handle);
    });
}

fn remove_transaction_for_connection(conn_handle: i64) {
    SQL_TRANSACTIONS.with(|transactions| {
        let mut map = transactions.borrow_mut();
        map.retain(|_, tx| {
            if tx.connection_handle == conn_handle && tx.active {
                if let Some(mut connection) = tx.connection.take() {
                    let _ = rollback_connection(&mut connection);
                }
            }
            tx.connection_handle != conn_handle
        });
    });
}

fn take_transaction(handle: i64) -> Option<SqlTransaction> {
    SQL_TRANSACTIONS.with(|transactions| transactions.borrow_mut().remove(&handle))
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

fn connection_still_alive(handle: i64) -> bool {
    SQL_CONNECTIONS.with(|connections| connections.borrow().contains_key(&handle))
}

fn drop_transaction_handle(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }

    let handle = unsafe { *(ptr as *mut i64) };
    if handle == 0 {
        return;
    }

    let Some(mut tx) = take_transaction(handle) else {
        return;
    };

    if !tx.active {
        return;
    }

    let Some(mut connection) = tx.connection.take() else {
        return;
    };

    let _ = rollback_connection(&mut connection);

    if connection_still_alive(tx.connection_handle) {
        return_connection(tx.connection_handle, connection);
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

fn value_list_to_sql_params(value: *mut Value) -> Result<Vec<SqlParam>, String> {
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

fn mux_value_to_sql_param(value: &Value) -> Result<SqlParam, String> {
    match value {
        Value::Unit => Ok(SqlParam::Null),
        Value::Bool(b) => Ok(SqlParam::Bool(*b)),
        Value::Int(i) => Ok(SqlParam::Int(*i)),
        Value::Float(f) => Ok(SqlParam::Float(f.into_inner())),
        Value::String(s) => Ok(SqlParam::String(s.clone())),
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
            Ok(SqlParam::Bytes(bytes))
        }
        Value::Optional(opt) => match opt {
            None => Ok(SqlParam::Null),
            Some(inner) => mux_value_to_sql_param(inner),
        },
        _ => Err("unsupported sql parameter type".to_string()),
    }
}

fn sql_param_to_sqlite(param: &SqlParam) -> SqliteValue {
    match param {
        SqlParam::Null => SqliteValue::Null,
        SqlParam::Bool(b) => SqliteValue::Integer(if *b { 1 } else { 0 }),
        SqlParam::Int(i) => SqliteValue::Integer(*i),
        SqlParam::Float(f) => SqliteValue::Real(*f),
        SqlParam::String(s) => SqliteValue::Text(s.clone()),
        SqlParam::Bytes(b) => SqliteValue::Blob(b.clone()),
    }
}

fn sql_param_to_mysql(param: &SqlParam) -> MySqlValue {
    match param {
        SqlParam::Null => MySqlValue::NULL,
        SqlParam::Bool(b) => MySqlValue::Int(if *b { 1 } else { 0 }),
        SqlParam::Int(i) => MySqlValue::Int(*i),
        SqlParam::Float(f) => MySqlValue::Double(*f),
        SqlParam::String(s) => MySqlValue::Bytes(s.as_bytes().to_vec()),
        SqlParam::Bytes(b) => MySqlValue::Bytes(b.clone()),
    }
}

fn sql_param_to_postgres(param: &SqlParam) -> Box<dyn ToSql + Sync> {
    match param {
        SqlParam::Null => Box::new(Option::<String>::None),
        SqlParam::Bool(b) => Box::new(*b),
        SqlParam::Int(i) => Box::new(*i),
        SqlParam::Float(f) => Box::new(*f),
        SqlParam::String(s) => Box::new(s.clone()),
        SqlParam::Bytes(b) => Box::new(b.clone()),
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
        return PostgresClient::connect(uri, NoTls)
            .map(SqlConnection::Postgres)
            .map_err(|e| format!("postgres connect failed: {}", e));
    }
    if uri.starts_with("mysql://") || uri.starts_with("mariadb://") {
        let opts =
            MySqlOpts::from_url(uri).map_err(|e| format!("mysql url parse failed: {}", e))?;
        return MySqlConnection::new(opts)
            .map(SqlConnection::MySql)
            .map_err(|e| format!("mysql connect failed: {}", e));
    }
    if uri.starts_with("sqlserver://") || uri.starts_with("mssql://") {
        return Err("unsupported sql provider: sqlserver".to_string());
    }

    Err(format!(
        "unsupported or unrecognised sql uri scheme: {}",
        uri
    ))
}

fn sqlite_execute(
    connection: &mut SqliteConnection,
    sql: &str,
    params: &[SqlParam],
) -> Result<i64, String> {
    let sqlite_params: Vec<SqliteValue> = params.iter().map(sql_param_to_sqlite).collect();
    let affected = connection
        .execute(sql, params_from_iter(sqlite_params.iter()))
        .map_err(|e| format!("sql execute failed: {}", e))?;
    i64::try_from(affected).map_err(|_| "affected row count overflowed int".to_string())
}

#[allow(clippy::mutable_key_type)]
fn sqlite_query(
    connection: &mut SqliteConnection,
    sql: &str,
    params: &[SqlParam],
) -> Result<SqlResultSet, String> {
    let sqlite_params: Vec<SqliteValue> = params.iter().map(sql_param_to_sqlite).collect();
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
        .query(params_from_iter(sqlite_params.iter()))
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

fn postgres_query_value(
    row: &postgres::Row,
    idx: usize,
    pg_type: &PgType,
) -> Result<Value, String> {
    macro_rules! get_col {
        ($row:expr, $idx:expr, $rust_type:ty, $map:expr) => {{
            let v: Option<$rust_type> = $row
                .try_get($idx)
                .map_err(|e| format!("postgres row read failed: {}", e))?;
            Ok(v.map($map).unwrap_or(Value::Unit))
        }};
    }

    match *pg_type {
        PgType::BOOL => get_col!(row, idx, bool, Value::Bool),
        PgType::INT2 => get_col!(row, idx, i16, |v| Value::Int(i64::from(v))),
        PgType::INT4 => get_col!(row, idx, i32, |v| Value::Int(i64::from(v))),
        PgType::INT8 => get_col!(row, idx, i64, Value::Int),
        PgType::FLOAT4 => get_col!(row, idx, f32, |v| Value::Float(
            ordered_float::OrderedFloat(f64::from(v))
        )),
        PgType::FLOAT8 => get_col!(row, idx, f64, |v| Value::Float(
            ordered_float::OrderedFloat(v)
        )),
        PgType::BYTEA => {
            let value: Option<Vec<u8>> = row
                .try_get(idx)
                .map_err(|e| format!("postgres row read failed: {}", e))?;
            Ok(value
                .map(|bytes| {
                    Value::List(
                        bytes
                            .into_iter()
                            .map(|b| Value::Int(i64::from(b)))
                            .collect(),
                    )
                })
                .unwrap_or(Value::Unit))
        }
        _ => get_col!(row, idx, String, Value::String),
    }
}

#[allow(clippy::mutable_key_type)]
fn postgres_query(
    client: &mut PostgresClient,
    sql: &str,
    params: &[SqlParam],
) -> Result<SqlResultSet, String> {
    let stmt = client
        .prepare(sql)
        .map_err(|e| format!("postgres query prepare failed: {}", e))?;
    let param_storage: Vec<Box<dyn ToSql + Sync>> =
        params.iter().map(sql_param_to_postgres).collect();
    let refs: Vec<&(dyn ToSql + Sync)> = param_storage
        .iter()
        .map(|p| p.as_ref() as &(dyn ToSql + Sync))
        .collect();
    let rows = client
        .query(&stmt, &refs)
        .map_err(|e| format!("postgres query failed: {}", e))?;

    let columns: Vec<String> = stmt
        .columns()
        .iter()
        .map(|col| col.name().to_string())
        .collect();
    let mut out_rows = Vec::new();
    for row in rows {
        let mut map = BTreeMap::new();
        for (idx, col) in stmt.columns().iter().enumerate() {
            let value = postgres_query_value(&row, idx, col.type_())?;
            map.insert(Value::String(col.name().to_string()), value);
        }
        out_rows.push(Value::Map(map));
    }

    Ok(SqlResultSet {
        rows: out_rows,
        columns,
        next_index: 0,
    })
}

fn postgres_execute(
    client: &mut PostgresClient,
    sql: &str,
    params: &[SqlParam],
) -> Result<i64, String> {
    let stmt = client
        .prepare(sql)
        .map_err(|e| format!("postgres execute prepare failed: {}", e))?;
    let param_storage: Vec<Box<dyn ToSql + Sync>> =
        params.iter().map(sql_param_to_postgres).collect();
    let refs: Vec<&(dyn ToSql + Sync)> = param_storage
        .iter()
        .map(|p| p.as_ref() as &(dyn ToSql + Sync))
        .collect();
    let affected = client
        .execute(&stmt, &refs)
        .map_err(|e| format!("postgres execute failed: {}", e))?;
    i64::try_from(affected).map_err(|_| "affected row count overflowed int".to_string())
}

fn mysql_value_to_mux(value: MySqlValue) -> Value {
    match value {
        MySqlValue::NULL => Value::Unit,
        MySqlValue::Int(v) => Value::Int(v),
        MySqlValue::UInt(v) => i64::try_from(v)
            .map(Value::Int)
            .unwrap_or(Value::String(v.to_string())),
        MySqlValue::Float(v) => Value::Float(ordered_float::OrderedFloat(f64::from(v))),
        MySqlValue::Double(v) => Value::Float(ordered_float::OrderedFloat(v)),
        MySqlValue::Bytes(bytes) => match String::from_utf8(bytes.clone()) {
            Ok(text) => Value::String(text),
            Err(_) => Value::List(
                bytes
                    .into_iter()
                    .map(|b| Value::Int(i64::from(b)))
                    .collect(),
            ),
        },
        MySqlValue::Date(year, month, day, hour, min, sec, micros) => Value::String(format!(
            "{:04}-{:02}-{:02} {:02}:{:02}:{:02}.{:06}",
            year, month, day, hour, min, sec, micros
        )),
        MySqlValue::Time(is_neg, days, hours, mins, secs, micros) => {
            let sign = if is_neg { "-" } else { "" };
            Value::String(format!(
                "{}{} {:02}:{:02}:{:02}.{:06}",
                sign, days, hours, mins, secs, micros
            ))
        }
    }
}

#[allow(clippy::mutable_key_type)]
fn mysql_query(
    conn: &mut MySqlConnection,
    sql: &str,
    params: &[SqlParam],
) -> Result<SqlResultSet, String> {
    let mysql_params = MySqlParams::Positional(params.iter().map(sql_param_to_mysql).collect());
    let result = conn
        .exec_iter(sql, mysql_params)
        .map_err(|e| format!("mysql query failed: {}", e))?;
    let columns: Vec<String> = result
        .columns()
        .as_ref()
        .iter()
        .map(|col| col.name_str().to_string())
        .collect();

    let mut out_rows = Vec::new();
    for row in result {
        let mut row = row.map_err(|e| format!("mysql row read failed: {}", e))?;
        let values: Vec<MySqlValue> = (0..columns.len())
            .map(|idx| row.take(idx).unwrap_or(MySqlValue::NULL))
            .collect();
        let mut map = BTreeMap::new();
        for (idx, raw) in values.into_iter().enumerate() {
            let col_name = columns
                .get(idx)
                .cloned()
                .unwrap_or_else(|| format!("column_{}", idx));
            map.insert(Value::String(col_name), mysql_value_to_mux(raw));
        }
        out_rows.push(Value::Map(map));
    }

    Ok(SqlResultSet {
        rows: out_rows,
        columns,
        next_index: 0,
    })
}

fn mysql_execute(
    conn: &mut MySqlConnection,
    sql: &str,
    params: &[SqlParam],
) -> Result<i64, String> {
    let mysql_params = MySqlParams::Positional(params.iter().map(sql_param_to_mysql).collect());
    conn.exec_drop(sql, mysql_params)
        .map_err(|e| format!("mysql execute failed: {}", e))?;
    i64::try_from(conn.affected_rows()).map_err(|_| "affected row count overflowed int".to_string())
}

fn begin_transaction_on_connection(connection: &mut SqlConnection) -> Result<(), String> {
    match connection {
        SqlConnection::Sqlite(conn) => conn
            .execute_batch("BEGIN TRANSACTION")
            .map_err(|e| format!("begin transaction failed: {}", e)),
        SqlConnection::Postgres(client) => client
            .batch_execute("BEGIN")
            .map_err(|e| format!("begin transaction failed: {}", e)),
        SqlConnection::MySql(conn) => conn
            .query_drop("START TRANSACTION")
            .map_err(|e| format!("begin transaction failed: {}", e)),
    }
}

fn commit_connection(connection: &mut SqlConnection) -> Result<(), String> {
    match connection {
        SqlConnection::Sqlite(conn) => conn
            .execute_batch("COMMIT")
            .map_err(|e| format!("commit failed: {}", e)),
        SqlConnection::Postgres(client) => client
            .batch_execute("COMMIT")
            .map_err(|e| format!("commit failed: {}", e)),
        SqlConnection::MySql(conn) => conn
            .query_drop("COMMIT")
            .map_err(|e| format!("commit failed: {}", e)),
    }
}

fn rollback_connection(connection: &mut SqlConnection) -> Result<(), String> {
    match connection {
        SqlConnection::Sqlite(conn) => conn
            .execute_batch("ROLLBACK")
            .map_err(|e| format!("rollback failed: {}", e)),
        SqlConnection::Postgres(client) => client
            .batch_execute("ROLLBACK")
            .map_err(|e| format!("rollback failed: {}", e)),
        SqlConnection::MySql(conn) => conn
            .query_drop("ROLLBACK")
            .map_err(|e| format!("rollback failed: {}", e)),
    }
}

fn connection_execute_with_params(
    handle: i64,
    sql: &str,
    params: &[SqlParam],
) -> Result<i64, String> {
    with_connection(handle, |connection| match connection {
        SqlConnection::Sqlite(conn) => sqlite_execute(conn, sql, params),
        SqlConnection::Postgres(client) => postgres_execute(client, sql, params),
        SqlConnection::MySql(conn) => mysql_execute(conn, sql, params),
    })
}

fn connection_query_with_params(
    handle: i64,
    sql: &str,
    params: &[SqlParam],
) -> Result<Value, String> {
    let resultset = with_connection(handle, |connection| match connection {
        SqlConnection::Sqlite(conn) => sqlite_query(conn, sql, params),
        SqlConnection::Postgres(client) => postgres_query(client, sql, params),
        SqlConnection::MySql(conn) => mysql_query(conn, sql, params),
    })?;
    let rs_handle = store_resultset(resultset);
    Ok(create_handle_value(rs_handle, *SQL_RESULTSET_TYPE_ID))
}

fn transaction_execute_with_params(
    tx_handle: i64,
    sql: &str,
    params: &[SqlParam],
) -> Result<i64, String> {
    with_transaction(tx_handle, |tx| {
        if !tx.active {
            return Err("transaction is no longer active".to_string());
        }
        let connection = tx
            .connection
            .as_mut()
            .ok_or_else(|| "transaction connection missing".to_string())?;
        match connection {
            SqlConnection::Sqlite(conn) => sqlite_execute(conn, sql, params),
            SqlConnection::Postgres(client) => postgres_execute(client, sql, params),
            SqlConnection::MySql(conn) => mysql_execute(conn, sql, params),
        }
    })
}

fn transaction_query_with_params(
    tx_handle: i64,
    sql: &str,
    params: &[SqlParam],
) -> Result<Value, String> {
    with_transaction(tx_handle, |tx| {
        if !tx.active {
            return Err("transaction is no longer active".to_string());
        }
        let connection = tx
            .connection
            .as_mut()
            .ok_or_else(|| "transaction connection missing".to_string())?;
        let resultset = match connection {
            SqlConnection::Sqlite(conn) => sqlite_query(conn, sql, params),
            SqlConnection::Postgres(client) => postgres_query(client, sql, params),
            SqlConnection::MySql(conn) => mysql_query(conn, sql, params),
        }?;
        let rs_handle = store_resultset(resultset);
        Ok(create_handle_value(rs_handle, *SQL_RESULTSET_TYPE_ID))
    })
}

#[unsafe(no_mangle)]
/// # Safety
/// The `uri` pointer must point to a valid, null-terminated C string for the
/// duration of this call.
pub unsafe extern "C" fn mux_sql_connect(uri: *const c_char) -> *mut Value {
    let _profile = runtime_scope("runtime: sql connect");
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
    let _profile = runtime_scope("runtime: sql value int");
    crate::refcount::mux_rc_alloc(Value::Int(value))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_float(value: f64) -> *mut Value {
    let _profile = runtime_scope("runtime: sql value float");
    crate::refcount::mux_rc_alloc(Value::Float(ordered_float::OrderedFloat(value)))
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_bool(value: bool) -> *mut Value {
    let _profile = runtime_scope("runtime: sql value bool");
    crate::refcount::mux_rc_alloc(Value::Bool(value))
}

/// # Safety
/// The `value` pointer must point to a valid, null-terminated C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mux_sql_value_string(value: *const c_char) -> *mut Value {
    let _profile = runtime_scope("runtime: sql value string");
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
pub extern "C" fn mux_sql_value_bytes(value: *const Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql value bytes");
    if value.is_null() {
        return crate::refcount::mux_rc_alloc(Value::List(Vec::new()));
    }
    let list = unsafe { &*value };
    match list {
        Value::List(items) => {
            let mut bytes = Vec::with_capacity(items.len());
            for item in items {
                let Value::Int(byte) = item else {
                    return crate::refcount::mux_rc_alloc(Value::List(Vec::new()));
                };
                bytes.push(Value::Int(*byte));
            }
            crate::refcount::mux_rc_alloc(Value::List(bytes))
        }
        _ => crate::refcount::mux_rc_alloc(Value::List(Vec::new())),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_null() -> *mut Value {
    let _profile = runtime_scope("runtime: sql value null");
    crate::refcount::mux_rc_alloc(Value::Unit)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_is_null(value: *const Value) -> bool {
    let _profile = runtime_scope("runtime: sql value is null");
    if value.is_null() {
        return true;
    }
    matches!(unsafe { &*value }, Value::Unit)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_value_as_int(value: *const Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql value as int");
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
pub extern "C" fn mux_sql_value_as_bool(value: *const Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql value as bool");
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
pub extern "C" fn mux_sql_value_as_float(value: *const Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql value as float");
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
pub extern "C" fn mux_sql_value_as_string(value: *const Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql value as string");
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
pub extern "C" fn mux_sql_value_as_bytes(value: *const Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql value as bytes");
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

#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_connection_close(connection: *mut Value) {
    let _profile = runtime_scope("runtime: sql connection close");
    if let Ok(handle) = connection_handle(connection) {
        if connection_has_active_transaction(handle) {
            remove_transaction_for_connection(handle);
        }
        remove_connection(handle);
        write_handle(connection, 0);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_connection_execute(
    connection: *mut Value,
    sql: *mut Value,
) -> *mut Value {
    let _profile = runtime_scope("runtime: sql connection execute");
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
) -> *mut Value {
    let _profile = runtime_scope("runtime: sql connection execute params");
    let result = connection_handle(connection).and_then(|handle| {
        let statement = value_to_string(sql)?;
        let sql_params = value_list_to_sql_params(params)?;
        connection_execute_with_params(handle, &statement, &sql_params)
    });
    sql_result_i64(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_connection_query(connection: *mut Value, sql: *mut Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql connection query");
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
) -> *mut Value {
    let _profile = runtime_scope("runtime: sql connection query params");
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
pub extern "C" fn mux_sql_connection_begin_transaction(connection: *mut Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql connection begin transaction");
    let handle = match connection_handle(connection) {
        Ok(h) => h,
        Err(err) => return sql_result_err(err),
    };
    if connection_has_active_transaction(handle) {
        return sql_result_err("connection already has an active transaction".to_string());
    }
    let conn = match take_connection(handle) {
        Ok(conn) => conn,
        Err(_) => return sql_result_err("invalid sql connection handle".to_string()),
    };
    let mut conn = conn;
    if let Err(err) = begin_transaction_on_connection(&mut conn) {
        return_connection(handle, conn);
        return sql_result_err(err);
    }
    let tx_handle = store_transaction(SqlTransaction {
        connection_handle: handle,
        connection: Some(conn),
        active: true,
    });
    sql_result_ok(create_handle_value(tx_handle, *SQL_TRANSACTION_TYPE_ID))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_transaction_commit(transaction: *mut Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql transaction commit");
    let handle = match transaction_handle(transaction) {
        Ok(h) => h,
        Err(err) => return sql_result_err(err),
    };
    let result = with_transaction(handle, |tx| {
        if !tx.active {
            return Err("transaction is no longer active".to_string());
        }
        let mut connection = tx
            .connection
            .take()
            .ok_or_else(|| "transaction connection missing".to_string())?;
        commit_connection(&mut connection)?;
        return_connection(tx.connection_handle, connection);
        tx.active = false;
        Ok(())
    });
    if result.is_ok() {
        remove_transaction(handle);
    }
    sql_result_unit(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_transaction_rollback(transaction: *mut Value) -> *mut Value {
    let _profile = runtime_scope("runtime: sql transaction rollback");
    let handle = match transaction_handle(transaction) {
        Ok(h) => h,
        Err(err) => return sql_result_err(err),
    };
    let result = with_transaction(handle, |tx| {
        if !tx.active {
            return Err("transaction is no longer active".to_string());
        }
        let mut connection = tx
            .connection
            .take()
            .ok_or_else(|| "transaction connection missing".to_string())?;
        rollback_connection(&mut connection)?;
        return_connection(tx.connection_handle, connection);
        tx.active = false;
        Ok(())
    });
    if result.is_ok() {
        remove_transaction(handle);
    }
    sql_result_unit(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_transaction_execute(
    transaction: *mut Value,
    sql: *mut Value,
) -> *mut Value {
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
) -> *mut Value {
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
pub extern "C" fn mux_sql_transaction_execute_params(
    transaction: *mut Value,
    sql: *mut Value,
    params: *mut Value,
) -> *mut Value {
    let result = transaction_handle(transaction).and_then(|tx_handle| {
        let statement = value_to_string(sql)?;
        let sql_params = value_list_to_sql_params(params)?;
        transaction_execute_with_params(tx_handle, &statement, &sql_params)
    });
    sql_result_i64(result)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_transaction_query_params(
    transaction: *mut Value,
    sql: *mut Value,
    params: *mut Value,
) -> *mut Value {
    let result = transaction_handle(transaction).and_then(|tx_handle| {
        let statement = value_to_string(sql)?;
        let sql_params = value_list_to_sql_params(params)?;
        transaction_query_with_params(tx_handle, &statement, &sql_params)
    });
    match result {
        Ok(value) => sql_result_ok(value),
        Err(err) => sql_result_err(err),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_resultset_rows(resultset: *mut Value) -> *mut Value {
    let result = resultset_handle(resultset)
        .and_then(|handle| with_resultset(handle, |rs| Ok(Value::List(rs.rows.clone()))));
    match result {
        Ok(value) => crate::refcount::mux_rc_alloc(value),
        Err(_err) => crate::refcount::mux_rc_alloc(Value::List(vec![])),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_resultset_next(resultset: *mut Value) -> *mut Value {
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
        Ok(value) => crate::refcount::mux_rc_alloc(value),
        Err(_err) => crate::refcount::mux_rc_alloc(Value::Optional(None)),
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_sql_resultset_columns(resultset: *mut Value) -> *mut Value {
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
        Ok(value) => crate::refcount::mux_rc_alloc(value),
        Err(_err) => crate::refcount::mux_rc_alloc(Value::List(vec![])),
    }
}
