use crate::Value;
use std::collections::BTreeMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

fn err_result(message: &str) -> *mut Value {
    let msg = CString::new(message).expect("error messages should not contain interior null bytes");
    unsafe { crate::result::mux_result_err_str(msg.as_ptr()) }
}

fn csv_parse_error_result(error: impl std::fmt::Display) -> *mut Value {
    err_result(&format!("CSV parse error: {}", error))
}

fn read_input_string(input: *const c_char) -> Result<String, *mut Value> {
    if input.is_null() {
        return Err(err_result("null input"));
    }

    let s = unsafe { CStr::from_ptr(input) }
        .to_string_lossy()
        .into_owned();
    Ok(s)
}

fn record_to_value_list(record: &csv::StringRecord) -> Value {
    let row: Vec<Value> = record
        .iter()
        .map(|field| Value::String(field.to_string()))
        .collect();
    Value::List(row)
}

fn collect_rows(reader: &mut csv::Reader<&[u8]>) -> Result<Vec<Value>, *mut Value> {
    let mut rows = Vec::new();

    for result in reader.records() {
        match result {
            Ok(record) => rows.push(record_to_value_list(&record)),
            Err(error) => return Err(csv_parse_error_result(error)),
        }
    }

    Ok(rows)
}

fn csv_value(headers: Value, rows: Vec<Value>) -> Value {
    let mut map = BTreeMap::new();
    map.insert(Value::String("headers".to_string()), headers);
    map.insert(Value::String("rows".to_string()), Value::List(rows));
    Value::Map(map)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
#[allow(clippy::mutable_key_type)]
pub extern "C" fn mux_csv_parse(input: *const c_char) -> *mut Value {
    let s = match read_input_string(input) {
        Ok(input) => input,
        Err(error) => return error,
    };

    let mut reader = csv::ReaderBuilder::new()
        .has_headers(false)
        .from_reader(s.as_bytes());
    let rows = match collect_rows(&mut reader) {
        Ok(rows) => rows,
        Err(error) => return error,
    };

    let csv_value = csv_value(Value::List(Vec::new()), rows);

    let v_ptr = crate::refcount::mux_rc_alloc(csv_value);
    crate::result::mux_result_ok_value(v_ptr)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
#[allow(clippy::mutable_key_type)]
pub extern "C" fn mux_csv_parse_with_headers(input: *const c_char) -> *mut Value {
    let s = match read_input_string(input) {
        Ok(input) => input,
        Err(error) => return error,
    };

    let mut reader = csv::Reader::from_reader(s.as_bytes());

    let headers = match reader.headers() {
        Ok(hdr) => {
            let header_values: Vec<Value> = hdr
                .iter()
                .map(|field| Value::String(field.to_string()))
                .collect();
            Value::List(header_values)
        }
        Err(error) => return csv_parse_error_result(error),
    };

    let rows = match collect_rows(&mut reader) {
        Ok(rows) => rows,
        Err(error) => return error,
    };

    let csv_value = csv_value(headers, rows);

    let v_ptr = crate::refcount::mux_rc_alloc(csv_value);
    crate::result::mux_result_ok_value(v_ptr)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[unsafe(no_mangle)]
pub extern "C" fn mux_csv_to_string(val: *const Value) -> *mut Value {
    if val.is_null() {
        let msg = CString::new("null input").unwrap();
        unsafe {
            return crate::result::mux_result_err_str(msg.as_ptr());
        }
    }

    let v = unsafe { &*val };

    match validate_and_extract_csv(v) {
        Ok((headers, rows)) => {
            let csv_string = build_csv_string(&headers, &rows, true);
            let result_value = crate::refcount::mux_rc_alloc(Value::String(csv_string));
            crate::result::mux_result_ok_value(result_value)
        }
        Err(e) => {
            let msg = CString::new(e).unwrap();
            unsafe { crate::result::mux_result_err_str(msg.as_ptr()) }
        }
    }
}

fn validate_and_extract_csv(val: &Value) -> Result<(Vec<String>, Vec<Vec<String>>), String> {
    match val {
        Value::Map(map) => {
            let headers_val = map
                .get(&Value::String("headers".to_string()))
                .ok_or("missing 'headers' key")?;

            let rows_val = map
                .get(&Value::String("rows".to_string()))
                .ok_or("missing 'rows' key")?;

            let headers = extract_string_list(headers_val)?;
            let rows = extract_row_list(rows_val)?;

            Ok((headers, rows))
        }
        _ => Err("value is not a map".to_string()),
    }
}

fn extract_string_list(val: &Value) -> Result<Vec<String>, String> {
    match val {
        Value::List(list) => {
            let mut result = Vec::new();
            for item in list {
                match item {
                    Value::String(s) => result.push(s.clone()),
                    _ => return Err("headers contain non-string value".to_string()),
                }
            }
            Ok(result)
        }
        _ => Err("headers is not a list".to_string()),
    }
}

fn extract_row_list(val: &Value) -> Result<Vec<Vec<String>>, String> {
    match val {
        Value::List(rows) => {
            let mut result = Vec::new();
            for row_val in rows {
                match row_val {
                    Value::List(row) => {
                        let mut row_strings = Vec::new();
                        for field in row {
                            match field {
                                Value::String(s) => row_strings.push(s.clone()),
                                _ => return Err("row contains non-string field".to_string()),
                            }
                        }
                        result.push(row_strings);
                    }
                    _ => return Err("rows contain non-list item".to_string()),
                }
            }
            Ok(result)
        }
        _ => Err("rows is not a list".to_string()),
    }
}

fn build_csv_string(headers: &[String], rows: &[Vec<String>], include_headers: bool) -> String {
    let mut output = Vec::new();
    {
        let mut wtr = csv::Writer::from_writer(&mut output);

        if include_headers && !headers.is_empty() {
            wtr.write_record(headers)
                .expect("in-memory write to Vec should not fail");
        }

        for row in rows {
            wtr.write_record(row)
                .expect("in-memory write to Vec should not fail");
        }

        wtr.flush().expect("in-memory flush to Vec should not fail");
    }
    String::from_utf8(output).unwrap_or_else(|_| "invalid UTF-8 in CSV output".to_string())
}
