---
title: Sql
---

# std.sql — Database Primitives

`std.sql` provides typed SQL access with connection, transaction, and result set primitives.

Current provider status:

- SQLite: supported (`sqlite::memory:`, `sqlite:///path/to/file.db`)
- PostgreSQL: supported (`postgres://...`, `postgresql://...`)
- MySQL/MariaDB: supported (`mysql://...`, `mariadb://...`)
- SQL Server: URI recognized, currently unsupported

## Connect

| Function | Signature | Return | Description |
| ------ | --------- | ------ | ----------- |
| `sql.connect(uri)` | `string` | `result<Connection, string>` | Opens a database connection. Use `sqlite::memory:` for in-memory SQLite. |

## Connection

| Method | Signature | Return | Description |
| ------ | --------- | ------ | ----------- |
| `conn.close()` | — | `void` | Closes the connection handle. |
| `conn.execute(sql)` | `string` | `result<int, string>` | Executes INSERT/UPDATE/DELETE and returns affected rows. |
| `conn.execute_params(sql, params)` | `string`, `list<SqlValue>` | `result<int, string>` | Parameterized execute. |
| `conn.query(sql)` | `string` | `result<ResultSet, string>` | Executes SELECT and returns a result set cursor. |
| `conn.query_params(sql, params)` | `string`, `list<SqlValue>` | `result<ResultSet, string>` | Parameterized query. |
| `conn.begin_transaction()` | — | `result<Transaction, string>` | Starts a transaction bound to this connection. |

## Transaction

| Method | Signature | Return | Description |
| ------ | --------- | ------ | ----------- |
| `tx.commit()` | — | `result<void, string>` | Commits the active transaction. |
| `tx.rollback()` | — | `result<void, string>` | Rolls back the active transaction. |
| `tx.execute(sql)` | `string` | `result<int, string>` | Executes a statement inside the transaction. |
| `tx.execute_params(sql, params)` | `string`, `list<SqlValue>` | `result<int, string>` | Parameterized execute inside the transaction. |
| `tx.query(sql)` | `string` | `result<ResultSet, string>` | Executes a query inside the transaction. |
| `tx.query_params(sql, params)` | `string`, `list<SqlValue>` | `result<ResultSet, string>` | Parameterized query inside the transaction. |

## ResultSet

| Method | Signature | Return | Description |
| ------ | --------- | ------ | ----------- |
| `rs.rows()` | — | `list<map<string, SqlValue>>` | Materializes all rows. |
| `rs.next()` | — | `optional<map<string, SqlValue>>` | Cursor-style row iteration. |
| `rs.columns()` | — | `list<string>` | Column names in result order. |

## SqlValue

Use `SqlValue` for typed parameters and typed row decoding.

### Constructors

| Function | Signature | Return |
| ------ | --------- | ------ |
| `sql.int(v)` | `int` | `SqlValue` |
| `sql.float(v)` | `float` | `SqlValue` |
| `sql.bool(v)` | `bool` | `SqlValue` |
| `sql.string(v)` | `string` | `SqlValue` |
| `sql.bytes(v)` | `list<int>` | `SqlValue` |
| `sql.null()` | — | `SqlValue` |

### Methods

| Method | Return | Description |
| ------ | ------ | ----------- |
| `value.is_null()` | `bool` | True when value is SQL NULL. |
| `value.as_bool()` | `result<bool, string>` | Converts SQL value to bool. |
| `value.as_int()` | `result<int, string>` | Converts SQL value to int. |
| `value.as_float()` | `result<float, string>` | Converts SQL value to float. |
| `value.as_string()` | `result<string, string>` | Converts SQL value to string. |
| `value.as_bytes()` | `result<list<int>, string>` | Converts SQL value to bytes. |
| `value.to_string()` | `string` | Generic string representation. |

## SQLite Example

```mux
import std.assert
import std.sql

func main() returns void {
    match sql.connect("sqlite::memory:") {
        ok(conn) {
            match conn.execute("CREATE TABLE users (id INTEGER, name TEXT)") {
                ok(_) {}
                err(e) { assert.assert(false, e) }
            }

            match conn.execute_params(
                "INSERT INTO users (id, name) VALUES (?, ?)",
                [sql.int(1), sql.string("Ada")]
            ) {
                ok(count) { assert.assert(count == 1, "insert mismatch") }
                err(e) { assert.assert(false, e) }
            }

            match conn.query("SELECT id, name FROM users") {
                ok(rs) {
                    auto rows = rs.rows()
                    auto row = rows[0]
                    match row["id"].as_int() {
                        ok(id) { assert.assert(id == 1, "id mismatch") }
                        err(e) { assert.assert(false, e) }
                    }
                    match row["name"].as_string() {
                        ok(name) { assert.assert(name == "Ada", "name mismatch") }
                        err(e) { assert.assert(false, e) }
                    }
                }
                err(e) { assert.assert(false, e) }
            }
        }
        err(e) { assert.assert(false, e) }
    }
}
```
