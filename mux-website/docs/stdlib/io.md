# IO Module

The `io` module provides file and path operations that return `Result<T, string>` for explicit error handling.

## Import

```mux
import std.io
```

## File Operations

- `io.read_file(string path) returns Result<string, string>`
- `io.write_file(string path, string content) returns Result<void, string>`
- `io.exists(string path) returns Result<bool, string>`
- `io.remove(string path) returns Result<void, string>`
- `io.mkdir(string path) returns Result<void, string>`
- `io.listdir(string path) returns Result<list<string>, string>`

## Path Operations

- `io.is_file(string path) returns Result<bool, string>`
- `io.is_dir(string path) returns Result<bool, string>`
- `io.join(string left, string right) returns Result<string, string>`
- `io.basename(string path) returns Result<string, string>`
- `io.dirname(string path) returns Result<string, string>`

## Example

```mux title="io_example.mux"
import std.io

func main() returns void {
    match io.write_file("hello.txt", "hello from mux") {
        Ok(_) {}
        Err(err) {
            print("write failed: " + err)
            return
        }
    }

    match io.read_file("hello.txt") {
        Ok(content) {
            print("read: " + content)
        }
        Err(err) {
            print("read failed: " + err)
        }
    }
}
```
