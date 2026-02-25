# IO Module

The `io` module provides file and path operations that return `result<T, string>` for explicit error handling.

## Import

```mux
import std.io
```

## File Operations

- `io.read_file(string path) returns result<string, string>`
- `io.write_file(string path, string content) returns result<void, string>`
- `io.exists(string path) returns result<bool, string>`
- `io.remove(string path) returns result<void, string>`
- `io.mkdir(string path) returns result<void, string>`
- `io.listdir(string path) returns result<list<string>, string>`

## Path Operations

- `io.is_file(string path) returns result<bool, string>`
- `io.is_dir(string path) returns result<bool, string>`
- `io.join(string left, string right) returns result<string, string>`
- `io.basename(string path) returns result<string, string>`
- `io.dirname(string path) returns result<string, string>`

## Example

```mux title="io_example.mux"
import std.io

func main() returns void {
    match io.write_file("hello.txt", "hello from mux") {
        ok(_) {}
        err(err) {
            print("write failed: " + err)
            return
        }
    }

    match io.read_file("hello.txt") {
        ok(content) {
            print("read: " + content)
        }
        err(err) {
            print("read failed: " + err)
        }
    }
}
```
