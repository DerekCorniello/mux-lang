---
title: Env
---

# std.env — Environment Utilities

`std.env` exposes fallible access to the process environment.

- `env.get(string key) -> optional<string>` — returns the environment variable value or `none` if the key does not exist.

Example:

```mux
import std.env

func main() returns void {
    match env.get("PATH") {
        some(value) {
            print("PATH length: " + value.length().to_string())
        }
        none { print("PATH not set") }
    }
}
```
