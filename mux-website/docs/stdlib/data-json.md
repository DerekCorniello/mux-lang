---
title: Data JSON
---

# std.data.json — JSON helpers

`std.data.json` (also exported as `std.json`) gives you explicit JSON parsing and conversion routines for working with Mux `Json` values.

- `data.json.parse(string source) -> result<Json, string>` — parses a JSON string.
- `data.json.from_map(map<string, T>) -> result<Json, string>` — converts a string-keyed map into a `Json` object (generic over values).
- `data.json.to_map(Json value) -> result<map<string, Json>, string>` — extracts an object map if the `Json` value is an object.

Example:

```mux
import std.json

func main() returns void {
    match json.parse("{\"user\": \"mux\"}") {
        ok(j) {
            match json.to_map(j) {
                ok(map) { print("user=" + map["user"].stringify(none).unwrap()) }
                err(e) { print("not object: " + e) }
            }
        }
        err(e) { print("parse failed: " + e) }
    }
}
```
